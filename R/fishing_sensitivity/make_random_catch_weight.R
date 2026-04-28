# Load required libraries
library(dplyr)
library(tidyr)

#' Calculate Generalized Catch Time Series Metrics
#'
#' Computes the Center of Mass, T50, T90, Peak Time, Quantiles, Clumpiness of Top Years, 
#' and Rolling Window maximums for every individual species, as well as the total system 
#' aggregate, across all randomized experiments.
#'
#' @param catch_file Character string; the path to the catch forcing text file.
#' @param sim_list List; an R list where each element contains a numeric vector 
#'   representing the timestep order for that specific simulation run.
#' @param species_names Character vector or NULL. If NULL, the function will 
#'   automatically process all columns except "Time".
#' @param start_time Numeric or NULL; the simulation time to begin metric calculations (excludes burn-in). Default is 7665.
#' @param end_time Numeric or NULL; the simulation time to end metric calculations. Default is 20805.
#' @param n_top Integer; number of top catch years to assess for peak location and clumpiness.
#' @param window_size Integer; number of years for the sustained catch rolling window.
#'
#' @return A data frame with metrics for every species and the aggregate catch per run.
#' @export
calculate_all_catch_metrics <- function(catch_file, sim_list, species_names = NULL,
                                        start_time = 7665, end_time = 20805,
                                        n_top = 5, window_size = 10) {
  
  # ----------------------------------------------------------------------------
  # 1. Parse the Header to Extract Column Names
  # ----------------------------------------------------------------------------
  all_lines <- readLines(catch_file, warn = FALSE)
  name_lines <- grep("^## COLUMN[0-9]+\\.name", all_lines, value = TRUE)
  col_names <- trimws(sub("^## COLUMN[0-9]+\\.name\\s+", "", name_lines))
  
  if (length(col_names) == 0) {
    stop("Could not parse any column names from the header.")
  }
  
  # ----------------------------------------------------------------------------
  # 2. Read the Catch Data & Handle Species Names
  # ----------------------------------------------------------------------------
  raw_data <- read.table(catch_file, comment.char = "#", header = FALSE)
  colnames(raw_data) <- col_names[1:ncol(raw_data)]
  
  if (is.null(species_names)) {
    species_names <- setdiff(colnames(raw_data), c("Time", "time"))
    if (length(species_names) == 0) stop("No species columns found.")
  } else {
    missing_spp <- setdiff(species_names, colnames(raw_data))
    if (length(missing_spp) > 0) {
      stop(paste("The following species were missing:", paste(missing_spp, collapse = ", ")))
    }
  }
  
  # Isolate species data and define continuous time sequence
  raw_spp <- raw_data[, species_names, drop = FALSE]
  n_steps <- nrow(raw_spp)
  time_steps <- seq_len(n_steps) 
  
  # --- NEW: Define the Analysis Window (Exclude Burn-in) ---
  time_col_name <- intersect(c("Time", "time"), colnames(raw_data))[1]
  
  if (!is.na(time_col_name)) {
    time_vals <- raw_data[[time_col_name]]
  } else {
    time_vals <- time_steps # Fallback to sequence if no Time column exists
  }
  
  valid_idx <- rep(TRUE, n_steps)
  if (!is.null(start_time)) valid_idx <- valid_idx & (time_vals >= start_time)
  if (!is.null(end_time)) valid_idx <- valid_idx & (time_vals <= end_time)
  
  valid_rows <- which(valid_idx)
  
  if (length(valid_rows) == 0) {
    stop("The specified start_time and end_time excluded all rows from the dataset.")
  }
  
  # Limit the timeline sequence to only those within the analysis window
  analysis_time_steps <- time_steps[valid_rows]
  
  # Safeguards for short time series based on the shortened valid window
  n_top <- min(n_top, length(valid_rows))
  window_size <- min(window_size, length(valid_rows))
  
  # Fast Base-R Rolling Sum Helper Function
  calc_roll_sum <- function(x, n) {
    cx <- c(0, cumsum(x))
    (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)])
  }
  
  # ----------------------------------------------------------------------------
  # 3. Core Math Function for a Single Vector
  # ----------------------------------------------------------------------------
  get_stats <- function(catch_vec, sp_name, exp_label) {
    total_c <- sum(catch_vec, na.rm = TRUE)
    
    # Handle zero-catch scenarios safely
    if (total_c == 0) {
      return(data.frame(
        Experiment = exp_label,
        Species = sp_name,
        Total_Catch = 0,
        Center_Of_Mass = NA_real_,
        T50 = NA_integer_,
        T90 = NA_integer_,
        Peak_Time = NA_integer_,
        Mean_TopN_Time = NA_real_, 
        SD_TopN_Time = NA_real_,
        Max_Window_Start = NA_integer_, 
        Max_Window_Prop = NA_real_
      ))
    }
    
    cum_prop <- cumsum(catch_vec) / total_c
    
    # --- Top N Metrics (Location & Clumpiness) ---
    top_indices <- order(catch_vec, decreasing = TRUE)[1:n_top]
    top_times <- analysis_time_steps[top_indices]
    
    # --- Sustained Window Metrics ---
    rolling_sums <- calc_roll_sum(catch_vec, window_size)
    max_roll_idx <- which.max(rolling_sums) 
    
    data.frame(
      Experiment = exp_label,
      Species = sp_name,
      Total_Catch = total_c,
      Center_Of_Mass = sum(analysis_time_steps * catch_vec, na.rm = TRUE) / total_c,
      T50 = analysis_time_steps[which.max(cum_prop >= 0.50)],
      T90 = analysis_time_steps[which.max(cum_prop >= 0.90)],
      Peak_Time = analysis_time_steps[which.max(catch_vec)],
      Mean_TopN_Time = mean(top_times),               
      SD_TopN_Time = sd(top_times),                   
      Max_Window_Start = analysis_time_steps[max_roll_idx],    
      Max_Window_Prop = rolling_sums[max_roll_idx] / total_c
    )
  }
  
  # ----------------------------------------------------------------------------
  # 4. Helper to Process an Entire Dataframe (Individual + Aggregate)
  # ----------------------------------------------------------------------------
  calc_experiment_metrics <- function(dat, exp_label) {
    
    # NEW: Isolate only the relevant time window before doing any math
    dat_window <- dat[valid_rows, , drop = FALSE]
    
    # 4a. Calculate stats for each individual species
    indiv_list <- lapply(species_names, function(sp) {
      get_stats(dat_window[[sp]], sp, exp_label)
    })
    
    # 4b. Calculate aggregate stats across all species
    agg_vec <- rowSums(dat_window, na.rm = TRUE)
    agg_row <- get_stats(agg_vec, "Aggregate", exp_label)
    
    # Combine individual species and the aggregate into one dataframe
    dplyr::bind_rows(indiv_list, list(agg_row))
  }
  
  # ----------------------------------------------------------------------------
  # 5. Execute Across Original and Randomized Runs
  # ----------------------------------------------------------------------------
  # Process original data
  orig_metrics <- calc_experiment_metrics(raw_spp, "Original")
  
  # Process permutations
  rand_metrics_list <- lapply(seq_along(sim_list), function(i) {
    seq_idx <- sim_list[[i]]
    permuted_dat <- raw_spp[seq_idx, , drop = FALSE]
    calc_experiment_metrics(permuted_dat, as.character(i))
  })
  
  rand_metrics <- dplyr::bind_rows(rand_metrics_list)
  
  # Combine everything
  final_metrics <- dplyr::bind_rows(orig_metrics, rand_metrics)
  
  return(final_metrics)
}

# ==============================================================================
# PIPELINE STEP 2: FORMATTING & COLLINEARITY HANDLING
# ==============================================================================

#' Prepare Wide Predictor Matrix
#' 
#' @param metrics_df The long dataframe output from calculate_all_catch_metrics()
#' @return A wide dataframe where each row is an experiment and columns are predictors.
prep_wide_predictors <- function(metrics_df) {
  
  # Define all the numeric metrics we want to pivot
  metrics_to_pivot <- c("Center_Of_Mass", "T50", "T90", "Peak_Time", 
                        "Mean_TopN_Time", "SD_TopN_Time", 
                        "Max_Window_Start", "Max_Window_Prop")
  
  wide_df <- metrics_df %>%
    # Drop species that have exactly 0 total catch (avoids NAs)
    filter(Total_Catch > 0) %>%
    # Keep only the identifying columns and the core metrics
    select(Experiment, Species, all_of(metrics_to_pivot)) %>%
    # Pivot all selected metrics to wide format
    pivot_wider(
      names_from = Species,
      values_from = all_of(metrics_to_pivot),
      names_glue = "{Species}_{.value}"
    )
  
  # Ensure Experiment is a character for safe merging later
  wide_df$Experiment <- as.character(wide_df$Experiment)
  
  return(wide_df)
}

#' Explore Highest Correlations in Predictor Data
#' 
#' Prints a list of the most highly correlated variable pairs in your dataset
#' so you can manually inspect redundancies.
#'
#' @param wide_predictors Dataframe output from prep_wide_predictors()
#' @param threshold Numeric; absolute correlation value above which to flag pairs (e.g., 0.85)
#' @export
explore_top_correlations <- function(wide_predictors, threshold = 0.85) {
  
  # 1. Isolate only numeric columns (drop the Experiment ID column)
  numeric_data <- wide_predictors %>% select(where(is.numeric))
  
  # 2. Calculate the correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
  # 3. Format into a readable dataframe
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  
  cor_df <- as.data.frame(as.table(cor_matrix)) %>%
    drop_na() %>%
    rename(Variable_1 = Var1, Variable_2 = Var2, Correlation = Freq) %>%
    mutate(Abs_Corr = abs(Correlation)) %>%
    filter(Abs_Corr >= threshold) %>%
    arrange(desc(Abs_Corr))
  
  message(paste("Found", nrow(cor_df), "pairs with a correlation >=", threshold))
  return(cor_df)
}

#' Automatically Remove Redundant Metric Categories Based on Aggregate
#'
#' This function assesses collinearity ONLY among the system-wide 'Aggregate' metrics. 
#' If it finds that two metric categories (e.g., T50 and Center_Of_Mass) are highly 
#' correlated at the aggregate level, it drops the redundant category entirely for ALL species.
#'
#' @param wide_predictors Dataframe output from prep_wide_predictors()
#' @param cutoff Numeric; the correlation threshold to use for dropping (default 0.85)
#' @return A cleaned wide dataframe ready for Random Forest
#' @export
remove_redundant_metric_categories <- function(wide_predictors, cutoff = 0.85) {
  
  # 1. Isolate only the Aggregate columns
  agg_cols <- grep("^Aggregate_", names(wide_predictors), value = TRUE)
  
  if (length(agg_cols) == 0) {
    message("No 'Aggregate_' columns found. No columns dropped.")
    return(wide_predictors)
  }
  
  agg_data <- wide_predictors %>% select(all_of(agg_cols))
  
  # 2. Calculate the correlation matrix for just the Aggregate metrics
  cor_matrix <- cor(agg_data, use = "pairwise.complete.obs")
  
  # 3. Use caret to find which aggregate metrics are redundant
  agg_cols_to_drop <- caret::findCorrelation(cor_matrix, cutoff = cutoff, names = TRUE, exact = TRUE)
  
  if (length(agg_cols_to_drop) == 0) {
    message("No redundant metric categories found at the aggregate level. No columns dropped.")
    return(wide_predictors)
  }
  
  # 4. Identify the base metric names to drop (e.g., "Aggregate_T50" -> "T50")
  base_metrics_to_drop <- sub("^Aggregate_", "", agg_cols_to_drop)
  
  message(paste("Redundant categories identified at the aggregate level:", 
                paste(base_metrics_to_drop, collapse = ", ")))
  
  # 5. Find ALL columns across ALL species that match these dropped categories
  # Using regex: e.g., matching "_T50$" or "_Center_Of_Mass$" at the end of the column name
  pattern <- paste0("_(", paste(base_metrics_to_drop, collapse = "|"), ")$")
  all_cols_to_drop <- grep(pattern, names(wide_predictors), value = TRUE)
  
  message(paste("Dropping", length(all_cols_to_drop), "total columns across all species for these categories..."))
  
  # 6. Filter the dataframe
  clean_wide_predictors <- wide_predictors %>% select(-all_of(all_cols_to_drop))
  
  return(clean_wide_predictors)
}

# ==============================================================================
# EXAMPLE USAGE:
# ==============================================================================
my_simulations <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_permutations.rds')

metric_results <- calculate_all_catch_metrics(
  catch_file = here::here('currentVersion','CatchFiles','total_catch_random_catch1.ts'),
  sim_list = my_simulations,
  species_names = NULL,
  start_time = 7665,
  end_time = 20805,
  n_top = 5,
  window_size = 10
)

saveRDS(metric_results, 'Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_catch_weight_metrics.rds')

# 3. Optional: Inspect all top correlations in the dataset
top_corrs <- explore_top_correlations(wide_preds, threshold = 0.85)

# 4. Clean the data to remove redundant metric categories across all species
final_clean_preds <- remove_redundant_metric_categories(wide_preds, cutoff = 0.85)
 
# 5. Save the final matrix to feed directly into the Random Forest batch script
saveRDS(final_clean_preds, 'Z:/fishing_sensitivity_manuscript/data/random_catch_combined/clean_wide_catch_metrics.rds')
