# Load required libraries
library(dplyr)
library(tidyr)

#' Calculate Generalized Catch Time Series Metrics
#'
#' Computes the Center of Mass, T50, T90, and Peak Time for every individual
#' species, as well as the total system aggregate, across all randomized experiments.
#'
#' @param catch_file Character string; the path to the catch forcing text file.
#' @param sim_list List; an R list where each element contains a numeric vector 
#'   representing the timestep order for that specific simulation run.
#' @param species_names Character vector or NULL. If NULL, the function will 
#'   automatically process all columns except "Time".
#'
#' @return A data frame with metrics for every species and the aggregate catch per run.
#' @export
calculate_all_catch_metrics <- function(catch_file, sim_list, species_names = NULL) {
  
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
        Peak_Time = NA_integer_
      ))
    }
    
    cum_prop <- cumsum(catch_vec) / total_c
    
    data.frame(
      Experiment = exp_label,
      Species = sp_name,
      Total_Catch = total_c,
      Center_Of_Mass = sum(time_steps * catch_vec, na.rm = TRUE) / total_c,
      T50 = time_steps[which.max(cum_prop >= 0.50)],
      T90 = time_steps[which.max(cum_prop >= 0.90)],
      Peak_Time = time_steps[which.max(catch_vec)]
    )
  }
  
  # ----------------------------------------------------------------------------
  # 4. Helper to Process an Entire Dataframe (Individual + Aggregate)
  # ----------------------------------------------------------------------------
  calc_experiment_metrics <- function(dat, exp_label) {
    
    # 4a. Calculate stats for each individual species
    indiv_list <- lapply(species_names, function(sp) {
      get_stats(dat[[sp]], sp, exp_label)
    })
    
    # 4b. Calculate aggregate stats across all species
    agg_vec <- rowSums(dat, na.rm = TRUE)
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
# EXAMPLE USAGE:
# ==============================================================================
my_simulations <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_permutations.rds')

metric_results <- calculate_all_catch_metrics(
  catch_file = here::here('currentVersion','CatchFiles','total_catch_random_catch1.ts'),
  sim_list = my_simulations,
  species_names = NULL
)

saveRDS(metric_results, 'Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_catch_weight_metrics.rds')
