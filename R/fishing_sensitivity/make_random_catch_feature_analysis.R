# Load required libraries
library(dplyr)
library(tidyr)
library(ranger)
library(ggplot2)

#' Identify Top Drivers for a Single Target Species
#'
#' @param wide_predictors Output from prep_wide_predictors() (or cleaned wide .rds)
#' @param target_outcomes A dataframe with columns 'Experiment' and the target variable
#' @param target_col Character; the name of the column containing the target variable (e.g., "rel_distance")
#' @param n_top Integer; how many top drivers to return and plot
#' 
#' @return A list containing the full model, importance dataframe, and a plot.
#' @export
identify_target_drivers <- function(wide_predictors, target_outcomes, target_col, n_top = 10) {
  
  # 1. Ensure Experiment columns match types
  target_outcomes$Experiment <- as.character(target_outcomes$Experiment)
  
  # SAFETY CHECK: Extract ONLY the Experiment and the specific target column
  # This prevents the model from "cheating" by using other metrics like 'biomass.ref'
  target_clean <- target_outcomes[, c("Experiment", target_col)]
  
  # 2. Merge predictors and target outcome (Excludes runs missing from predictors)
  model_data <- inner_join(target_clean, wide_predictors, by = "Experiment") %>%
    select(-Experiment) # Drop ID column for modeling
  
  # Check if we have enough data
  if (nrow(model_data) < 50) {
    stop("Not enough matching experiments to run a robust Random Forest.")
  }
  
  # 3. Train the Random Forest
  # Formula: target_col ~ all other columns (.)
  formula <- as.formula(paste(target_col, "~ ."))
  
  rf_model <- ranger(
    formula = formula, 
    data = model_data, 
    importance = 'permutation',
    num.trees = 1000 # High number of trees for stable importance scoring
  )
  
  # 4. Extract and sort variable importance
  importance_df <- data.frame(
    Predictor = names(rf_model$variable.importance),
    Importance = as.numeric(rf_model$variable.importance)
  ) %>%
    arrange(desc(Importance)) %>%
    mutate(Relative_Importance_Pct = round((Importance / max(Importance)) * 100, 1))
  
  # 5. Create a plot of the Top N Drivers
  top_drivers <- head(importance_df, n_top)
  
  p <- ggplot(top_drivers, aes(x = reorder(Predictor, Importance), y = Importance, fill = Predictor)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      subtitle = paste("R-squared:", round(rf_model$r.squared, 3)),
      x = "Catch Metric (Species_Metric)",
      y = "Permutation Importance"
    )
  
  return(list(
    model = rf_model,
    importance_data = importance_df,
    plot = p
  ))
}

#' Batch Analyze All Target Species
#'
#' @param wide_predictors The wide predictor dataframe.
#' @param target_outcomes_full The full dataframe containing all species outcomes (must have 'Code' column).
#' @param target_col Character; the column to predict (e.g., "rel_distance").
#' @param n_top Integer; number of top drivers to save in plots.
#'
#' @return A list containing the combined importance dataframe, a summary of the #1 driver per species, and all plots.
#' @export
analyze_all_target_drivers <- function(wide_predictors, target_outcomes_full, target_col = "rel_distance", n_top = 10) {
  
  unique_targets <- unique(target_outcomes_full$Code)
  
  all_importances <- list()
  all_plots <- list()
  
  # Loop through every species Code
  for (target_sp in unique_targets) {
    
    # Subset outcomes for just this species
    sp_outcomes <- target_outcomes_full %>% filter(Code == target_sp)
    
    # Use tryCatch to prevent loop from breaking if one species fails (e.g., all 0s)
    res <- tryCatch({
      identify_target_drivers(
        wide_predictors = wide_predictors,
        target_outcomes = sp_outcomes,
        target_col = target_col,
        n_top = n_top
      )
    }, error = function(e) {
      message(paste("Skipped", target_sp, "-", e$message))
      return(NULL)
    })
    
    # If model succeeded, extract and label the data
    if (!is.null(res)) {
      
      # Append Target_Species and R_squared to the importance dataframe
      imp_df <- res$importance_data %>%
        mutate(
          Target_Species = target_sp,
          R_squared = res$model$r.squared
        ) %>%
        select(Target_Species, R_squared, Predictor, Importance, Relative_Importance_Pct)
      
      all_importances[[target_sp]] <- imp_df
      
      # Finalize the plot title
      p <- res$plot + labs(title = paste("Target Species:", target_sp))
      all_plots[[target_sp]] <- p
    }
  }
  
  # Combine all the individual lists into single dataframes
  combined_importances <- dplyr::bind_rows(all_importances)
  
  # Generate a quick summary of the absolute #1 driver for each species
  top_1_summary <- combined_importances %>%
    group_by(Target_Species) %>%
    slice_max(order_by = Importance, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(R_squared))
  
  return(list(
    importance_df = combined_importances,
    top_1_summary = top_1_summary,
    plots = all_plots
  ))
}
# ==============================================================================
# EXAMPLE USAGE:
# ==============================================================================

# 1. Load your ALREADY CLEANED AND WIDE predictors from the previous script
# We bypass prep_wide_predictors() entirely because the data is already prepped!
wide_preds <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/clean_wide_catch_metrics.rds')

# 2. Run the batch analysis for ALL species in your dataframe
df_contrasts <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_contrasts.rds') |> 
  rename(Experiment = 'ID')

batch_results <- analyze_all_target_drivers(
  wide_predictors = wide_preds, # Pass the loaded dataframe directly!
  target_outcomes_full = df_contrasts, 
  target_col = "rel_distance",
  n_top = 10
)

# 3. View the #1 driver for every species sorted by model accuracy
print(batch_results$top_1_summary)

# 4. Filter the full importance dataframe to dive deep into a specific species
focal_species_data <- batch_results$importance_df %>% filter(Target_Species == "MAK")

# 5. Save ALL 57 plots into a single, scrollable PDF
pdf(here::here('Manuscript','Figures',"All_Species_Drivers.pdf"), width = 8, height = 6)
invisible(lapply(batch_results$plots, print))
dev.off()

drivers.df = batch_results$top_1_summary |> 
  tidyr::separate(Predictor, c('Predictor_Species','Metric'),sep ='_',remove = F) |> 
  mutate(Self_Driver = ifelse(Predictor_Species == Target_Species, "Yes", "No")) 

drivers.df |> 
  select(Target_Species, Self_Driver) |> 
  arrange(desc(Self_Driver))

drivers.df |> 
  group_by(Predictor) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

drivers.df |> 
  group_by(Metric) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

##Plot rel_distance as a function of LOB_T50

df.combined = wide_preds |> 
  filter(Experiment != 'Original') |> 
  mutate(Experiment = as.numeric(Experiment)) |> 
  left_join(df_contrasts)

ggplot(df.combined, aes(x = LOB_T50, y = rel_distance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Code, scale = 'free_y') +
  theme_minimal() +
  labs(title = "Relationship between LOB_T50 and Relative Distance",
       x = "LOB_T50",
       y = "Relative Distance")

ggplot(df.combined, aes(x = Aggregate_T50, y = rel_distance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Code, scale = 'free_y') +
  theme_minimal() +
  labs(title = "Relationship between LOB_T50 and Relative Distance",
       x = "LOB_T50",
       y = "Relative Distance")

