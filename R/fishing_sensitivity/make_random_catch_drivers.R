library(dplyr)
library(ggplot2)

#' Calculate Spearman Rank Correlations for all Species
#'
#' @param outcomes_df Dataframe containing Code, Experiment, and the target outcome.
#' @param predictors_df Wide dataframe containing Experiment and catch metrics.
#' @param metric Character string; the exact name of the catch metric to test.
#' @param target_col Character string; the outcome variable to test against.
#'
#' @return A sorted dataframe of correlation coefficients and p-values.
#' @export
calculate_metric_correlations <- function(outcomes_df, predictors_df, metric = "Aggregate_T50", target_col = "rel_distance") {
  
  # 1. Join the specific metric to the outcomes data
  # Select only the Experiment ID and the specific metric we want to test
  pred_subset <- predictors_df %>% select(Experiment, all_of(metric))
  
  # Merge data
  merged_data <- outcomes_df %>%
    inner_join(pred_subset, by = "Experiment")
  
  # 2. Calculate correlations grouped by species
  results <- merged_data %>%
    group_by(Code) %>%
    summarize(
      # Calculate Spearman rho (rank correlation)
      rho = cor(!!sym(metric), !!sym(target_col), method = "spearman", use = "complete.obs"),
      
      # Calculate the p-value (exact = FALSE prevents warnings for tied ranks)
      p_value = cor.test(!!sym(metric), !!sym(target_col), method = "spearman", exact = FALSE)$p.value,
      
      .groups = "drop"
    ) %>%
    # 3. Correct p-values for multiple comparisons (False Discovery Rate)
    mutate(adj_p_value = p.adjust(p_value, method = "bonferroni")) %>%
    # 4. Sort by the absolute strength of the correlation
    arrange(desc(abs(rho))) %>%
    # 5. Add Effect Size and Adjusted Significance flags
    mutate(
      Variance_Explained_Pct = round((rho^2) * 100, 1),
      Effect_Magnitude = case_when(
        abs(rho) >= 0.5 ~ "Large",
        abs(rho) >= 0.3 ~ "Medium",
        abs(rho) >= 0.1 ~ "Small",
        TRUE ~ "Negligible"
      ),
      Significance = case_when(
        adj_p_value < 0.001 ~ "***",
        adj_p_value < 0.01  ~ "**",
        adj_p_value < 0.05  ~ "*",
        TRUE ~ "ns"
      )
    )
  
  return(results)
}

# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

# 1. Load your existing data
df_contrasts <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_contrasts.rds') %>% 
  rename(Experiment = 'ID')
wide_preds <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/clean_wide_catch_metrics.rds') |> 
  filter(Experiment != 'Original') |> 
  mutate(Experiment = as.numeric(Experiment))

# 2. Run the correlations for Aggregate_T50
cor_results_T50 <- calculate_metric_correlations(
  outcomes_df = df_contrasts,
  predictors_df = wide_preds,
  metric = "Aggregate_T50",
  target_col = "rel_distance"
)

cor_results_T50 |> 
  group_by(Significance) |> 
  summarise(count = n())

cor_results_T50 |> 
  group_by(Effect_Magnitude) |> 
  summarise(count = n())

# View the top 10 species most heavily driven by Aggregate T50
print(head(cor_results_T50, 10))

# ------------------------------------------------------------------------------
# BONUS: Plot the relationship for the top species (e.g., if "COD" was #1)
# ------------------------------------------------------------------------------
top_species <- cor_results_T50$Code[1]
top_rho <- round(cor_results_T50$rho[1], 3)
top_adj_p <- signif(cor_results_T50$adj_p_value[1], 3)
top_var <- cor_results_T50$Variance_Explained_Pct[1]

plot_data <- df_contrasts %>%
  filter(Code == top_species) %>%
  inner_join(wide_preds, by = "Experiment")

ggplot(plot_data, aes(x = Aggregate_T50, y = rel_distance)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = paste("Effect of Aggregate T50 on", top_species, "Divergence"),
    subtitle = paste0("Spearman \u03C1: ", top_rho, " | Adj p-value: ", top_adj_p, " | Var Explained: ", top_var, "%"),
    x = "Aggregate T50 (Time Step)",
    y = "Relative Distance from Original Run"
  )

library(dplyr)
library(tidyr)

#' Find Top Species-Specific Catch Drivers using Exhaustive Correlation
#'
#' @param outcomes_df Dataframe containing Code, Experiment, and target outcome.
#' @param predictors_df Wide dataframe containing Experiment and catch metrics.
#' @param target_col Character string; outcome variable (e.g., "rel_distance").
#' @param metric_suffix Character string; the specific metric to isolate (e.g., "_T50").
#'
#' @return A list containing the full correlation matrix and a summary of the #1 driver per species.
#' @export
find_top_species_drivers <- function(outcomes_df, predictors_df, target_col = "rel_distance", metric_suffix = "_T50") {
  
  # 1. Identify all predictor columns that match the desired metric (e.g., all T50 columns)
  pred_cols <- grep(paste0(metric_suffix, "$"), names(predictors_df), value = TRUE)
  
  unique_targets <- unique(outcomes_df$Code)
  all_results <- list()
  
  # 2. Loop through every Target Species
  for (target in unique_targets) {
    
    # Isolate outcome data for this target
    target_data <- outcomes_df %>% filter(Code == target)
    merged_data <- inner_join(target_data, predictors_df, by = "Experiment")
    
    if (nrow(merged_data) < 10) next
    target_vec <- merged_data[[target_col]]
    
    # 3. Correlate Target Outcome against EVERY species' T50 metric
    cor_list <- lapply(pred_cols, function(p_col) {
      pred_vec <- merged_data[[p_col]]
      
      # Safety check: skip if the predictor has no variance (e.g., a species was never caught)
      if (sum(!is.na(pred_vec)) < 10 || sd(pred_vec, na.rm = TRUE) == 0) {
        return(data.frame(Predictor = p_col, rho = NA, p_value = NA))
      }
      
      res <- cor.test(pred_vec, target_vec, method = "spearman", exact = FALSE)
      data.frame(Predictor = p_col, rho = res$estimate, p_value = res$p.value)
    })
    
    # 4. Compile, FDR correct, and calculate effect sizes for THIS target species
    target_res <- bind_rows(cor_list) %>% 
      filter(!is.na(rho)) %>%
      mutate(
        Target_Species = target,
        adj_p_value = p.adjust(p_value, method = "fdr"), # Multiple comparisons correction
        Variance_Explained_Pct = round((rho^2) * 100, 1),
        Effect_Magnitude = case_when(
          abs(rho) >= 0.5 ~ "Large",
          abs(rho) >= 0.3 ~ "Medium",
          abs(rho) >= 0.1 ~ "Small",
          TRUE ~ "Negligible"
        ),
        Significance = case_when(
          adj_p_value < 0.001 ~ "***",
          adj_p_value < 0.01  ~ "**",
          adj_p_value < 0.05  ~ "*",
          TRUE ~ "ns"
        )
      ) %>%
      arrange(desc(abs(rho)))
    
    all_results[[target]] <- target_res
  }
  
  full_results <- bind_rows(all_results)
  
  # 5. Extract just the absolute #1 highest-correlated driver for each target species
  top_drivers <- full_results %>%
    group_by(Target_Species) %>%
    slice_max(order_by = abs(rho), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(abs(rho)))
  
  return(list(
    full_results = full_results, 
    top_1_summary = top_drivers
  ))
}

# Run the exhaustive analysis isolating just the T50 metric for all species
bivariate_results <- find_top_species_drivers(
  outcomes_df = df_contrasts,
  predictors_df = wide_preds,
  target_col = "rel_distance",
  metric_suffix = "_T50"  # You can easily swap this to "_Center_Of_Mass" or "_Peak_Time"
)

# View the #1 driver for every single species!
print(bivariate_results$top_1_summary %>% select(Target_Species, Predictor, rho, Effect_Magnitude, Significance))

saveRDS(bivariate_results, 'Z:/fishing_sensitivity_manuscript/data/random_catch_combined/bivariate_correlation_results.rds')


plot.data = bivariate_results$top_1_summary |>
  filter(Significance != 'ns') |> 
  group_by(Predictor,Effect_Magnitude) |> 
  summarise(count = n()) |> 
  tidyr::separate(Predictor, c('Species','metric'),sep = '_') |> 
  arrange(count)

plot.order = plot.data |> 
  group_by(Species) |> 
  summarise(count = sum(count)) |> 
  arrange(count) 

plot.data$Species = factor(plot.data$Species, levels = pull(plot.order,Species))


ggplot(data = plot.data, aes(y = Species, x = count, fill = Effect_Magnitude))+
  geom_bar(stat = 'identity', position = 'stack')+
  ylab('Functional Group T50')+
  xlab('Count')+
  scale_fill_manual(values = RColorBrewer::brewer.pal(3,'Set2'),name = 'Effect Magnitude',
                    labels = c(
                      "Large (|\u03c1| \u2265 0.5)", 
                      "Medium (0.3 \u2264 |\u03c1| < 0.5)", 
                      "Small (0.1 \u2264 |\u03c1| < 0.3)"
                    ))+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave('Z:/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/Figure_10_Top_Drivers_By_Species.png', width = 7, height = 8)

bivariate_results$top_1_summary |> 
  filter(grepl('LOB',Predictor), Significance != 'ns')

length(unique(bivariate_results$top_1_summary$Target_Species))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

#which spp not in random catch analysis
fgs$Code[which(!(fgs$Code %in% bivariate_results$top_1_summary$Target_Species))]

#which spp had no significant drivers
bivariate_results$top_1_summary |> 
  filter(Significance == 'ns') |> 
  pull(Target_Species)
