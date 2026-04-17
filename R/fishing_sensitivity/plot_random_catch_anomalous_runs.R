#plot all the timeseries that resulted in anomalous runs from random catch scenario
library(dplyr)
library(ggplot2)

results.summ = readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_path_dependence.rds') |> 
  rename(source = 'experiment.id')
results.raw =  readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_mean_5yr_proj.rds')
sim.combs = readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_permutations.rds')

source(here::here('R','fishing_sensitivity','plot_random_catch_timeseries.R'))

data = results.raw |> 
  extract(
    col = run.id, 
    into = c("prefix", "exp_group", "exp_run"), 
    regex = "([A-Za-z]+)(\\d+)_(\\d+)", 
    remove = FALSE, 
    convert = TRUE
  ) %>%
  # 2. Group by the numeric experiment and run values
  group_by(exp_group, exp_run) %>%
  # 3. Assign a single unified ID to every row in the group
  mutate(ID = cur_group_id()) %>%
  # 4. Ungroup and clean up the temporary columns
  ungroup() %>%
  select(-prefix, -exp_group, -exp_run) |> 
  left_join(results.summ, by = 'Code') |> 
  filter(Biomass.ref > 0) |> 
  mutate(biomass.diff = Biomass- Biomass.ref,
         biomass.diff.rel = abs(biomass.diff / Biomass.ref)) |> 
  rename(biomass = 'Biomass', biomass.ref = 'Biomass.ref') |> 
  select(Code, ID, biomass, biomass.ref)

length(unique(data$ID))
# Set the threshold for the tails (e.g., top 10% and bottom 10%)
tail_percentile <- 0.10 

df_contrasts <- data %>%
  mutate(
    # Use absolute relative distance to standardize across species of varying sizes
    rel_distance = abs((biomass - biomass.ref) / biomass.ref)
  ) %>%
  group_by(Code) %>%
  mutate(
    # Calculate the dynamic thresholds for each specific Code
    closest_threshold = quantile(rel_distance, probs = tail_percentile, na.rm = TRUE),
    furthest_threshold = quantile(rel_distance, probs = 1 - tail_percentile, na.rm = TRUE),
    
    # Categorize the runs
    divergence_class = case_when(
      rel_distance <= closest_threshold ~ "Closest",
      rel_distance >= furthest_threshold ~ "Furthest",
      TRUE ~ "Middle"
    )
  ) %>%
  # Keep only the extremes to set up your contrastive analysis
  # filter(divergence_class != "Middle") %>%
  ungroup() %>%
  # Arrange for easy review
  arrange(Code, divergence_class, rel_distance)

saveRDS(df_contrasts, 'Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_contrasts.rds')
# d = df_contrasts |> 
#   filter(Code == 'BLF')
# 
# ggplot(d, aes(x = ID, y = biomass, color = divergence_class))+
#   geom_hline(yintercept = d$biomass.ref[1])+
#   geom_point()
