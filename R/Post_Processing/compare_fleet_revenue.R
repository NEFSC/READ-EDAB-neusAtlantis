# -----------------------------------------------------------------------------
# SCRIPT NAME: compare_fleet_revenue.R
#
# AUTHOR: [Original Author Name]
# DATE: [Date of Last Revision]
#
# DESCRIPTION:
# This script compares the economic revenue of fishing fleets across multiple
# Atlantis model runs (scenarios). It calculates the annual revenue for each
# fleet by applying historical price data to the model's catch output. The
# script then generates and saves plots comparing these revenue streams.
#
# KEY FUNCTIONS:
# 1.  Defines a set of Atlantis model runs to compare.
# 2.  Loads and processes species-specific price data.
# 3.  Loops through each model run, calling a helper script
#     (`make_catch_revenue.R`) to calculate annual revenue.
# 4.  Combines the revenue data from all runs into a single data frame.
# 5.  Generates two comparative plots:
#     - A faceted line plot showing revenue over time for each individual port,
#       comparing the different scenarios.
#     - A line plot showing the total aggregated revenue across all fleets,
#       comparing the scenarios.
# 6.  Saves both plots as PNG files.
#
# LIBRARIES:
# - dplyr: For data manipulation.
# - ggplot2: For creating plots.
# - tidyr: For data reshaping (separate).
# - here: For constructing portable file paths.
#
# INPUTS:
# - A vector of Atlantis run names (`run.names`).
# - A species price reference file (`data-raw/species_price_reference.csv`).
# - A helper script to calculate revenue (`R/Post_Processing/make_catch_revenue.R`).
#
# OUTPUTS:
# - PNG Plots:
#   - Gloucester_Groundfish_Consolidation_Revenue_Fleet.png
#   - Gloucester_Groundfish_Consolidation_Revenue_Total.png
# -----------------------------------------------------------------------------


# --- 1. LOAD LIBRARIES AND SETUP ---
library(dplyr)
library(ggplot2)
library(tidyr) # For the separate() function
source(here::here('R', 'Post_Processing', 'make_catch_revenue.R'))

# --- Configuration ---
# Define the Atlantis model run directories to compare.
run.names = c('GF_gloucester_scale_2', 'dev_07152025')
# Provide human-readable names for the plot legend corresponding to run.names.
run.long.names = c('Increased Gloucester Effort', 'Normal Scenario')
# Define paths to the run directories.
run.dirs = here::here('Atlantis_Runs', run.names, '/')

# Load and process the species price data, averaging by code and year.
price.reference.orig = read.csv(here::here('data-raw', 'species_price_reference.csv'))
price.reference = price.reference.orig %>%
  dplyr::group_by(Code, Year) %>%
  dplyr::summarise(Price = mean(Price, na.rm = T), .groups = 'drop')


# --- 2. CALCULATE REVENUE FOR EACH RUN ---

# Initialize a list to store the revenue data from each run.
revenue.ls = list()

# Loop through each specified run name.
for (i in 1:length(run.names)) {
  # Call the helper function to calculate revenue from the model's catch data.
  # The function is set not to write or plot its own output.
  revenue.ls[[i]] = make_catch_revenue(
    run.name = run.names[i],
    run.dir = run.dirs[i],
    price.reference = price.reference,
    write = F,
    plot = F,
    price.year = 2023 # Specify the price year to use for calculations.
  ) %>%
    # Add columns to identify which run the data belongs to.
    dplyr::mutate(run.name = run.names[i],
                  run.long.name = run.long.names[i])
}


# --- 3. PROCESS COMBINED DATA AND PLOT REVENUE BY FLEET ---

# Combine the list of data frames into a single data frame.
revenue.df = dplyr::bind_rows(revenue.ls) %>%
  # Filter for groundfish fleets only.
  filter(grepl('^gf', Fishery)) %>%
  # Separate the port name from the fleet code (e.g., 'gfgloucester' -> 'gloucester').
  tidyr::separate(Fishery, c('dum', 'Port'), sep = 'gf', remove = F) %>%
  # Calculate revenue in millions of dollars for easier plotting.
  mutate(revenue.million = Revenue * 1E-6) %>%
  # Filter out years beyond the scope of the analysis.
  filter(Year < 2021)

# Create a faceted plot showing revenue by port for each scenario.
ggplot(revenue.df, aes(x = Year, y = revenue.million, color = run.long.name)) +
  geom_line() +
  facet_wrap( ~ Port) +
  theme_bw() +
  scale_color_brewer(name = 'Scenario', type = 'qual') +
  ylab('Revenue (Millions USD)') +
  theme(legend.position = 'bottom')

# Save the plot.
ggsave(
  here::here('Figures', 'Gloucester_Groundfish_Consolidation_Revenue_Fleet.png'),
  width = 12,
  height = 12,
  units = 'in',
  dpi = 300
)


# --- 4. PLOT TOTAL REVENUE ACROSS ALL FLEETS ---

# Aggregate the data to get the total revenue across all fleets for each year and scenario.
revenue.total = revenue.df %>%
  group_by(Year, run.long.name) %>%
  summarise(revenue.million = sum(Revenue, na.rm = T) * 1E-6, .groups = 'drop')

# Create a line plot of the total revenue over time.
ggplot(revenue.total, aes(x = Year, y = revenue.million, color = run.long.name)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(name = 'Scenario', type = 'qual') +
  ylab('Revenue (Millions USD)') +
  theme(legend.position = 'bottom')

# Save the plot.
ggsave(
  here::here('Figures', 'Gloucester_Groundfish_Consolidation_Revenue_Total.png'),
  width = 8,
  height = 4,
  units = 'in',
  dpi = 300
)
