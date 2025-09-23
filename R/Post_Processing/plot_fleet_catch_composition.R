# -----------------------------------------------------------------------------
# SCRIPT NAME: plot_fleet_catch_composition.R
#
# AUTHOR: [Original Author Name]
# DATE: [Date of Last Revision]
#
# DESCRIPTION:
# This script visualizes and compares the species composition of catch for
# different fishing fleets. It processes catch data from an Atlantis model run
# and compares it against a reference dataset.
#
# KEY FUNCTIONS:
# 1.  Processes model output to calculate the proportional species composition
#     of catch for each groundfish fleet over time.
# 2.  Generates a multi-page PDF with time series plots showing the catch
#     composition for each individual fleet.
# 3.  Aggregates and plots the average catch composition over the last 10 years
#     of the model run.
# 4.  Loads and plots the catch composition from a reference dataset.
# 5.  Creates a final comparative plot that directly contrasts the model's
#     catch composition (last 10 years) with the reference data for each port.
#
# LIBRARIES:
# - dplyr: For data manipulation.
# - ggplot2: For creating plots.
# - RColorBrewer: For creating color palettes.
# - gridExtra: For arranging plots.
# - tidyr: For data reshaping.
#
# INPUTS:
# - Atlantis model output: /Post_Processed/Data/catch_fleet.rds
# - Spatial reference landings: /data/spatial_reference_landings_fleet.rds
#
# OUTPUTS:
# - PDF Plot:
#   - {run.name}_fleet_catch_composition.pdf
# - PNG Plots:
#   - {run.name}_catch_composition_all.png
#   - groundfish_catch_composition_reference.png
#   - {run.name}_catch_composition_all_ref.png
# -----------------------------------------------------------------------------


# --- 1. LOAD LIBRARIES AND SETUP ---
library(dplyr)
library(ggplot2)
library(RColorBrewer) # For plot color palettes
library(gridExtra)    # For arranging plots in the PDF
library(tidyr)        # For the separate() function

# --- Configuration ---
# Define the name of the Atlantis model run to be analyzed.
run.name = 'GF_gloucester_scale_2'
# Define directories.
run.dir = here::here('Atlantis_Runs', run.name)
figure.dir = paste0(run.dir, '/Post_Processed/')


# --- 2. PROCESS MODEL DATA FOR TIME SERIES ANALYSIS ---

# Load the processed Atlantis model catch data for all fleets.
catch.fleet = readRDS(paste0(run.dir, '/Post_Processed/Data/catch_fleet.rds'))

# Calculate the proportional catch composition for each fleet at each time step.
catch.fleet.comp = catch.fleet %>%
  mutate(time = as.numeric(time)) %>%
  # Filter for groundfish fleets and a specific time period (e.g., after model spin-up).
  filter(grepl('gf', fleet) & time >= 30) %>%
  # Summarize total catch for each species within a fleet at each time step.
  group_by(fleet, species, time) %>%
  summarise(catch = sum(atoutput, na.rm = T), .groups = 'drop') %>%
  # Calculate the total catch across all species for the fleet at that time step.
  group_by(fleet, time) %>%
  mutate(catch.tot = sum(catch, na.rm = T)) %>%
  ungroup() %>%
  # Calculate the proportion of each species.
  mutate(catch.prop = ifelse(catch.tot == 0, 0, catch / catch.tot))

# Get a unique list of all fleet names for the plotting loop.
fleet.names = sort(unique(catch.fleet$fleet))


# --- 3. PLOT CATCH COMPOSITION TIME SERIES FOR EACH FLEET ---

# Define a color palette for the species.
plot.cols = c(RColorBrewer::brewer.pal(12, 'Paired'), 'grey50', 'grey30')

# Open a PDF device to save the plots.
pdf(paste0(figure.dir, run.name, '_fleet_catch_composition.pdf'))
# Loop through each fleet and create a plot.
for (i in 1:length(fleet.names)) {
  plot.data = catch.fleet.comp %>%
    filter(fleet == fleet.names[i])
  
  # Create a stacked bar chart showing species proportion over time.
  p = ggplot(plot.data, aes(x = time, y = catch.prop, fill = species)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_fill_manual(values = plot.cols) +
    theme_bw() +
    ggtitle(paste0('Fleet: ', fleet.names[i])) +
    ylab('Proportion of Landings')
  
  # Print the plot to the PDF file.
  gridExtra::grid.arrange(p)
  
}
# Close the PDF device.
dev.off()


# --- 4. ANALYZE AVERAGE COMPOSITION (LAST 10 YEARS OF MODEL) ---

# Aggregate catch over the last 10 years of the simulation for groundfish fleets.
catch.fleet.comp.last10 = catch.fleet %>%
  mutate(time = as.numeric(time)) %>%
  filter(time >= (max(time) - 10) & grepl('^gf', fleet)) %>%
  group_by(fleet, species) %>%
  summarise(catch = sum(atoutput, na.rm = T), .groups = 'drop') %>%
  group_by(fleet) %>%
  mutate(catch.tot = sum(catch, na.rm = T),
         catch.prop = catch / catch.tot)

# Plot the average catch composition for all groundfish fleets.
ggplot(catch.fleet.comp.last10,
       aes(x = fleet, y = catch.prop, fill = species)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(values = plot.cols) +
  theme_bw() +
  ylab('Proportion of Landings') +
  xlab('') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 12
        ))

# Save the plot.
ggsave(
  paste0(figure.dir, run.name, '_catch_composition_all.png'),
  width = 10,
  height = 6,
  units = 'in',
  dpi = 300
)


# --- 5. ANALYZE REFERENCE DATA CATCH COMPOSITION ---

# Load and process the reference landings data.
catch.ref = readRDS(here::here('data', 'spatial_reference_landings_fleet.rds')) %>%
  filter(statistic == 'value' &
           grepl('^gf', fleet) & !is.na(species) & fleet != 'gfother') %>%
  group_by(fleet, species) %>%
  summarise(catch = sum(ref.value, na.rm = T), .groups = 'drop') %>%
  group_by(fleet) %>%
  mutate(catch.tot = sum(catch, na.rm = T),
         catch.prop = catch / catch.tot)

# Plot the catch composition from the reference data.
ggplot(catch.ref, aes(x = fleet, y = catch.prop, fill = species)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(values = plot.cols) +
  theme_bw() +
  ylab('Proportion of Landings') +
  xlab('') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 12
        ))

# Save the plot.
ggsave(
  here::here('data', 'groundfish_catch_composition_reference.png'),
  width = 12,
  height = 10,
  units = 'in',
  dpi = 300
)


# --- 6. CREATE COMPARISON PLOT (MODEL VS. REFERENCE) ---

# Prepare the model data for comparison.
catch.fleet.comp.last10 = catch.fleet.comp.last10 %>%
  tidyr::separate(fleet, c('dum', 'port'), sep = 'gf', remove = F) %>%
  mutate(source = 'model')

# Prepare the reference data for comparison.
catch.ref = catch.ref %>%
  tidyr::separate(fleet, c('dum', 'port'), sep = 'gf', remove = F) %>%
  mutate(source = 'reference')

# Combine the two datasets.
catch.comp.all = catch.fleet.comp.last10 %>%
  bind_rows(catch.ref)

# Create a faceted plot to compare model vs. reference side-by-side.
ggplot(catch.comp.all, aes(x = port, y = catch.prop, fill = species)) +
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(name = 'Species', values = plot.cols) +
  # Create separate panels for 'model' and 'reference'.
  facet_wrap( ~ source, nrow = 2) +
  theme_bw() +
  ylab('Proportion of Landings') +
  xlab('') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          size = 12
        ))

# Save the final comparison plot.
ggsave(
  paste0(figure.dir, run.name, '_catch_composition_all_ref.png'),
  width = 10,
  height = 12,
  units = 'in',
  dpi = 300
)
