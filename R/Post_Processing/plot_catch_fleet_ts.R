# -----------------------------------------------------------------------------
# SCRIPT NAME: plot_catch_fleet_ts.R
#
# AUTHOR: [Original Author Name]
# DATE: [Date of Last Revision]
#
# DESCRIPTION:
# This script processes, analyzes, and visualizes fisheries catch data from an
# Atlantis ecosystem model run. It compares the model's simulated catch with
# reference (real-world) catch data to evaluate model performance and generate
# calibration factors.
#
# KEY FUNCTIONS:
# 1.  Loads simulated catch data from a specified Atlantis model run.
# 2.  Loads corresponding reference catch data for groundfish, scallop, and
#     other fisheries.
# 3.  Filters and aggregates both model and reference data by various groupings
#     (total, by fleet, by species, and by fleet-species combination).
# 4.  Generates and saves a series of plots comparing model vs. reference data.
# 5.  Calculates and saves two sets of correction factors to CSV files for use
#     in model calibration:
#     - Swept area corrections (total catch by fleet).
#     - Catchability (q) corrections (catch by species for each fleet).
#
# LIBRARIES:
# - dplyr: For data manipulation and transformation.
# - ggplot2: For creating plots and visualizations.
# - mapdata: For accessing map datasets.
# - here: For constructing portable file paths relative to the project root.
# - gridExtra: For arranging multiple plots.
# - tidyr: For reshaping data frames.
#
# INPUTS:
# - Atlantis model output: /Post_Processed/Data/catch_fleet.rds
# - Functional groups file: /currentVersion/neus_groups.csv
# - Reference data files:
#   - /data-raw/data/groundfishFleetData.rds
#   - /data-raw/data/scallopFleetData.rds
#   - /data/neusCatchData.rds
#
# OUTPUTS:
# - PNG Plots:
#   - _catch_fleet_total.png
#   - _catch_species_total.png
#   - _catch_total.png
# - PDF Plot:
#   - _catch_fleet_species.pdf (multi-page)
# - CSV Correction Factors:
#   - _sweptarea_corrections.csv
#   - _q_corrections.csv
# -----------------------------------------------------------------------------


# --- 1. LOAD LIBRARIES ---
library(dplyr)      # For data manipulation (filter, group_by, summarise, etc.)
library(ggplot2)    # For creating high-quality plots
library(mapdata)    # For map data (though not used in final plots)


# --- 2. SETUP AND CONFIGURATION ---

# Define the name of the Atlantis model run to be analyzed.
run.name = 'GF_gloucester_scale_2'

# Define directories and file paths using the 'here' library for portability.
run.dir = here::here('Atlantis_Runs', run.name)

# Set the start and end years for the analysis period (relative to model start).
ref.years = c(20, 60)

# Define the directory to save output figures and create it if it doesn't exist.
figure.dir = paste0(run.dir, '/Post_Processed/')
if (!dir.exists(figure.dir)) {
  dir.create(figure.dir)
}

# Load functional group information, mapping long names to species codes.
fgs = read.csv(here::here('currentVersion', 'neus_groups.csv')) %>%
  select(LongName, Code)

# Load map data for the USA and Canada (defined but not used in this script).
neus.map = map_data('worldHires', region = c('USA', 'Canada'))


# --- 3. LOAD AND PROCESS MODEL DATA ---

# Load the processed Atlantis model catch data for all fleets.
catch.fleet.model = readRDS(paste0(run.dir, '/Post_Processed/Data/catch_fleet.rds')) %>%
  rename(year = 'time') %>%
  mutate(time = (year * 365)) %>% # Convert year to a 'time' variable in days.
  filter(year >= ref.years[1] & year <= ref.years[2]) # Filter for the specified year range.

# Aggregate model catch data in three ways:

# 3.1. Total catch across all species for each fleet over time.
catch.fleet.tot = catch.fleet.model %>%
  group_by(time, fleet) %>%
  summarise(catch = sum(atoutput, na.rm = T)) %>%
  mutate(var = 'model') # Add a variable to identify this as model data.

# 3.2. Catch by species for each fleet over time.
catch.fleet.spp = catch.fleet.model %>%
  group_by(time, fleet, species) %>%
  summarise(catch = sum(atoutput, na.rm = T)) %>%
  mutate(var = 'model')

# 3.3. Total catch for each species across all fleets over time.
catch.spp = catch.fleet.model %>%
  group_by(time, species) %>%
  summarise(catch = sum(atoutput, na.rm = T)) %>%
  mutate(var = 'model')


# --- 4. LOAD AND PROCESS REFERENCE DATA ---

# Load and process reference landings data for three different fishery types.

# 4.1. Groundfish fleet data.
catch.ref.gf = readRDS(here::here('data-raw', 'data', 'groundfishFleetData.rds'))$landings %>%
  mutate(fleet = paste0('gf', gsub(' ', '', tolower(newport)))) %>%
  filter(Year >= (1964 + ref.years[1]) &
           Year <= (1964 + ref.years[2]) & Box %in% 1:22) %>%
  select(Year, Box, fleet, Code, landings) %>%
  group_by(Year, fleet, Code) %>%
  summarise(catch = sum(landings, na.rm = T))

# 4.2. Scallop fleet data.
catch.ref.sca = readRDS(here::here('data-raw', 'data', 'scallopFleetData.rds'))$landings %>%
  mutate(fleet = paste0('SCA', gsub(' ', '', tolower(newport)))) %>%
  select(Year, Box, fleet, Code, landings) %>%
  filter(Year >= (1964 + ref.years[1]) &
           Year <= (1964 + ref.years[2]) & Box %in% 1:22) %>%
  group_by(Year, fleet, Code) %>%
  summarise(catch = sum(landings, na.rm = T))

# 4.3. Other species data (assigned to a 'catchall' fleet).
catch.ref.ts = readRDS(here::here('data', 'neusCatchData.rds')) %>%
  filter(!(Code %in% c(unique(catch.ref.gf$Code), 'SCA')) &
           YEAR >= (1964 + ref.years[1]) &
           YEAR <= (1964 + ref.years[2])) %>%
  rename(Year = 'YEAR',
         catch = 'value') %>%
  mutate(fleet = 'catchall')

# Combine the three reference datasets into a single data frame.
catch.ref = catch.ref.gf %>%
  bind_rows(catch.ref.sca) %>%
  bind_rows(catch.ref.ts) %>%
  mutate(time = (Year - 1964) * 365) %>% # Convert year to days, assuming 1964 start.
  left_join(fgs) %>% # Join with functional group data to get species names.
  rename(species = 'LongName')

# Aggregate the combined reference data in the same three ways as the model data.

# 4.4. Total reference catch by fleet.
catch.ref.fleet.tot = catch.ref %>%
  group_by(time, fleet) %>%
  summarise(catch = sum(catch, na.rm = T)) %>%
  mutate(var = 'reference') # Add variable to identify as reference data.

# 4.5. Reference catch by fleet and species.
catch.ref.fleep.spp = catch.ref %>%
  group_by(time, fleet, species) %>%
  summarise(catch = sum(catch, na.rm = T)) %>%
  mutate(var = 'reference')

# 4.6. Total reference catch by species.
catch.ref.spp = catch.ref %>%
  group_by(time, species) %>%
  summarise(catch = sum(catch, na.rm = T)) %>%
  mutate(var = 'reference')


# --- 5. PLOT CATCH BY FLEET & CALCULATE SWEPT AREA CORRECTIONS ---

# Combine model and reference data for total catch by fleet.
catch.fleet.tot.all = catch.fleet.tot %>%
  bind_rows(catch.ref.fleet.tot) %>%
  mutate(year = floor(time / 365)) %>% # Convert time in days back to years.
  group_by(year, fleet, var) %>%
  summarise(catch = sum(catch, na.rm = T))

# Create and save a plot comparing total catch (model vs. reference) for each fleet.
ggplot(data = catch.fleet.tot.all, aes(x = year, y = catch, col = var)) +
  geom_line() +
  facet_wrap( ~ fleet, scale = 'free_y') + # Separate plot panel for each fleet.
  theme_bw()
ggsave(
  paste0(figure.dir, run.name, '_catch_fleet_total.png'),
  width = 12,
  height = 12,
  dpi = 250
)

# Calculate swept area correction factors. This metric compares the total
# reference catch to the total model catch for each fleet, which can be used
# to adjust the fishing effort parameter in the model.
sweptarea.corr = catch.fleet.tot.all %>%
  tidyr::spread(var, catch) %>% # Reshape data to have 'model' and 'reference' as columns.
  filter(!is.na(reference) & model > 0) %>%
  mutate(corr.ratio = reference / model) %>% # Calculate the ratio of reference/model.
  group_by(fleet) %>%
  summarise(
    sweptarea.corr.mean = mean(corr.ratio, na.rm = T),
    sweptarea.corr.max = max(corr.ratio, na.rm = T),
    sweptarea.corr.min = min(corr.ratio, na.rm = T)
  )

# Save the swept area correction factors to a CSV file.
write.csv(
  sweptarea.corr,
  here::here('Setup_Files', paste0(run.name, '_sweptarea_corrections.csv')),
  row.names = F
)


# --- 6. PLOT CATCH BY SPECIES ---

# Combine model and reference data for total catch by species.
catch.spp.all = catch.spp %>%
  bind_rows(catch.ref.spp)

# Aggregate data by year for plotting.
catch.spp.allyr = catch.spp.all %>%
  mutate(year = floor(time / 365)) %>%
  group_by(year, species, var) %>%
  summarise(catch = sum(catch, na.rm = T))

# Create and save a plot comparing total catch (model vs. reference) for each species.
ggplot(data = catch.spp.allyr, aes(x = year, y = catch, col = var)) +
  geom_line() +
  facet_wrap( ~ species, scale = 'free_y') +
  theme_bw()
ggsave(
  paste0(figure.dir, run.name, '_catch_species_total.png'),
  width = 24,
  height = 24,
  dpi = 250
)

# Create and save a plot of the total aggregated catch across all fleets and species.
catch.whole.model = catch.spp.allyr %>%
  group_by(year, var) %>%
  summarise(catch = sum(catch, na.rm = T))
ggplot(data = catch.whole.model, aes(x = year, y = catch, col = var)) +
  geom_line() +
  theme_bw() +
  ggtitle('All Catch')
ggsave(
  paste0(figure.dir, run.name, '_catch_total.png'),
  width = 8,
  height = 5,
  dpi = 250
)


# --- 7. PLOT FLEET-SPECIES COMBINATIONS & CALCULATE Q CORRECTIONS ---

# Combine model and reference data for catch by both fleet and species.
catch.fleet.spp.all = catch.fleet.spp %>%
  bind_rows(catch.ref.fleep.spp)

# Get a unique list of fleet names to loop through.
fleet.names = sort(unique(catch.fleet.spp.all$fleet))

# Initialize an empty list to store correction factor data frames.
catch.corr.ls = list()

# Open a multi-page PDF device to save all the plots from the loop.
pdf(paste0(figure.dir, run.name, '_catch_fleet_species.pdf'),
    width = 12,
    height = 12)

# Loop through each fleet to generate plots and calculate catchability (q) correction factors.
for (i in 1:length(fleet.names)) {
  # Filter data for the current fleet and aggregate by year.
  this.catch = catch.fleet.spp.all %>%
    filter(fleet == fleet.names[i]) %>%
    mutate(year = floor(time / 365)) %>%
    group_by(year, fleet, species, var) %>%
    summarise(catch = sum(catch, na.rm = T))
  
  # Create a faceted plot showing catch by species for the current fleet.
  p = ggplot(data = this.catch, aes(x = year, y = catch, col = var)) +
    geom_line() +
    facet_wrap( ~ species, scale = 'free_y') +
    ggtitle(fleet.names[i]) +
    theme_bw()
  
  # Print the plot to the PDF device.
  gridExtra::grid.arrange(p)
  
  # Calculate catchability (q) correction factors for each species in the fleet.
  # This compares the model vs reference catch for a specific species by a
  # specific fleet, which can be used to adjust the catchability parameter 'q'.
  catch.corr.ls[[i]] = this.catch %>%
    tidyr::spread(var, catch) %>% # Reshape data.
    filter(!is.na(reference) & model > 0) %>%
    group_by(fleet, species) %>%
    summarise(
      model.mean = mean(model, na.rm = T),
      model.max = max(model, na.rm = T),
      ref.mean = mean(reference, na.rm = T),
      ref.max = max(reference, na.rm = T)
    ) %>%
    mutate(
      corr.mean = ifelse(model.mean == 0, 1, ref.mean / model.mean),
      corr.max = ifelse(model.max == 0, 1, ref.max / model.max)
    )
}

# Close the PDF device, saving the file.
dev.off()

# Combine the list of correction factors from the loop into a single data frame.
catch.corr.df = bind_rows(catch.corr.ls)

# Save the final catchability (q) correction factors to a CSV file.
write.csv(catch.corr.df,
          here::here('Setup_Files', paste0(
            run.name, '_q_corrections.csv'
          )),
          row.names = F)
