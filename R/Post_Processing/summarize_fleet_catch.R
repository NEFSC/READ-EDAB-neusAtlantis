# -----------------------------------------------------------------------------
# SCRIPT NAME: summarize_fleet_catch.R
#
# AUTHOR: [Original Author Name]
# DATE: [Date of Last Revision]
#
# DESCRIPTION:
# This script processes catch data from a specified Atlantis ecosystem model run.
# It focuses specifically on the groundfish fleets, aggregating their catch
# data by US state and by species within each state. The final summaries are
# then exported as CSV files for further analysis or reporting.
#
# KEY FUNCTIONS:
# 1.  Loads total catch data from a specified Atlantis model run.
# 2.  Filters the data to include only groundfish fleets ('gf*').
# 3.  Manually maps each groundfish fleet to a corresponding US state.
# 4.  Aggregates the groundfish catch in two ways:
#     - Total catch by state, species, and year.
#     - Total catch by state and year.
# 5.  Exports these two aggregated datasets to CSV files in the model's
#     'Post_Processed' directory.
#
# LIBRARIES:
# - dplyr: For data manipulation and transformation (group_by, summarise, etc.).
# - here: For constructing portable file paths relative to the project root.
#
# INPUTS:
# - Atlantis model output: /Post_Processed/Data/catch_fleet.rds
# - Functional groups file: /currentVersion/neus_groups.csv
#
# OUTPUTS:
# - CSV Summaries:
#   - groundfish_catch_state_species.csv
#   - groundfish_catch_state_total.csv
# -----------------------------------------------------------------------------


# --- 1. SETUP AND CONFIGURATION ---

# Define the name of the Atlantis model run to be analyzed.
run.name = 'GF_gloucester_scale_2'
# Define the main directory for the Atlantis run using 'here' for portability.
atl.dir = here::here('Atlantis_Runs', run.name, '/')

# Load the functional groups file which contains mappings of species codes to names.
fgs = read.csv(here::here('currentVersion', 'neus_groups.csv'))


# --- 2. LOAD AND PROCESS MODEL CATCH DATA ---

# Load the processed Atlantis model catch data for all fleets.
# The data is then immediately grouped and summarized to get total catch in metric tons.
catch.fleet = readRDS(paste0(atl.dir, '/Post_Processed/Data/catch_fleet.rds')) %>%
  group_by(species, fleet, time) %>%
  summarise(catch.mt = sum(atoutput, na.rm = T))

# Define a vector of groundfish species codes.
gf.spp = c('COD','HAD','YTF','POL','PLA','WTF','WHK','WIF','RED','HAL', 'WPF','OPT','WOL')
# Match the codes to their long names from the functional groups file.
# Note: This 'gf.spp.long' variable is defined but not used later in the script.
gf.spp.long = fgs$LongName[match(gf.spp, fgs$Code)]

# Filter the dataset to focus only on groundfish fleets.
# It selects rows where the fleet name starts with 'gf' and catch is positive.
# It also converts the model 'time' step to a calendar 'year'.
catch.groundfish = catch.fleet %>%
  filter(grepl('^gf', fleet) & catch.mt > 0) %>%
  mutate(year = floor(time) + 1964)


# --- 3. MAP FLEETS TO STATES AND AGGREGATE CATCH ---

# Get a sorted, unique list of the groundfish fleet names present in the data.
fleet.names = sort(unique(catch.groundfish$fleet))

# Manually create a data frame to map the specific fleet names to US states.
# IMPORTANT: This mapping is hard-coded. If the fleet names in the model run
# change, this data frame must be updated accordingly.
fleet.state = data.frame(
  fleet = fleet.names,
  state = c('MA', 'MA', 'MA', 'NY', 'MA', 'Other', 'RI', 'ME', 'NH')
)

# 3.1 Aggregate catch by state and species.
# Join the state mapping to the catch data, then group and summarize.
catch.groundfish.state.spp = catch.groundfish %>%
  left_join(fleet.state) %>%
  group_by(state, species, year) %>%
  summarise(catch.mt = sum(catch.mt, na.rm = T)) %>%
  arrange(year, state, species)

# Write the state-species summary to a CSV file.
write.csv(
  catch.groundfish.state.spp,
  paste0(atl.dir, 'Post_Processed/groundfish_catch_state_species.csv'),
  row.names = F
)

# 3.2 Aggregate total catch by state.
# Join the state mapping to the catch data again, then group and summarize.
catch.groundfish.state = catch.groundfish %>%
  left_join(fleet.state) %>%
  group_by(state, year) %>%
  summarise(catch.mt = sum(catch.mt, na.rm = T)) %>%
  arrange(year, state)

# Write the state-total summary to a CSV file.
write.csv(
  catch.groundfish.state,
  paste0(atl.dir, 'Post_Processed/groundfish_catch_state_total.csv'),
  row.names = F
)

# Note: The original script had an extraneous closing brace '}' here, which
# would cause an error. It has been removed.
