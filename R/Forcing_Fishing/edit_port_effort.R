# -----------------------------------------------------------------------------
# SCRIPT NAME: edit_port_effort.R
#
# AUTHOR: [Original Author Name]
# DATE: [Date of Last Revision]
#
# DESCRIPTION:
# This script is designed to create a new fishing effort scenario for the
# Atlantis model. It reads the existing effort distribution for a specific
# fishery (e.g., groundfish), and then reallocates that effort so that a
# target fleet (port) accounts for a new, specified proportion of the total
# effort.
#
# The script ensures that:
# 1. The total effort across all fleets at each time step remains unchanged.
# 2. The target fleet's effort is scaled to meet the desired proportion.
# 3. The effort of all other (non-target) fleets is scaled proportionally to
#    make up the remaining effort.
# 4. The relative effort proportions among the non-target fleets are preserved.
#
# It includes options to write the new effort files, update the Atlantis
# forcing parameter file, and generate a comparison plot.
#
# LIBRARIES:
# - dplyr: For data manipulation.
# - here: For constructing portable file paths.
# - forcats: Used implicitly via ggplot2 for factor level reordering.
# - tidyr: Used for data reshaping.
#
# INPUTS:
# - Atlantis fisheries definition file (`neus_fisheries.csv`).
# - Atlantis effort time series files (`effort_box*.ts`).
# - Atlantis forcing parameter file (`at_force_LINUX.prm`).
#
# OUTPUTS:
# - A new set of effort*.ts files in a dedicated experiment directory.
# - An updated at_force_LINUX.prm file pointing to the new effort files.
# - A PNG bar chart comparing the original vs. modified effort distribution.
# -----------------------------------------------------------------------------


# --- 1. LOAD LIBRARIES AND SETUP ---
library(dplyr)
library(ggplot2) # Required for plotting
library(forcats) # Required for fct_relevel
library(tidyr)   # Required for separate
project.dir = here::here("")

# --- Control Flags ---
# Set to TRUE to write new .ts files and update the .prm file.
write.effort = FALSE
# Set to TRUE to generate and save the comparison plot.
plot.effort = TRUE

# --- Scenario Parameters ---
# Name for the new scenario/experiment directory.
experiment.name = 'GF_effort_redist'
# The fleet to be targeted for reallocation.
target.fleet = 'gfgloucester'
# The desired final proportion of total effort for the target fleet.
target.prop = 0.32
# Tolerance for numeric checks to account for floating-point inaccuracies.
tol = 1E-6

# Source helper scripts for reading/writing Atlantis .ts files.
source(paste0(project.dir, 'R/Forcing_Fishing/edit_forcing_ts.r'))
source(paste0(project.dir, 'R/Forcing_Fishing/get_forcing_ts.r'))


# --- 2. LOAD INITIAL DATA AND FILE SETUP ---

# Read fleets data and identify all groundfish fleets.
fleet.file = paste0(project.dir, 'currentVersion/neus_fisheries.csv')
fleet.def = read.csv(fleet.file)
# Note: The original script used 'sca.fleets' but it refers to groundfish. Renaming for clarity.
gf.fleets = grep('^gf', fleet.def$Code, value = T)

# Get paths and names for all existing effort files.
effort.dir = paste0(project.dir, 'currentVersion/CatchFiles/')
effort.file.name = list.files(effort.dir, '^effort_box.*.ts$')
effort.file.name.long = list.files(effort.dir, '^effort_box.*.ts$', full.names = T)
effort.file.name.short = gsub('.ts', '', effort.file.name)
# Extract the box number from each file name.
effort.file.box = gsub("^effort_box(\\d+)\\.ts$", "\\1", effort.file.name)

# Create a new directory to store the modified effort files for this experiment.
new.effort.dir = paste0(project.dir, 'currentVersion/CatchFiles/', experiment.name, '/')
if (!dir.exists(new.effort.dir)) {
  dir.create(new.effort.dir)
}

# --- 3. READ AND AGGREGATE ORIGINAL EFFORT DATA ---

# Loop through each groundfish fleet and each effort file to read all data.
orig.effort.ls = list()
ind.ls = 1
for (f in 1:length(gf.fleets)) {
  for (b in 1:length(effort.file.name)) {
    # Use the helper function to read the time series data.
    orig.effort.ls[[ind.ls]] = get_forcing_ts(gf.fleets[f],
                                              filenm = effort.file.name.short[b],
                                              time = 'daily') %>%
      dplyr::mutate(box = effort.file.box[b])
    ind.ls = ind.ls + 1
  }
}

# Combine the list of data frames into a single master data frame.
orig.effort.df = dplyr::bind_rows(orig.effort.ls) %>%
  dplyr::rename(fleet = 'Variable',
                effort = 'Value')


# --- 4. REALLOCATE EFFORT BASED ON TARGET PROPORTION ---

# This block re-calculates effort for each fleet at each time step.
scenario.effort.df <- orig.effort.df %>%
  # Drop any 0-effort rows so sums are only over active boxes/fleets.
  filter(effort > 0) %>%
  # For each day (time step), compute the key totals needed for scaling.
  group_by(Time) %>%
  mutate(
    total_T = sum(effort),
    orig_target_T = sum(effort[fleet == target.fleet]),
    orig_non_target_T = total_T - orig_target_T,
    # Calculate scaling factors.
    # The new target effort will be total_T * target.prop.
    # The new non-target effort will be total_T * (1 - target.prop).
    scale_target = if_else(orig_target_T > 0, (target.prop * total_T) / orig_target_T, 0),
    scale_non_target = if_else(orig_non_target_T > 0, ((1 - target.prop) * total_T) / orig_non_target_T, 0)
  ) %>%
  ungroup() %>%
  # Apply the calculated scaling factors to each row's effort value.
  mutate(new_effort = if_else(
    fleet == target.fleet,
    effort * scale_target,
    effort * scale_non_target
  )) %>%
  # Select only the columns needed for writing the new files.
  select(Time, fleet, box, effort = new_effort)


# --- 5. SANITY CHECKS TO VERIFY REALLOCATION ---

# Check 1: Ensure the total effort for each day remains unchanged.
check_totals <- orig.effort.df %>%
  filter(effort > 0) %>%
  group_by(Time) %>%
  summarize(orig_total = sum(effort)) %>%
  inner_join(
    scenario.effort.df %>%
      group_by(Time) %>%
      summarize(new_total = sum(effort)),
    by = "Time"
  ) %>%
  mutate(diff = orig_total - new_total)

if (any(abs(check_totals$diff) > tol, na.rm = TRUE)) {
  stop("Total-effort mismatch on some days.")
}

# Check 2: Ensure the target fleet's proportion of effort is correct for each day.
check_target <- scenario.effort.df %>%
  group_by(Time) %>%
  summarize(
    target_ratio = sum(effort[fleet == target.fleet]) / sum(effort),
    diff = target_ratio - target.prop
  )

if (any(abs(check_target$diff) > tol, na.rm = TRUE)) {
  stop("Target-fleet proportion not met on some days.")
}

# Check 3: Ensure the relative proportions among non-target fleets are preserved.
orig_NA_props <- orig.effort.df %>%
  filter(effort > 0, fleet != target.fleet) %>%
  group_by(Time, fleet) %>%
  summarize(orig_e = sum(effort), .groups = 'drop') %>%
  group_by(Time) %>%
  mutate(orig_prop = orig_e / sum(orig_e)) %>%
  select(Time, fleet, orig_prop)

new_NA_props <- scenario.effort.df %>%
  filter(fleet != target.fleet) %>%
  group_by(Time, fleet) %>%
  summarize(new_e = sum(effort), .groups = 'drop') %>%
  group_by(Time) %>%
  mutate(new_prop = new_e / sum(new_e)) %>%
  select(Time, fleet, new_prop)

check_NA <- orig_NA_props %>%
  inner_join(new_NA_props, by = c("Time", "fleet")) %>%
  mutate(diff = orig_prop - new_prop)

if (any(abs(check_NA$diff) > tol, na.rm = TRUE)) {
  stop("Relative proportions among non-target fleets changed on some days.")
}

# If all checks pass, print a success message.
message("All effort-reallocation checks succeeded.")


# --- 6. WRITE NEW EFFORT FILES AND UPDATE PRM FILE ---

if (write.effort) {
  # Define the new file names based on the experiment name.
  new.file.names = paste0(effort.file.name.short, '_', experiment.name)
  
  # First, copy all original files to have a complete set to start with.
  file.copy(effort.file.name.long, paste0(effort.dir, new.file.names, '.ts'))
  
  # Loop through each effort box file to update it.
  for (i in 1:length(effort.file.name)) {
    this.box = as.numeric(effort.file.box[i])
    this.box.effort = filter(scenario.effort.df, box == this.box)
    if (nrow(this.box.effort) == 0) {
      next()
    }
    
    # Get the unique fleets that have effort in this box.
    which.fleets = sort(unique(this.box.effort$fleet))
    
    # For each fleet, write its new effort time series to the file.
    for (j in 1:length(which.fleets)) {
      this.fleet.effort = filter(this.box.effort, fleet == which.fleets[j]) %>%
        select(Time, effort) %>%
        as.matrix()
      
      # Use helper function to edit the .ts file in place.
      edit_forcing_ts(
        code = which.fleets[j],
        tstype = 'effort',
        trange = this.fleet.effort,
        filename = new.file.names[i],
        keep = F
      )
    }
  }
  
  # Move the newly created files to the experiment directory.
  file.copy(paste0(effort.dir, new.file.names, '.ts'),
            new.effort.dir,
            overwrite = T)
  file.remove(paste0(effort.dir, new.file.names, '.ts'))
  file.remove(list.files(effort.dir, '*.tstemp', full.names = T))
  
  # Edit the force.prm file to point to the new effort files.
  force.prm.file = paste0(project.dir, 'currentVersion/at_force_LINUX.prm')
  force.lines = readLines(force.prm.file)
  effort.file.line = grep("Effortts[0-9]+\\.data\\s+CatchFiles/effort_box[0-9]+\\.ts",
                          force.lines)
  effort.str.orig = force.lines[effort.file.line]
  effort.pattern <-
    "Effortts([0-9]+)\\.data\\s+CatchFiles/effort_box([0-9]+)\\.ts"
  effort.replacement <-
    paste0(
      "Effortts\\1.data CatchFiles/",
      experiment.name,
      "/effort_box\\2_",
      experiment.name,
      ".ts"
    )
  new.effort.str <- sub(effort.pattern, effort.replacement, effort.str.orig)
  
  force.lines[effort.file.line] = new.effort.str
  writeLines(force.lines, force.prm.file)
}


# --- 7. PLOT COMPARISON OF ORIGINAL VS. MODIFIED EFFORT ---

if (plot.effort) {
  # Prepare data for plotting by combining original and scenario data frames.
  orig.effort.df$source = 'original'
  scenario.effort.df$source = 'modified'
  
  all.effort.df = scenario.effort.df %>%
    bind_rows(orig.effort.df) %>%
    filter(effort > 0) %>%
    group_by(source, fleet) %>%
    summarise(effort = sum(effort, na.rm = T), .groups = 'drop') %>%
    tidyr::separate(fleet, c('dum', 'port'), sep = 'gf', remove = F)
  
  # Reorder factor levels for a more logical plot layout.
  all.effort.df$port <- as.factor(all.effort.df$port)
  all.effort.df$port <-
    fct_relevel(all.effort.df$port, "other", after = Inf)
  
  # Create and save the bar chart.
  ggplot(all.effort.df, aes(x = port, y = effort, fill = source)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Total Effort') +
    xlab('') +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 16,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = element_text(size = 12),
      legend.position = 'bottom',
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15)
    )
  
  ggsave(
    here::here(
      'Figures',
      'Groundfish_Gloucester_consolidation_32_comparison.png'
    ),
    width = 12,
    height = 4,
    units = 'in'
  )
}
