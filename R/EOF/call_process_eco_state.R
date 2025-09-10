# process_run.R
# This script processes a single Atlantis run based on the provided array task ID.

# Load necessary libraries
library(tictoc)
library(dplyr)
library(here)
library(atlantiseof)
library(atlantisdiagnostics)

`%>%` = dplyr::`%>%`

# --- Get the array task ID from command-line arguments ---
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Error: No array task ID provided. Please run this script with an argument (e.g., Rscript process_run.R 1)")
}


array_task_id = as.integer(args[1])

run_index = array_task_id # Using 1-based indexing directly from SLURM_ARRAY_TASK_ID

# --- Setup file and experiment ID ---
project.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
message('project.dir exists? ',dir.exists(project.dir))
experiment.id = 'catch_thresholds_eof_3'
setup.df = read.csv(paste0(project.dir,'Setup_Files/catch_thresholds_eof_setup.csv'))

message(paste0("Processing run for array task ID: ", array_task_id))
message(paste0("Corresponding R run index: ", run_index))

# Check if the run_index is valid
if (run_index > nrow(setup.df) || run_index < 1) {
  stop(paste0("Error: Invalid run index (", run_index, ") for setup.df with ", nrow(setup.df), " rows."))
}

# --- Define output directories ---
output.dir = paste0('/atlantisarchive/Joseph.Caracappa/',experiment.id,'/analysis/')
if(!dir.exists(output.dir)){
  dir.create(output.dir, recursive = TRUE)
}
message('Output dir exists? ',dir.exists(output.dir))

# Get the specific run directory for this task
run.dir = paste0('/atlantisarchive/Joseph.Caracappa/',experiment.id,'/',experiment.id,'_',setup.df$run[run_index],'/')
print(list.files(run.dir))
message(paste0("Run ", run_index,": ",dir.exists(run.dir)))
message(paste0("Processing run directory: ", run.dir))

print(list.files('/'))
# --- Perform calculations for the specific run ---
tic(paste0("Processing run ", run_index))

# Create data subdirectory within the run directory and set permissions
data.dir = paste0(run.dir,'data') # Corrected: use run.dir
if(!dir.exists(data.dir)){
  dir.create(data.dir, recursive = TRUE)
}
message('Finished making output directories for processed run:',run_index)


survdat.data = readRDS(paste0(project.dir,'data-raw/surdat_lenagewgt.rds'))
group.index.file = paste0(project.dir,'data-raw/group_index.rds')

message('Finished make_eco_indicators_time for run:',run_index)

message('Making output directories for processed run:',run_index)
param.ls = atlantisdiagnostics::get_atl_paramfiles(param.dir =paste0(project.dir,'currentVersion/'),
                                                   atl.dir = run.dir, # Corrected: use run.dir
                                                   run.prefix = 'neus_output',
                                                   include_catch = TRUE
)


# Process Atlantis output
message('Running process_atl_output for run:',run_index)
if (length(list.files(run.dir, pattern = 'Catch.txt'))==0) {
  atlantisdiagnostics::process_atl_output(param.dir = paste0(project.dir,'currentVersion/'),
                                          atl.dir = run.dir, # Corrected: use run.dir
                                          out.dir = data.dir,
                                          run.prefix = 'neus_output',
                                          param.ls = param.ls,
                                          plot.length.age = TRUE,
                                          plot.biomass.timeseries = TRUE,
                                          plot.numbers.timeseries = TRUE,
                                          plot.catch = FALSE)
} else {
  atlantisdiagnostics::process_atl_output(param.dir = paste0(project.dir,'currentVersion/'),
                                          atl.dir = run.dir, # Corrected: use run.dir
                                          out.dir = data.dir,
                                          run.prefix = 'neus_output',
                                          param.ls = param.ls,
                                          plot.length.age = TRUE,
                                          plot.biomass.timeseries = TRUE,
                                          plot.numbers.timeseries = TRUE,
                                          plot.catch = TRUE)
}
message('Finished process_atl_output for run:',run_index)

# make_eco_indicators_time
message('Running make_eco_indicators_time for run:',run_index)
run.ind.t = atlantiseof::make_eco_indicators_time(param.dir = paste0(project.dir,'currentVersion/'),
                                                  atl.dir = run.dir, # Corrected: use run.dir
                                                  group.index = group.index.file,
                                                  fgs.file = paste0(project.dir,'currentVersion/neus_groups.csv'),
                                                  dietSource = 'detdiet',
                                                  timeRange = 1:100,
                                                  survdat.data = survdat.data,
                                                  cloud = TRUE
)
message('Finished make_eco_indicators_time for run:',run_index)

# make_eco_indicators
message('Running make_eco_indicators for run:',run_index)
run.ind.mean = atlantiseof::make_eco_indicators(param.dir = paste0(project.dir,'currentVersion/'),
                                                atl.dir = run.dir, # Corrected: use run.dir
                                                group.index = group.index.file,
                                                fgs.file =paste0(project.dir,'currentVersion/neus_groups.csv'),
                                                dietSource = 'detdiet',
                                                timeRange = 1:100,
                                                cloud = TRUE
)

# Calculate PPR and PPC
message('Running get_ppc for run:',run_index)
ppc = atlantiseof::get_ppc(param.dir =paste0(project.dir,'currentVersion/'),
                           atl.dir = run.dir, # Corrected: use run.dir
                           fgs = paste0(project.dir,'currentVersion/neus_groups.csv'),
                           dietSource = 'realized',
                           timeRange = 1:100)
message('Finished get_ppcs for run:',run_index)

message('Exporting results for run:',run_index)
# --- Export results to atlantisarchive ---
export.dir = paste0('/atlantisarchive/Joseph.Caracappa/',experiment.id,'/analysis/',experiment.id,'_',run_index,'/')
if(!dir.exists(export.dir)){
  dir.create(export.dir, recursive = TRUE)
}

# Copy processed files
files.export = c('biomass.rds','biomass_age.rds','length_age.rds','numbers_age.rds','catch.rds')
file.copy(from = paste0(data.dir,'/',files.export), to = paste0(export.dir,'/',files.export), overwrite = TRUE)

# Copy detdiet processed file
file.copy(from = paste0(run.dir,'neus_outputDetDiet_processed.gz'), to = paste0(export.dir,'neus_outputDetDiet_processed.gz'), overwrite = TRUE)

# Save R objects
saveRDS(run.ind.t, paste0(export.dir, 'eco_indicators_ts.rds'))
saveRDS(run.ind.mean, paste0(export.dir,'eco_indicators_mean.rds'))
saveRDS(ppc, paste0(export.dir,'ppc.rds'))

toc()
message(paste0("Finished processing run ", run_index))