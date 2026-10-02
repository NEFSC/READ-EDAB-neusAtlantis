# process_run.R
# This script processes a single Atlantis run based on the provided array task ID.
# Edit the .sh & Run this in command line

# sbatch /model/Joseph.Caracappa/READ-EDAB-neusAtlantis/R/Cloud_Utilities/cloud_postprocessing_sbatch.sh

# Load necessary libraries
# library(tictoc)
library(dplyr)
library(here)
library(atlantiseof)
library(atlantisdiagnostics)

`%>%` = dplyr::`%>%`

# --- Get the array task ID from command-line arguments ---
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  message("Error: No array task ID provided. Please run this script with an argument (e.g., Rscript process_run.R 1)")
  array_task_id = 2
  experiment.id = 'eof_targeting_4'
  run.dir.base = paste0('/atlantisarchive/Joseph.Caracappa/',experiment.id,'/')
  run.dir = paste0(run.dir.base,experiment.id,'_',array_task_id,'/')
  project.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
}else{
  
  experiment.id = args[1]
  project.dir = args[2]
  run.dir.base = args[3]
  array_task_id = as.integer(args[4])
  experiment.id = args[5]
  run.dir = args[6]
  
  print(args)
}

run_index = array_task_id # Using 1-based indexing directly from SLURM_ARRAY_TASK_ID

# --- Setup file and experiment ID ---
message('project.dir exists? ',dir.exists(project.dir))

setup.df = read.csv(paste0(project.dir,'Setup_Files/',experiment.id,'_setup.csv'))
message(paste0("Processing run for array task ID: ", array_task_id))
message(paste0("Corresponding R run index: ", run_index))

# Check if the run_index is valid
if (run_index > nrow(setup.df) || run_index < 1) {
  stop(paste0("Error: Invalid run index (", run_index, ") for setup.df with ", nrow(setup.df), " rows."))
}

# Get the specific run directory for this task
print(list.files(run.dir))
message(paste0("Run ", run_index,": ",dir.exists(run.dir)))
message(paste0("Processing run directory: ", run.dir))

# --- Perform calculations for the specific run ---
print(paste0("Processing run ", run_index))

# Create data subdirectory within the run directory and set permissions
data.dir = paste0(run.dir,'/data') # Corrected: use run.dir
if(!dir.exists(data.dir)){
  dir.create(data.dir, recursive = TRUE)
}
message('Using run directory ',run.dir)
message('Finished making output directories for processed run:',run_index)


survdat.data = readRDS(paste0(project.dir,'data-raw/surdat_lenagewgt.rds'))
group.index.file = paste0(project.dir,'data-raw/group_index.rds')

message('Making output directories for processed run:',run_index)
param.ls = atlantisdiagnostics::get_atl_paramfiles(param.dir =paste0(project.dir,'currentVersion/'),
                                                   atl.dir = run.dir, # Corrected: use run.dir
                                                   run.prefix = 'neus_output',
                                                   include_catch = TRUE
)

# Process Atlantis output
message('Running process_atl_output for run:',run_index)
run.files = list.files(run.dir)
data.files = list.files(data.dir)


include.catch = setup.df$catch.scalar[setup.df$run == array_task_id] != 0
if (sum(grepl('Catch.txt',run.files))==0) {
  expected.data.files = c('biomass_age_invert.rds', 'biomass_age.rds', 'biomass.rds', 'biomass_box.rds','biomass_box_invert.rds',
                           'data_age_mat.rds', 'dz.rds', 'length_age.rds', 'nominal_dz.rds', 'numbers_age.rds',
                           'numbers_box.rds', 'numbers.rds', 'RN_age_mean.rds', 'RN_age.rds', 'SN_age_mean.rds',
                           'SN_age.rds', 'volume.rds')
  if(!all(expected.data.files %in% data.files)){
    atlantisdiagnostics::process_atl_output(param.dir = paste0(project.dir,'currentVersion/'),
                                            atl.dir = run.dir, # Corrected: use run.dir
                                            out.dir = data.dir,
                                            run.prefix = 'neus_output',
                                            param.ls = param.ls,
                                            plot.length.age = TRUE,
                                            plot.biomass.timeseries = TRUE,
                                            plot.numbers.timeseries = TRUE,
                                            plot.biomass.box = TRUE,
                                            plot.spatial.biomass.seasonal = T,
                                            plot.catch = include.catch)
  }
} else {
  expected.data.files = c('biomass_age_invert.rds', 'biomass_age.rds', 'biomass.rds', 'catch.rds', 'catchmt.rds',
                          'data_age_mat.rds', 'dz.rds', 'length_age.rds', 'nominal_dz.rds', 'numbers_age.rds',
                          'numbers_box.rds', 'numbers.rds', 'RN_age_mean.rds', 'RN_age.rds', 'SN_age_mean.rds',
                          'SN_age.rds', 'totcatch.rds', 'volume.rds','biomass_box.rds','biomass_box_invert.rds')
  if(!all(expected.data.files %in% data.files)){
    
    atlantisdiagnostics::process_atl_output(param.dir = paste0(project.dir,'currentVersion/'),
                                            atl.dir = run.dir, # Corrected: use run.dir
                                            out.dir = data.dir,
                                            run.prefix = 'neus_output',
                                            param.ls = param.ls,
                                            plot.length.age = TRUE,
                                            plot.biomass.timeseries = TRUE,
                                            plot.numbers.timeseries = TRUE,
                                            plot.biomass.box = TRUE,
                                            plot.spatial.biomass.seasonal = TRUE,
                                            plot.catch = include.catch)
  }
}
message('Finished process_atl_output for run:',run_index)
message('Current PostProcessed Output:',paste0(list.files(data.dir),collapse = ', '))

message(paste0("Finished processing run ", run_index))
