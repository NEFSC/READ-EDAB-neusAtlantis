# process_run.R
# This script processes a single Atlantis run based on the provided array task ID.

# Load necessary libraries
library(tictoc) # For timing, if desired (can be removed for production)
library(dplyr) # For the pipe operator
library(here) # For managing file paths relative to the project root
library(atlantiseof) # Atlantis EOF processing
library(atlantisdiagnostics) # Atlantis output processing
# Uncomment these if they are not already installed on the compute node
# library(R.utils)
# library(stocksmart) # Not explicitly used in the loop, but listed in original
# library(remotes) # If you need to install from github on the fly

`%>%` = dplyr::`%>%`

# --- Get the array task ID from command-line arguments ---
# Slurm array tasks are typically 1-indexed.
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  # stop("Error: No array task ID provided. Please run this script with an argument (e.g., Rscript process_run.R 1)")
  # args = run.dirs
  experiment.id = 'catch_thresholds_eof_3'
  setup.df = read.csv(here::here('Setup_Files','catch_thresholds_eof_setup.csv'))
  run.dirs = paste0('/atlantisdisk2/',experiment.id,'/',experiment.id,'_',setup.df$run,'/')
}else{
  
  array_task_id = as.integer(args[1])
}
i=1
# --- Setup file and experiment ID ---
project.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
experiment.id = 'catch_thresholds_eof_3'
setup.df = read.csv(paste0(project.dir,'Setup_Files/catch_thresholds_eof_setup.csv'))

for(i in 1:length(run.dirs)){
    
  array_task_id = run.dirs[i]
  # Adjust for 0-based vs 1-based indexing if necessary.
  # If your SLURM_ARRAY_TASK_ID starts from 1 and matches your R indexing (1:length), no adjustment is needed.
  # If SLURM_ARRAY_TASK_ID is 0-based and you want 1-based in R: run_index = array_task_id + 1
  run_index = array_task_id # Using 1-based indexing directly from SLURM_ARRAY_TASK_ID
  
  message(paste0("Processing run for array task ID: ", array_task_id))
  message(paste0("Corresponding R run index: ", run_index))
  

  # Check if the run_index is valid
  # if (run_index > nrow(setup.df) || run_index < 1) {
  #   stop(paste0("Error: Invalid run index (", run_index, ") for setup.df with ", nrow(setup.df), " rows."))
  # }
  
  # --- Define output directories ---
  # Output directory for overall analysis (might be created by the submit script or manually)
  # Note: 'sudo' commands are typically not used within Slurm jobs by users.
  # Ensure appropriate permissions are set for '/atlantisdisk2' or use a user-writable path.
  output.dir = paste0('/atlantisarchive/',experiment.id,'/analysis/')
  # It's better to ensure this top-level directory exists before submitting jobs,
  # or handle it carefully with user permissions.
  # if(!dir.exists(output.dir)){
  #   dir.create(output.dir, recursive = TRUE) # Use dir.create for R equivalent
  # }
  
  # Get the specific run directory for this task
  
  run.dir = paste0('/atlantisdisk2/',experiment.id,'/',experiment.id,'_',setup.df$run[run_index],'/')
  message(paste0("Processing run directory: ", run.dir))
  
  # --- Perform calculations for the specific run ---
  tic(paste0("Processing run ", run_index))
  
  # survdat.url = "https://github.com/NOAA-EDAB/atlantiseof/raw/refs/heads/dev/data-raw/survey_lenagewgt.rds"
  # temp_file = tempfile(fileext = '.rds')
  # download.file(survdat.url, destfile = paste0(project.dir,'data-raw/surdat_lenagewgt.rds'), mode = 'wb')
  survdat.data = readRDS(paste0(project.dir,'data-raw/surdat_lenagewgt.rds'))
  
  # group.url = 'https://raw.githubusercontent.com/NOAA-EDAB/atlantiseof/refs/heads/joe_branch/data-raw/neus_species_index.csv'
  # download.file(group.url,destfile = paste0(project.dir,'data-raw/group_index.csv'))
  group.index.file = paste0(project.dir,'data-raw/group_index.rds')
  
  # make_eco_indicators_time
  run.ind.t = atlantiseof::make_eco_indicators_time(param.dir = paste0(project.dir,'currentVersion/'),
                                                    atl.dir = run.dirs[i],
                                                    group.index = group.index.file,
                                                    fgs.file = paste0(project.dir,'currentVersion/neus_groups.csv'),
                                                    dietSource = 'detdiet',
                                                    timeRange = 1:100,
                                                    survdat.data = survdat.data,
                                                    cloud = TRUE
  )
  
  # make_eco_indicators
  run.ind.mean = atlantiseof::make_eco_indicators(param.dir = paste0(project.dir,'currentVersion/'),
                                                  atl.dir = run.dirs[i],
                                                  group.index = group.index.file,
                                                  fgs.file =paste0(project.dir,'currentVersion/neus_groups.csv'),
                                                  dietSource = 'detdiet',
                                                  timeRange = 1:100,
                                                  cloud = TRUE
  )
  
  param.ls = atlantisdiagnostics::get_atl_paramfiles(param.dir =paste0(project.dir,'currentVersion/'),
                                                    atl.dir = run.dirs[i],
                                                    run.prefix = 'neus_output',
                                                    include_catch = TRUE
  )
  
  # Create data subdirectory within the run directory and set permissions
  # Again, avoid 'sudo' in job scripts. Ensure your user has write permissions to run.dirs[i]
  # data.dir = paste0(run.dirs[i],'data')
  # if(!dir.exists(data.dir)){
  #   dir.create(data.dir, recursive = TRUE)
  #   # system(paste0('sudo chmod -R 777 ',data.dir)) # This might not work on all systems.
  #   # Better to configure umask or group permissions for the directory.
  # }
  # 
  # # Process Atlantis output
  # if (run.ind.mean$catch.tot == 0) {
  #   atlantisdiagnostics::process_atl_output(param.dir = paste0(project.dir,'currentVersion/'),
  #                                          atl.dir = run.dirs[i],
  #                                          out.dir = data.dir,
  #                                          run.prefix = 'neus_output',
  #                                          param.ls = param.ls,
  #                                          plot.length.age = TRUE,
  #                                          plot.biomass.timeseries = TRUE,
  #                                          plot.numbers.timeseries = TRUE,
  #                                          plot.catch = FALSE)
  # } else {
  #   atlantisdiagnostics::process_atl_output(param.dir = paste0(project.dir,'currentVersion/'),
  #                                          atl.dir = run.dirs[i],
  #                                          out.dir = data.dir,
  #                                          run.prefix = 'neus_output',
  #                                          param.ls = param.ls,
  #                                          plot.length.age = TRUE,
  #                                          plot.biomass.timeseries = TRUE,
  #                                          plot.numbers.timeseries = TRUE,
  #                                          plot.catch = TRUE)
  # }
  
  # Calculate PPR and PPC
  ppc = atlantiseof::get_ppc(param.dir =paste0(project.dir,'currentVersion/'),
                             atl.dir = run.dirs[i],
                             fgs = paste0(project.dir,'currentVersion/neus_groups.csv'),
                             dietSource = 'realized',
                             timeRange = 1:100)
  
  # --- Export results to atlantisarchive ---
  # Create export directory
  # Note: 'sudo' commands are typically not used within Slurm jobs.
  # Ensure /atlantisarchive has appropriate permissions or use a user-writable path.
  export.dir = paste0('/atlantisarchive/Joseph.Caracappa/',experiment.id,'/',experiment.id,'_',run_index,'/')
  # if(!dir.exists(export.dir)){
  #   dir.create(export.dir, recursive = TRUE)
  #   # system(paste0('sudo chmod 777 -R ',export.dir)) # This might not work on all systems.
  # }
  # 
  # # Copy processed files
  # files.export = c('biomass.rds','biomass_age.rds','length_age.rds','numbers_age.rds','catch.rds')
  # file.copy(from = paste0(data.dir,'/',files.export), to = paste0(export.dir,'/',files.export), overwrite = TRUE)
  # 
  # # Copy detdiet processed file
  # file.copy(from = paste0(run.dirs[i],'neus_outputDetDiet_processed.gz'), to = paste0(export.dir,'neus_outputDetDiet_processed.gz'), overwrite = TRUE)
  # 
  # Save R objects
  saveRDS(run.ind.t, paste0(export.dir, 'eco_indicators_ts.rds'))
  saveRDS(run.ind.mean, paste0(export.dir,'eco_indicators_mean.rds'))
  saveRDS(ppc, paste0(export.dir,'ppc.rds'))
  
  toc() # End timing for this run
  message(paste0("Finished processing run ", run_index))
  
}
