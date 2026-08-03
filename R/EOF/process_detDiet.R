# process_run.R
# This script processes Atlantis runs in parallel on a single Linux node using forking.

# Load necessary libraries
library(tictoc) # For timing
library(dplyr) 
library(here) 
library(atlantiseof) 
library(atlantisdiagnostics) 
library(foreach)     # For parallel loops
library(doParallel)  # Parallel backend

`%>%` = dplyr::`%>%`

# --- Setup variables and paths ---
project.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
experiment.id = 'eof_targeting_4'
setup.df = read.csv(paste0(project.dir, 'Setup_Files/', experiment.id, '_setup.csv'))
partition.name = 'atlantisarchive/Joseph.Caracappa'
run.dir.index = setup.df$run
redo = FALSE


system(paste0('sudo chmod 777 -R /',partition.name,'/'))

if(redo == TRUE){
  output.dirs = list.files(paste0('/atlantisarchive/Joseph.Caracappa/', experiment.id, '/analysis/'), include.dirs = T)
  complete.names = paste0(experiment.id, '_', setup.df$run)
  which.missing = which(!(complete.names %in% output.dirs))
  run.dirs = paste0('/',partition.name,'/', experiment.id, '/', experiment.id, '_', which.missing, '/')
  run.dir.index = which.missing
} else {
  run.dirs = paste0('/',partition.name,'/', experiment.id, '/', experiment.id, '_', setup.df$run, '/')
}

# --- Initialize Parallel Backend (Forking) ---
num_cores <- parallel::detectCores() - 1 
message("Initializing parallel multicore processing with ", num_cores, " cores.")

# On Linux, passing 'cores' directly uses forking. No makeCluster() needed!
registerDoParallel(cores = num_cores)

# --- Parallel Processing Loop ---
tic() # Start timer

#length(run.dirs)
parallel_results <- foreach(i = 1:length(run.dirs)) %dopar% {
  
  run_index = run.dir.index[i]
  run.dir = paste0('/',partition.name,'/', experiment.id, '/', experiment.id, '_', run_index, '/')
  
  # Print progress directly to the console
  cat(sprintf("[%s] STARTING: run directory %s\n", Sys.time(), run.dir), file = stderr())
  
  # Execute the processing function
  atlantiseof::process_det_diet(atl.dir = run.dir, 
                                detDietfile = "neus_outputDetailedDietCheck.txt", 
                                outputname = "neus_outputDetDiet_processed.gz",
                                cloud = FALSE) 
  
  cat(sprintf("[%s] FINISHED: run directory %s\n", Sys.time(), run.dir), file = stderr())
}

toc() # End timer
message("Batch processing complete.")