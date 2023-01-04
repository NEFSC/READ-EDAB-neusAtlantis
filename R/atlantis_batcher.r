library(tidyverse)
library(here)

# batcherFilename     = csv file containing the information to run the batcher
# userName            = network username - used to create docker/podman container name
# CHECK_TIME_INTERVAL = interval in seconds between checks for a container finishing
# NUM_TO_RUN          = number of containers to run simultaneously
# CONTAINER_TYPE      = 'podman' or 'docker'

atlantis_batcher = function(batcherFilename, userName, CHECK_TIME_INTERVAL = 30, NUM_TO_RUN = 3, CONTAINER_TYPE = 'docker') {
  
  batcherFile <- read.csv(batcherFilename, as.is = T)
  numRuns <- nrow(batcherFile)
  
  folders <- batcherFile$OutputDir
  
  logFileSuffix <- strsplit(batcherFilename,".",fixed=TRUE)[[1]][1]
  logFileSuffix_split <- strsplit(logFileSuffix,"/")[[1]]
  logFileSuffix <- logFileSuffix_split[[length(logFileSuffix_split)]]
  logfileName <- paste0("log_",logFileSuffix)
  columns_logfile <- c("Run_name", "Start_time", "End_time", "Status")
  logData <- data.frame(matrix(nrow = numRuns, ncol = length(columns_logfile)))
  colnames(logData) = columns_logfile
  
  
  # Check for already running containers
  container_ps_output <- system(paste0(CONTAINER_TYPE, " ps"), intern = TRUE)
  # Set number of containers to be removed from original Number to Run and subtract from original Number to Run
  numContainersRunning <- length(container_ps_output) - 1   # -1 to eliminate the header row
  numContainersToCreate <- NUM_TO_RUN-numContainersRunning
  
  # Create a list of run directories to be instantiated
  startedContainers <- vector(mode="character")
  containersToRun <- folders
  
  param.dir <- paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/")
  output.dir <- paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs")
  #    param.dir <- paste0(here(),"/Rob_Project_Template/Project_Name_Version/")
  #    output.dir <- paste0(here(),"/Rob_Project_Template/Project_Name_Version/Atlantis_Runs")
  
  
  for (n in 1:numContainersToCreate) {
    initFile <- batcherFile$InitNC[n]
    biolPrm <- batcherFile$BiolPrm[n]
    runPrm <- batcherFile$RunPrm[n]
    harvestPrm <- batcherFile$HarvestPrm[n]
    
    system(paste0("mkdir -p ",paste0('"',output.dir,folders[n],'"')))
    
    run.atlantis.sh = readLines(paste0(param.dir,'RunAtlantis_base.sh'))
    new.line = paste0('atlantisMerged -i ', initFile, ' 0 -o neus_output.nc -r ', runPrm, ' -f at_force_LINUX.prm -p at_physics.prm -b ',biolPrm,' -h ', harvestPrm, ' -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
    print(new.line)
    run.atlantis.sh[3] = new.line
    writeLines(run.atlantis.sh, con = paste0(param.dir,'RunAtlantis.sh'))
    
    # run docker
    
    #build run command string
    containerName <- paste0(userName, '_Batcher_Test_', n )
    startedContainers <- append(startedContainers, containerName)
    print(containerName)
    outputFolder <- paste0('"',output.dir,folders[n],'"')
    print(paste0(CONTAINER_TYPE," run -d --name ",  containerName, " --rm --mount \"type=bind,src=",param.dir,",dst=/app/model\" --mount \"type=bind,src=",outputFolder,"/,","dst=/app/model/output/\" atlantis_6536"))
    run <- paste0(CONTAINER_TYPE," run -d --name ",  containerName, " --rm --mount \"type=bind,src=",param.dir,",dst=/app/model\" --mount \"type=bind,src=",outputFolder,"/,","dst=/app/model/output/\" atlantis_6536")
    
    system(run)
    logData$Run_name[n] <- batcherFile$Run[n]
    logData$Start_time[n] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    logData$End_time[n] < "Unfinished"
    logData$Status[n] <- "Started"
    batcherFile$Status[n] <- "Started"
    try(write.csv(logData,paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/",logfileName), row.names = FALSE, append = FALSE))
    try(write.csv(batcherFile,batcherFilename, row.names = FALSE, append = FALSE))
    
    Sys.sleep(120)  
  }
  notFinished <- TRUE
  
  # Will be finished when no container names are left in a to run list
  while (notFinished) {
    Sys.sleep(CHECK_TIME_INTERVAL)
    container_ps_output <- system(paste0(CONTAINER_TYPE, " ps"), intern = TRUE)
    if (length(container_ps_output) > 1) {
      container_ps_output <- container_ps_output[2:length(container_ps_output)]                          # Remove the header row
      numContainers <- length(container_ps_output)
      activeContainers <- trimws(vector(mode="character"))
      
      # Create list of active containers
      for (c in 1:numContainers) {
        activeContainerInfo <- unlist(strsplit(container_ps_output[c], " "))
        activeContainerInfoLength <- length(activeContainerInfo)
        activeContainer <- activeContainerInfo[activeContainerInfoLength]
        activeContainers <- append(activeContainers, activeContainer)
      }
    } else {
      activeContainers <- c("NO CONTAINERS")
    }
    # For each container created (startedContainers) check to see if the container is still running (activeContainers)
    numStartedRuns <- length(startedContainers)

    
    endedIndices <- c()
    for (j in 1:numStartedRuns) {
      checkedContainer <- startedContainers[j]
      # If a created container is no longer an active run
      if (!(checkedContainer %in% activeContainers)) {
        # Keep track of indices of ended containers
        endedIndices <- c(endedIndices, j)
        for (r in 1:n) {
          if (grepl(batcherFile$Run[r], checkedContainer)) {
            batcherFile$Status[r] <- "Completed"
          }
        }
        # Update status in the logfile
        # Update status in the batcher data file 
        for (r in 1:n) {
          if (grepl(logData$Run_name[r], checkedContainer)) {
            logData$Status[r] <- "Completed"
          }
        }
      }
    }
    
    # Remove ended containers from started container list
    if (length(endedIndices) > 0) {
      startedContainers <- startedContainers[-endedIndices]
    }

    # Check for finished status (startedContainers empty and number started (n) equal or greater to number of containers in the batcher file (numRuns))
    if ((length(startedContainers) == 0) && (n >= numRuns)) {
      notFinished <- FALSE
    } else {
      # Start new runs equal to the difference between the number of running containers and NUM_TO_RUN
      # or number of remaining runs total to do, whichever is smaller
      diffRunningAndNumToRun <- NUM_TO_RUN - numContainers
      remainingRuns <- numRuns - n
      if (diffRunningAndNumToRun <= remainingRuns) {
        numToSetup <- diffRunningAndNumToRun
      } else {
        numToSetup <- remainingRuns
      }
      
      if (numToSetup > 0) {
        endRun <- n + numToSetup
        n <- n + 1
        for (i in n:endRun) {
          
          initFile <- batcherFile$InitNC[i]
          biolPrm <- batcherFile$BiolPrm[i]
          runPrm <- batcherFile$RunPrm[i]
          harvestPrm <- batcherFile$HarvestPrm[i]
          
          system(paste0("mkdir -p ",paste0('"',output.dir,folders[i],'"')))
          
          run.atlantis.sh = readLines(paste0(param.dir,'RunAtlantis_base.sh'))
          new.line = paste0('atlantisMerged -i ', initFile, ' 0 -o neus_output.nc -r ', runPrm, ' -f at_force_LINUX.prm -p at_physics.prm -b ',biolPrm,' -h ', harvestPrm, ' -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
          print(new.line)
          run.atlantis.sh[3] = new.line
          writeLines(run.atlantis.sh, con = paste0(param.dir,'RunAtlantis.sh'))
          
          # run docker
          
          #build run command string
          containerName <- paste0(userName, '_Batcher_Test_', i )
          startedContainers <- append(startedContainers, containerName)
          print(containerName)
          outputFolder <- paste0('"',output.dir,folders[i],'"')
          print(paste0(CONTAINER_TYPE," run -d --name ",  containerName, " --rm --mount \"type=bind,src=",param.dir,",dst=/app/model\" --mount \"type=bind,src=",outputFolder,"/,","dst=/app/model/output/\" atlantis_6536"))
          run <- paste0(CONTAINER_TYPE," run -d --name ",  containerName, " --rm --mount \"type=bind,src=",param.dir,",dst=/app/model\" --mount \"type=bind,src=",outputFolder,"/,","dst=/app/model/output/\" atlantis_6536")
          
          system(run)
          logData$Run_name[i] <- batcherFile$Run[i]
          logData$Start_time[i] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
          logData$End_time[i] < "Unfinished"
          logData$Status[i] <- "Started"
          batcherFile$Status[i] <- "Started"
          try(write.csv(logData,paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/",logfileName), row.names = FALSE, append = FALSE))
          try(write.csv(batcherFile,batcherFilename, row.names = FALSE, append = FALSE))
          
          Sys.sleep(120)
          
        }
        n <- i
      }
   }
  }
  try(write.csv(logData,paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/",logfileName), row.names = FALSE, append = FALSE))
  try(write.csv(batcherFile,batcherFilename, row.names = FALSE, append = FALSE))
  
}
