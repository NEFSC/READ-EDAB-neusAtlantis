library(tidyverse)
library(here)

# batcherFilename     = csv file containing the information to run the batcher
# userName            = network username - used to create docker/podman container name
# CHECK_TIME_INTERVAL = interval in seconds between checks for a container finishing
# NUM_TO_RUN          = number of containers to run simultaneously
# CONTAINER_TYPE      = 'podman' or 'docker'

atlantis_batcher = function(batcherFilename, userName, CHECK_TIME_INTERVAL = 30, NUM_TO_RUN = 3, CONTAINER_TYPE = 'docker') {

  batcherFilename <- '/net/work3/EDAB/atlantis/robVersion/batcher_test_1.csv'
  userName <- "rgamble"
  CHECK_TIME_INTERVAL = 30
  NUM_TO_RUN = 3
  CONTAINER_TYPE="docker"
  print(batcherFilename)
  batcherFile <- read.csv(batcherFilename, as.is = T)
  numRuns <- nrow(batcherFile)

  folders <- batcherFile$OutputDir

  notFinished <- TRUE
  
  logFileSuffix <- strsplit(batcherFilename,".",fixed=TRUE)[[1]][1]
  logFileSuffix_split <- strsplit(logFileSuffix,"/")[[1]]
  logFileSuffix <- logFileSuffix_split[[length(logFileSuffix_split)]]
  logfileName <- paste0("log_",logFileSuffix)
  columns_logfile <- c("Run_name", "Start_time", "End_time", "Status")
  logData <- data.frame(matrix(nrow = numRuns, ncol = length(columns_logfile)))
  colnames(logData) = columns_logfile
  
  activeRunDirs <- folders[1:NUM_TO_RUN]
  numActiveRuns <- length(activeRunDirs)
  containerNameVector <- vector(mode="character")

  for (n in 1:NUM_TO_RUN) {
    param.dir <- paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/")
    output.dir <- paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs")
#    param.dir <- paste0(here(),"/Rob_Project_Template/Project_Name_Version/")
#    output.dir <- paste0(here(),"/Rob_Project_Template/Project_Name_Version/Atlantis_Runs")
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
    containerNameVector <- append(containerNameVector, containerName)
    print(containerName)
    print(param.dir)
    print(folders[1])
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
    
    Sys.sleep(90)  
  }
  
  print(paste0("n = ",n))
  while (notFinished) {
    Sys.sleep(CHECK_TIME_INTERVAL)
    container_ps_output <- system(paste0(CONTAINER_TYPE, " ps"), intern = TRUE)
    numContainers <- length(container_ps_output)
    activeContainers <- trimws(vector(mode="character"))
    for (c in 2:numContainers) {
      activeContainerInfo <- unlist(strsplit(container_ps_output[c], " "))
      activeContainerInfoLength <- length(activeContainerInfo)
      activeContainer <- activeContainerInfo[activeContainerInfoLength]
      activeContainers <- append(activeContainers, activeContainer)
    }
    
    numActiveRuns <- length(containerNameVector)
    i <- numActiveRuns
    for (j in 1:numActiveRuns) {
      if (!(containerNameVector[i] %in% activeContainers)) {
        for (bf_index in 1:numRuns ) {
          if (grepl(batcherFile$Run[bf_index], containerNameVector[i]) == TRUE) {
            logData$Status[bf_index] <- "Completed"
            logData$End_time[bf_index] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
            batcherFile$Status[bf_index] <- "Completed"
            try(write.csv(logData,paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/",logfileName), row.names = FALSE, append = FALSE))
            try(write.csv(batcherFile,batcherFilename, row.names = FALSE, append = FALSE))
          }
        }
        # If more runs, add in next run
        print(paste0("FINISHED: ", activeRunDirs[i]))
        activeRunDirs <- activeRunDirs[-i]
        containerNameVector <- containerNameVector[-i]
        numActiveRuns <- length(activeRunDirs)
        print(paste0("Active runs = ", length(activeRunDirs)))
        print(paste0("Num containers = ", numActiveRuns))
        if ((n < numRuns) && (numActiveRuns < NUM_TO_RUN)) {
          notFinished <- TRUE
          n <- n + 1
          print(paste0("n = ", n))
          param.dir <- "/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/"
          output.dir <-"/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/"
#          param.dir <- paste0("/net/work3/atlantis/Rob_Project_Template/Project_Name_Version/")
#          output.dir <- paste0(here(),"/net/work3/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/")
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
          containerNameVector <- append(containerNameVector, containerName)
          print(containerName)
          print(param.dir)
          print(folders[1])
          outputFolder <- paste0('"',output.dir,folders[n],'"')
          run <- paste0(CONTAINER_TYPE," run -d --name ",  containerName, " --rm --mount \"type=bind,src=",param.dir,",dst=/app/model\" --mount \"type=bind,src=",outputFolder,"/,","dst=/app/model/output/\" atlantis_6536")
          
          system(run)
          logData$Run_name[n] <- batcherFile$Run[n]
          logData$Start_time[n] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
          logData$End_time[n] < "Unfinished"
          logData$Status[n] <- "Started"
          batcherFile$Status[n] <- "Started"
          try(write.csv(logData,paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/",logfileName), row.names = FALSE, append = FALSE))
          try(write.csv(batcherFile,batcherFilename, row.names = FALSE, append = FALSE))
          
          Sys.sleep(90)  
          activeRunDirs <- append(activeRunDirs, folders[n])  
          containerNameVector <- append(containerNameVector, containerName)
          try(write.csv(logData,paste0("/net/work3/EDAB/atlantis/Rob_Project_Template/Project_Name_Version/Atlantis_Runs/",logfileName), row.names = FALSE, append = FALSE))
        } else if ( n >= numRuns) {
          notFinished <- FALSE
          print(notFinished)
        } else {
          notFinished <- TRUE
        }
      } else {
        notFinished <- TRUE
      }
      i <- i - 1
    }  
  }
}

