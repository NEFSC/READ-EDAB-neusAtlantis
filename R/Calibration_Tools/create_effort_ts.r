#' Create the forced Effort time series file
#'
#' Creates the Effort ts file (in total_effort.ts)
#'
#' @param outFile Character. The name of the output file (without the extension)
#' @param nYrs numeric. The number of years (on a daily time step) to create effort
#' 
#' @Section Info
#' 
#' The effort series catch file resides in the curentVersion/CatchFiles folder. 
#' The output file will be created in the same location
#' 
#' Currently the value is a global value applied to all species codes
#'

create_effort_ts <- function(filename="total_effort", nYrs=57) {
  
  # input and output files
  #catchFile <- here::here("currentVersion/CatchFiles/total_effort.ts")
  outFile <- here::here(paste0("currentVersion/catchFiles/",filename,".ts"))
  copyFile <- here::here(paste0("currentVersion/catchFiles/",filename,".tstemp"))
  
  if(file.exists(outFile)) {
    file.rename(outFile,copyFile)
    message(paste0("Your previous version of catch ",filename,".ts has been renamed to ",filename,".tstemp"))
  }
  
  # read in fisheries.csv file 
  fisheries <- readr::read_csv(here::here("currentVersion/neus_fisheries.csv"),show_col_types=F)
  
  numFisheries <- length(fisheries$Code)
  con <- file(outFile,open="a")
  
  writeLines("# Effort to Reproduce Historical Catch 1980s-Present",con)
  writeLines("# ",con)
  writeLines(paste0("## COLUMNS ",numFisheries+1),con)
  writeLines("## ",con)
  writeLines("## COLUMN1.name Time",con)
  writeLines("## COLUMN1.long_name Time",con)
  writeLines("## COLUMN1.units days since 1964-01-01 00:00:00 10  ",con)
  writeLines("## COLUMN1.missing_value 0",con)
  writeLines("## ",con)
  
  ic <- 1
  for (afleet in fisheries$Code) {
    ic <- ic + 1
    writeLines(paste0("## COLUMN",ic,".name ",afleet),con)
    writeLines(paste0("## COLUMN",ic,".long_name ",afleet),con)
    writeLines(paste0("## COLUMN",ic,".units days day-1"),con)
    writeLines(paste0("## COLUMN",ic,".missing_value 0"),con)
    writeLines("## ",con)
  }
  
  # now add data. just add a matrix of zeros
  t <- data.frame(t = 1:(365*nYrs))
  d <- as.data.frame(matrix(data=0,nrow=nrow(t),ncol=numFisheries))
  abody <- cbind(t,d)
  
  write.table(abody, file = con, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )
  
  close(con)

}