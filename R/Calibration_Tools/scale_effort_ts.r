#' Change a species forced Effort time
#'
#' Changes time series (in total_effort.ts) of an individual species
#'
#' @param speciesCode Character vector. The functional group code of the species to scale
#' @param value Numeric. The value you want to addto the existing time series (Default = 1)
#' @param outFile Character. The name of the output file (without the extension)
#' 
#' @Section Info
#' 
#' The effort series catch file resides in the curentVersion/CatchFiles folder. 
#' The output file will be created in the same location
#' 
#' Currently the value is a global value applied to all species codes
#'

scale_effort_ts <- function(speciesCode,value=1,outFile) {
  
  # input and utput files
  catchFile <- here::here("currentVersion/CatchFiles/total_effort.ts")
  outFile <- here::here(paste0("currentVersion/catchFiles/",outFile,".ts"))
  
  # read in input file
  content <- readLines(con = catchFile)
  
  # find species names and the column in file they represent
  df <- NULL
  for (iline in 1:length(content)) {
    line <- trimws(content[iline])
    if(grepl("\\.name",line)) {
      # parse column number and species name
      column <- stringr::str_match(line,"COLUMN[0-9]+")
      column <- stringr::str_match(column,"[0-9]+")
      species <- tail(unlist(stringr::str_split(line,"\\s+")),1)
      df <- rbind(df,c(column,species))
    }
  }
  colnames(df) <- c("Column","Code")
  df <- as.data.frame(df) |> 
    dplyr::mutate(Column = as.integer(Column))
  
  # separate header from data
  header <- content[which(grepl("^#",content))]
  body <- content[which(grepl("^[^#]",content))] 
  # convert body to a numeric data frame
  body <- as.data.frame(stringr::str_split(body,"\\s+",simplify = T))
  abody <- sapply(body,as.numeric,simplify = T)
  # multiply a column by a scalar
  index <- df$Column[which(df$Code %in% speciesCode)]
  abody[,index] <- abody[,index]+scalar
  
  # write new file
  writeLines(header,con=outFile)
  write.table(abody, file = outFile, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )
}