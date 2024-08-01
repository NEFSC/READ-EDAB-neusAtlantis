#' Manipulate a forced catch/effort time series
#'
#' Manipulates time series (in total_catch.ts or total_effort) of an individual species/fleet
#'
#' @param code character vector. The functional group code of the species or the fleet code to scale
#' @param tstype character. Make the change to the catch or the effort time series (Default = "effort")
#' @param tseries numeric. The time series to use to replace esisting
#' @param filename character. The name of the output file (without the extension) (Default = "temp")
#' @param keep boolean. keep (T) previous filename or just overwrite (F). (Default = T)
#' 
#' @Section Info
#' 
#' The time series file of forced catch and forced effort reside in the curentVersion/CatchFiles folder. 
#' The output file will be created in the same location. If the output file is already present it will be
#' copied in case the change was made in error
#' 
#' Currently the scalar is a global scalar applied to all species/fleet codes
#' @examples
#' # example code
#' 

replace_forcing_ts <- function(code,tstype="effort",tseries,filename="temp",keep=T) {

  # input and output files
  if(tstype == "effort") {
    file <- here::here(paste0("currentVersion/CatchFiles/",filename,".ts"))
  } else if (tstype=="catch") {
    file <- here::here("currentVersion/CatchFiles/total_catch.ts")
  } else {
    stop("tsfile must be either 'catch' or 'effort'")
  }
  
  outFile <- here::here(paste0("currentVersion/CatchFiles/",filename,".ts"))
  copyFile <- here::here(paste0("currentVersion/CatchFiles/temp/",filename,".tstemp"))
  
  # read in input file
  content <- readLines(con = file)
  
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
  
  # check to make sure code is valid
  if(!(code %in% df$Code) ){
    stop(paste0(code," is not a valid code for ",tstype," file"))
  }

  # separate header from data
  header <- content[which(grepl("^#",content))]
  body <- content[which(grepl("^[^#]",content))] 
  # convert body to a numeric data frame
  body <- as.data.frame(stringr::str_split(body,"\\s+",simplify = T))
  abody <- sapply(body,as.numeric,simplify = T)
  # identify columns to manipulate
  index <- df$Column[which(df$Code %in% code)]
  
  # replace time series
  abody[,index] <- tseries

  
  if(file.exists(outFile) & (keep)) {
    file.rename(outFile,copyFile)
    message(paste0("Your previous version of catch ",filename,".ts has been renamed to temp/",filename,".tstemp"))
  }
  

  # write new file
  writeLines(header,con=outFile)
  write.table(abody, file = outFile, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )
  
  return(abody)
}