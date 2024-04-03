#' Manipulate a forced catch/effort time series
#'
#' Manipulates time series (in total_catch.ts or total_effort) of an individual species/fleet
#'
#' @param code character vector. The functional group code of the species or the fleet code to scale
#' @param tstype character. Make the change to the catch or the effort time series (Default = "effort")
#' @param value numeric. The value by which you want to apply to the time series. How it is applies is determined by the operation argument (Default = 1)
#' @param operation character. "add" or "multiply" the value to the time series (Default = "multiply") or "create" using value = 1 and code values
#' @param filename character. The name of the output file (without the extension) (Default = "temp")
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

scale_forcing_ts <- function(code,tstype="effort",value=1,operation="multiply",filename="temp") {

  # input and output files
  if(tstype == "effort") {
    file <- here::here("currentVersion/CatchFiles/total_effort.ts")
  } else if (tstype=="catch") {
    file <- here::here("currentVersion/CatchFiles/total_catch.ts")
  } else {
    stop("tsfile must be either 'catch' or 'effort'")
  }
  
  outFile <- here::here(paste0("currentVersion/CatchFiles/",filename,".ts"))
  copyFile <- here::here(paste0("currentVersion/CatchFiles/",filename,".tstemp"))
  
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
  # identify columns to scale
  index <- df$Column[which(df$Code %in% code)]
  
  if(operation == "multiply") {
    abody[,index] <- abody[,index]*value
  } else if(operation == "add") {
    abody[,index] <- abody[,index]+value
  } else if(operation == "create"){
    # create only using the fleets/species defined with value = value
    abody <- cbind(abody[,1],(abody[,index]*0) + value)
    # parse header to pick out  Time and code sections
    headerNew <- header[1:2]
    headerNew[3] <- paste0("## COLUMNS ",length(code)+1)
    headerNew[4:8] <- header[4:8]
    ic <- 8
    for (icode in 1:length(code)) {
      ic <- ic + 1
      headerNew[ic] <- "##"
      acode <- code[icode]
      for (aline in header) {
        if(grepl(paste0("COLUMN",index[icode],"."),aline)){
          ic <- ic + 1
          newaline <- sub(paste0("COLUMN",index[icode],"."),paste0("COLUMN",icode+1,"."),aline)
          headerNew[ic] <- newaline
        }
      }
    }
    headerNew[ic+1] <- "##"
    header <- headerNew
    
  } else {
    stop("operation must be either 'add', 'multiply', or 'create'")
  }
  
  
  if(file.exists(outFile)) {
    file.rename(outFile,copyFile)
    message(paste0("Your previous version of catch ",filename,".ts has been renamed to ",filename,".tstemp"))
  }
  

  # write new file
  writeLines(header,con=outFile)
  write.table(abody, file = outFile, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )
}