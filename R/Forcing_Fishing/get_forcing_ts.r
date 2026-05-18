#' pulls time series for groups of species
#'
#' Reads time series from ts files of an individual species/fleet
#'
#' @param file_path character. The full or relative path to the .ts file to read.
#' @param code character vector. The functional group code of the species or the fleet code to read (Default = NULL, all codes)
#' @param time character.  (Default = "annual", "weekly", "daily")
#' 
#' @examples
#' # example code
#' 

get_forcing_ts <- function(file_path, code=NULL, time="daily") {
  
  message(paste0("Getting info for: ", basename(file_path)))
  
  # Ensure time is lowercase
  time = tolower(time)
  
  # read in input file directly from the provided path
  content <- readLines(con = file_path)
  
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
  
  # check to make sure codes is valid
  if(!is.null(code)) {
    if(!all(code %in% df$Code) ){
      message(paste0("Valid codes are: ",paste0(tail(df$Code,-1),collapse = ",")))
      stop(paste0(code," is not a valid code for the ", basename(file_path), " file"))
    }
  }
  
  # separate header from data
  header <- content[which(grepl("^#",content))]
  body <- content[which(grepl("^[^#]",content))] 
  # convert body to a numeric data frame
  body <- as.data.frame(stringr::str_split(body,"\\s+",simplify = T))
  abody <- sapply(body,as.numeric,simplify = T)
  
  if (is.null(code)) {
    colnames(abody) <- df$Code
    ts <- as.data.frame(abody)
  } else { 
    # identify columns to retrieve
    index <- df$Column[which(df$Code %in% code)]
    # pull out selected species
    colnames(abody) <- df$Code
    ts <- as.data.frame(abody[,c(1,index)])
  }
  
  # convert to long format for aggregation
  ts <- ts |> 
    tidyr::pivot_longer(cols=-Time,names_to = "Variable",values_to = "Value")
  
  if (time == "annual") {
    ts <- ts |>
      dplyr::mutate(Time = 1+ floor(Time/365)) |>
      dplyr::group_by(Time,Variable) |>
      dplyr::summarise(Value = sum(Value)/365,
                       .groups="drop")
  } else if (time == "weekly") {
    ts <- ts |>dplyr::mutate(Time = 1+ floor(Time/7)) |>
      dplyr::group_by(Time,Variable) |>
      dplyr::summarise(Value = sum(Value)/7,
                       .groups="drop")
  } else {
    # Keep as daily (or original time step) if neither annual nor weekly
  }
  
  return(ts)
  
}