#' pulls time series for groups of species
#'
#' Manipulates time series (in total_catch.ts or total_effort) of an individual species/fleet
#'
#' @param code character vector. The functional group code of the species or the fleet code to scale
#' @param tstype character. Make the change to the catch or the effort time series (Default = "effort")
#' @param time character.  (Default = "annual", "weekly", "daily")
#' 
#' @Section Info
#' 
#' The time series file of forced catch and forced effort reside in the curentVersion/CatchFiles folder. 
#' 
#' @examples
#' # example code
#' 

get_forcing_ts <- function(code,tstype="catch",time="daily") {

  # input and output files
  if(tstype == "effort") {
    file <- here::here("currentVersion/CatchFiles/total_effort.ts")
  } else if (tstype=="catch") {
    file <- here::here("currentVersion/CatchFiles/total_catch.ts")
  } else {
    stop("tsfile must be either 'catch' or 'effort'")
  }
  
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
  
  # check to make sure codes is valid
  if(!all(code %in% df$Code) ){
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
  
  # pull out selected species
  colnames(abody) <- df$Code
  ts <- as.data.frame(abody[,c(1,index)])
  
  # convert o long format for aggregation
  ts <- ts |> 
    tidyr::pivot_longer(cols=-Time,names_to = "Species",values_to = "Value")
  
  if (time == "annual") {
    ts <- ts |>
      dplyr::mutate(Time = 1+ floor(Time/365)) |>
      dplyr::group_by(Time,Species) |>
      dplyr::summarise(Value = sum(Value),
                       .groups="drop")
  } else if (time == "weekly") {
    ts <- ts |>dplyr::mutate(Time = 1+ floor(Time/7)) |>
      dplyr::group_by(Time,Species) |>
      dplyr::summarise(Value = sum(Value),
                       .groups="drop")
  } else {

  }
  
  return(ts)

}