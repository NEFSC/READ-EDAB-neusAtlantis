#' Get min_move_temp & max_move_temp values from a bio.prm file
#' 
#' @description
#' The values are the minimum and maximum temperatures that a group can tolerate
#' XXX_min_move_temp, XXX_max_move_temp
#' 
#' @params bio.prm path to the bio.prm file
#' 
#' @return a data frame with the group, value and limit (max/min)

get_param_move_temp = function(bio.prm){
  
  # read n parameter file
  bio.lines = readLines(bio.prm)
  # identify lines that contain the min_move_temp and max_move_temp values
  line = grep(paste0("_move_temp"),bio.lines)
  # extract the lines
  lines <- bio.lines[line]
  
  
  # create a null dataframe
  out.df = data.frame(group = NULL,value = NULL,limit = NULL)
  # loop through the lines and extract the values, groups and the min/max
  for (i in 1:length(lines)){
    linesi <- gsub("_move_temp","",lines[i])
    splitline <- strsplit(linesi,"\\s+") 
    value <- as.numeric(splitline[[1]][2])
    grouplimit <- splitline[[1]][1]
    if(grepl("_min",grouplimit)) {
      lim <- "min"
    } else if(grepl("_max",grouplimit)) {
      lim <- "max"
    }
    group <- gsub("_min|_max","",grouplimit)
    out.df <- rbind(out.df,data.frame(group = group,value = value,limit = lim))
  }

  out.df <- tibble::as_tibble(out.df)
  # return the data frame
  return(out.df)
}