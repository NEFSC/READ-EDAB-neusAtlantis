#' Get horizontal distribution recruitment values from a bio.prm file
#' 
#' @description
#' The values are proportion of recruits and how they are distributed among
#' the polygons. XXX_rectuit_hdistrib
#' 
#' @params bio.prm path to the bio.prm file
#' 
#' @return a data frame with the group, value and polygon

get_param_recruit_hdistrib = function(bio.prm){
  
  # read n parameter file
  bio.lines <-  readLines(bio.prm)
  # identify lines that contain "_recruit_hdistrib"
  line <-  grep("_recruit_hdistrib",bio.lines)
  # extract the lines
  lines <- bio.lines[line]
  values <- bio.lines[line+1]
  values <- gsub("\t"," ",values)
  
  # create a null dataframe
  out.df = data.frame(group = NULL, polygon=NULL, value=NULL)
  # loop through the lines and extract the values, groups and the min/max
  for (i in 1:length(lines)){
    # remove trailing part of the line where the number of values on next line is listed
    linei <- gsub("\\s+[0-9]+","",lines[i])
    # replace "_recruit_hdistrib" with ""
    groupi <- gsub("_recruit_hdistrib","",linei)

    # parse the value object to get the value 
    valuei <- as.numeric(unlist(strsplit(values[i]," ")))
    # create the data frame
    df <- data.frame(group = groupi, polygon = c(0:(length(valuei)-1)),value = valuei)
    # concatenate the data frame
    out.df <- rbind(out.df,df)
  }
  
  # return the data frame
  return(out.df)
}