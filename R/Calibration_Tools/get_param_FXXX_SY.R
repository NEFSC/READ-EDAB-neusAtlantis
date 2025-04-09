#' Get adult distribution (FXXX_SY) values from a bio.prm file
#' 
#' @description
#' For each of the 4 seasons get the values (proportion of the population)
#' that are distributed among polygons. 
#' 
#' @params bio.prm path to the bio.prm file
#' 
#' @return a data frame with the group, value and polygons, season, cohort

get_param_FXXX_SY = function(bio.prm){
  
  # read n parameter file
  bio.lines <-  readLines(bio.prm)
  # identify lines that contain the min_move_temp and max_move_temp values
  line <-  grep("^F[A-Za-z]+_S[0-9]",bio.lines)
  # extract the lines
  lines <- bio.lines[line]
  values <- bio.lines[line+1]
  values <- gsub("\t"," ",values)
  
  # create a null dataframe
  out.df = data.frame(group = NULL,season = NULL,cohort = NULL,polygon=NULL, value=NULL)
  # loop through the lines and extract the values, groups and the min/max
  for (i in 1:length(lines)){
    # remove trailing part of the line where the number of values on next line is listed
    linei <- gsub("\\s+[0-9]+","",lines[i])
    # check to see if string contains "juv"
    if(grepl("juv",linei)) {
      cohorti <- "juv"
      # remove "juv" from linei
      linei <- gsub("juv","",linei)
    } else {
      cohorti <- "adult"
    }
    
    # remove the first charactor in linei
    linei <- substr(linei,2,nchar(linei))
    # split the line by the underscore + S to get the group and the value
    # flatten list to vector
    groupi <- unlist(strsplit(linei,"_S"))[1]
    seasoni <- unlist(strsplit(linei,"_S"))[2]
    # parse the value object to get the value 
    valuei <- as.numeric(unlist(strsplit(values[i]," ")))
   
    polygoni <- data.frame(polygon = c(0:(length(valuei)-1)),value = valuei)
    df <- data.frame(group = groupi,season = seasoni, cohort = cohorti)
    
    # replicate rows of df to match the number of polygons. omit row names
    df <- df[rep(seq_len(nrow(df)), each = nrow(polygoni)),]
    rownames(df) <- NULL
    
    out.df <- rbind(out.df,cbind(df,polygoni))
    
  }
  out.df <- tibble::as_tibble(out.df)
  
  # return the data frame
  return(out.df)
}