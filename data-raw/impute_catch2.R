#' Impute comlandr values
#'
#' Fill in missing data from ends of time series  by taking mean of first/last few years
#' 
#'
#' @param Code Numeric vector. 
#' @param Year Numeric vector. 
#' @param data
#' @param Imputation_Type Character. How to impute "scalar" or "mean"
#' @param nyrs Numeric scalar. window of running mean centered at target, with weights 1/nyrs
#'
#' @return vector
#' \item{value}{imputed value for Code/Year combo based on imputation type}
#'
#' @section Notes: used as a function in create_neus_catch_data.r
#'
#' @noRd

impute_catch2 <- function(Code,YEAR,data,Imputation_Type,nyrs) {
  
  if(all(is.na(data)) ) {
    newValue <- data
  } else if (any(is.na(data))) {
  
   if  (all(Imputation_Type == "mean")) {
      # take the mean of first or last few years of stocksmart data
      dataAvail <- is.na(data)
      missing <- which(dataAvail) # find which are missing
      
      #nyrs window of running mean centered at target, with weights 1/n
      #running mean

      newV <- stats::filter(data,rep(1/nyrs,nyrs),sides = 2)
      # select the running mean for the two missing missing entries
      vals <- newV[range(which(!is.na(newV)))]
      
      # assign these values to the missing values at either the start or end of the timeseries
      # for time series with missing entries at both start and end need to find the index that 
      # separates the start and end of missing data
      cut <- which(diff(missing) != 1)
      data[is.na(data)] <- 0
      bot <- rep(0,length(YEAR))
      top <- rep(0,length(YEAR))
  
      # assign mean value    
      if (length(cut)>0){ # missing data both start and end of time series
        bot[1:cut] <- vals[1]
        top[missing[cut+1]:length(YEAR)] <- vals[2]
      } else { # missing at either start or end
        if (min(missing) == 1) { # at start
          bot[missing] <- vals[1]
        } else { # at end
          top[missing] <- vals[2]
        }
      }
      newV <- data + bot + top
  
      newValue <- newV
    }
  } else {
    newValue = data
  }

  
  return(newValue)
 
}
