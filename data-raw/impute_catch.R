#' Impute stocksmart value
#'
#' Fill in missing data from ends of time series either by taking mean of first/last few years
#' or by scaling up comlandr (Assumes trend is similar over time
#'
#' @param YEAR Numeric vector. 
#' @param value Numeric vector. 
#' @param value Character. 
#' @param Catch_Source Character.
#' @param Imputation_Type Character. How to impute "scalar" or "mean"
#' @param nyrs Numeric scalar. window of running mean centered at target, with weights 1/nyrs
#'
#' @return vector
#' \item{value}{imputed value for Code/Year combo based on imputation type}
#'
#' @section Notes: used as a function in create_neus_catch_data.r
#'
#' @noRd

impute_catch <- function(Code,YEAR,comlandr,stocksmart,Catch_Source,Imputation_Type,nyrs) {
  
  if(all(Imputation_Type == "")) {
    # Do nothing and return original value
    newValue <-  stocksmart
    
  } else if (all(Imputation_Type == "scalar")) {
    # need to find the scale in which to shift comlandr to stocksmart.
    # Assumes that trends are same but offset by a scalar
    scalar <- mean(stocksmart-comlandr,na.rm =T)
    comlandr[is.na(comlandr)] <- 0 # set all nas to zero
    newss <- comlandr+scalar
    dataAvail <- is.na(stocksmart) # find na's that need to be replaced
    stocksmart[is.na(stocksmart)] <- 0
    newValue <- stocksmart + dataAvail*newss

  } else if  (all(Imputation_Type == "mean")) {
    # take the mean of first or last few years of stocksmart data
    dataAvail <- is.na(stocksmart)
    missing <- which(dataAvail) # find which are missing
    
    #nyrs window of running mean centered at target, with weights 1/n
    #running mean
    newV <- stats::filter(stocksmart,rep(1/nyrs,nyrs),sides = 2)
    # select the running mean for the two missing missing entries
    vals <- newV[range(which(!is.na(newV)))]
    
    # assign these values to the missing values at either the start or end of the timeseries
    # for time series with missing entries at both start and end need to find the index that 
    # separates the start and end of missing data
    cut <- which(diff(missing) != 1)
    stocksmart[is.na(stocksmart)] <- 0
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
    newV <- stocksmart + bot + top

    
    newValue <- newV
  }
    
    
    
    
  
  
  
  return(newValue)
  # 
  # alpha <- exp(as.double(lengthWeightParams$logAlpha))
  # beta <- as.double(lengthWeightParams$betas)
  # sigma2 <- as.double(lengthWeightParams$var)
  # 
  # # vector of total weight for the number of fish (in the sample) of a given length
  # fishWeight <- (alpha*fishLength^beta)*exp(sigma2/2)*numAtLength
  # 
  # #mean Sample Weight
  # sampleWeight <- sum(fishWeight)
  # # proportion of total landed weight
  # expansionFactor <- landings/sampleWeight
  # 
  # # scaled weight to landings total.
  # fishWeight <- fishWeight * expansionFactor
  # 
  # # if length samples are missing, just assign the landings as the weight.
  # if(anyNA(fishWeight)) {
  #   fishWeight <- landings
  # }
  # 
  # return(fishWeight)
}
