#' Impute stocksmart value
#'
#' Fill in missing data from ends of time series either by taking mean of first/last few years
#' or by scaling up comlandr (Assumes trend is similar over time
#'
#'@param stocksmart
#'@param comlandr
#'@param newvalue
#'@param nafo
#' @param Catch_Source Character.
#'
#' @return vector
#' \item{value}{sum of comlandr+nfo or stocksmart}
#'
#' @noRd

select_source <- function(stocksmart,comlandr,newstocksmart,nafo,Catch_Source) {
  
  if (Catch_Source == "stocksmart") {
    value = newstocksmart
  } else {
    if (is.na(comlandr) & is.na(nafo)) {
      value <- NA
    } else {
      value <- sum(comlandr,nafo,na.rm = T)
    }
  }

  return(value)
 
}
