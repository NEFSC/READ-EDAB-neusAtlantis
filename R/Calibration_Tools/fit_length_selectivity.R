# Function to fit logistic selectivity curves
#Adopted from Communities at Sea Repo https://github.com/NOAA-EDAB/atlantiscas/blob/main/data-raw/fit_selectivity.R

#data should be a 2 column dataframe(Code,LENGTH)
fit_length_selectivity = function(data,Code){
 
  #Form of logistic function
  
  logisticF <- function(x,dataFrame) {
    
    lsm <- x[1]
    selb <- x[2]
    lengthData <- dataFrame$LENGTH
    prob <- dataFrame$cump
    
    numer <- 1
    denom <- 1 + exp(-selb*(lengthData-lsm))
    
    f <- numer/denom
    
    rss <- sum((prob-f)^2)
    
    return(rss)
    
  }
  
  fit.df = data.frame(Code = Code,lsm = NA,b = NA)
  
  for(i in 1:length(Code)){
    #Data formatting
    spp.len <- data |>
      filter(Code == Code[i]) |>
      dplyr::select(Code,LENGTH) |>
      dplyr::arrange(Code,LENGTH) |>
      dplyr::group_by(Code) |>
      dplyr::mutate(prob = (1:dplyr::n())/dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::select(LENGTH) |>
      dplyr::group_by(LENGTH) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::mutate(p = n/sum(n),
                    cump=cumsum(p))
    
    x0 <- c(30,0.1)
    opts <- list("algorithm"="NLOPT_LN_COBYLA",
                 "xtol_rel"=1.0e-04)
    x <- nloptr::nloptr(x0=x0,
                        eval_f=logisticF,
                        lb=c(0,-Inf),
                        ub=c(Inf,Inf),
                        opts = opts,
                        dataFrame=spp.len)
    
    fitted <- 1/(1 + exp(-x$solution[2]*(spp.len$LENGTH - x$solution[1])))
    
    fit.df$lsm[i] = x$solution[1]
    fit.df$b[i] = x$solution[2]
    
  }
  
  return(fit.df)
}
