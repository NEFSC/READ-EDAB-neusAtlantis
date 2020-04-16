#' Converts atlantis variable to long (i.e. flat) format
#' 
#' Takes specified variable from atlantis output.nc file and converts
#' it to flat format with spatio-temporal structure (i.e. time, box, level).
#' Can be useful for plotting, table, and analysis.
#' 
#' @output.file string. full path of output file
#' @var.name string. nc file parameter name
#' @origin string. Reference date (start date) for model "%Y-%m-%d'
#' 
#' @return dataframe 
#' 
#' Author: J. Caracappa
#' 

atl_var_to_longform = function(output.file, var.name,origin){
  
  output.nc = ncdf4::nc_open(output.file)
  var.dat = ncdf4::ncvar_get(output.nc, var.name)  
  
  var.dim = dim(var.dat)
  boxes = 0:(var.dim[2]-1)
  levels = 1:var.dim[1]
  
  time = output.nc$dim$t$vals
  time.date = as.POSIXct(time, origin = origin, tz = 'UTC')
  
  var.ls = lapply(1:length(time.date),function(x){
    dat = as.data.frame(var.dat[,,x])
    # colnames(dat) = paste0('B',boxes)
    colnames(dat) = as.character(boxes)
    dat$lev = levels
    dat$time = time[x]
    dat$time.date = time.date[x]
    dat.long =reshape2::melt(dat,id.vars = c('time','time.date','lev'))
    return(dat.long)
    })
  
  var.long = dplyr::bind_rows(var.ls)
  colnames(var.long) = c('time','time.date','level','box','value')
  var.long$box = as.numeric(as.character(var.long$box))
  return(var.long)
}