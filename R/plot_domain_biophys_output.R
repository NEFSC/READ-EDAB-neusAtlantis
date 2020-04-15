#'Creates timeseries plots of any domain-wide variable in main output.nc file
#'
#'@description Specifies a given variable and creates a box-level timeseries of a 
#'specified variable found in the main output.nc file. This is mainly used for 
#'diagnostics/calibration, and allows for a more refined look into specific variables
#'than other aggregative plots.
#'
#'@nc.file string. path to output.nc file
#'@variable.name string. name of variable found in output.nc
#'@plot.dir string. path where output to be saved
#'@plot.name string. name of output plot
#'
#'@return pdf with each page showing timeseries of specified variable for each level within a box. 
#'
#'Author: J. Caracappa
#'

# nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/neus_output_test.nc'
# variable.name = 'NH3'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/Figures/geophysical_output/'
# plot.name = 'NH3_timeseries'

plot.domain.biophys = function(nc.file, variable.name, plot.dir, plot.name){
  
  output.nc = ncdf4::nc_open(nc.file)
  var.data = ncdf4::ncvar_get(output.nc,variable.name)
  var.plotname = ncdf4::ncatt_get(output.nc,variable.name)$long_name
  var.units = ncdf4::ncatt_get(output.nc,variable.name)$units
  var.time = ncdf4::ncvar_get(output.nc,'t')
  t.start = strsplit(ncdf4::ncatt_get(output.nc,'t')$units,' ')[[1]][3]
  var.time = as.POSIXct(var.time, origin = paste0(t.start,' 00:00:00'),tz = 'UTC')
  ncdf4::nc_close(output.nc)
  
  # boxes = 0:(dim(var.data)[2] -1) 
  levels = 1:dim(var.data)[1]
  
  var.domain = as.data.frame(t(apply(var.data,c(1,3),sum,na.rm=T)))
  colnames(var.domain) = paste0('L',levels)
  var.domain$time = var.time
  var.domain.lev = reshape2::melt(var.domain, id.vars = 'time')
  
  ggplot2::ggplot(data = var.domain.lev,ggplot2::aes(x=time, y=value))+
    ggplot2::geom_line(col='red3')+
    ggplot2::facet_wrap(~variable,nrow = 5)+
    ggplot2::xlab('Date')+
    ggplot2::ylab(paste0(var.plotname,' (',var.units,')'))+
    ggplot2::theme_classic()+
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )+
    ggplot2::ggsave(paste0(plot.dir,plot.name,'_domain.png'),width = 14, height = 8, units = 'in', dpi = 350)

}
