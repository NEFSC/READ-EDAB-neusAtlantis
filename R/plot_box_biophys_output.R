#'Creates timeseries plots of any box-level variable in main output.nc file
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

plot.box.biophys = function(nc.file, variable.name, plot.dir, plot.name){
  
  output.nc = ncdf4::nc_open(nc.file)
  var.data = ncdf4::ncvar_get(output.nc,variable.name)
  var.plotname = ncdf4::ncatt_get(output.nc,variable.name)$long_name
  var.units = ncdf4::ncatt_get(output.nc,variable.name)$units
  var.time = ncdf4::ncvar_get(output.nc,'t')
  t.start = strsplit(ncdf4::ncatt_get(output.nc,'t')$units,' ')[[1]][3]
  var.time = as.POSIXct(var.time, origin = paste0(t.start,' 00:00:00'),tz = 'UTC')
  ncdf4::nc_close(output.nc)
  
  boxes = 0:(dim(var.data)[2] -1) 
  levels = 1:dim(var.data)[1]
  
  plot.ls = list()
  for(b in 1:length(boxes)){
    
    var.box = as.data.frame(t(var.data[,b,]))
    colnames(var.box) = paste0('L',levels)
    var.box$time = var.time
    var.box.lev = reshape2::melt(var.box,id.vars = 'time')
    
    plot.ls[[b]] = ggplot2::ggplot(data = var.box.lev,ggplot2::aes(x=time, y=value))+
      ggplot2::geom_line(col='red3')+
      ggplot2::facet_wrap(~variable,nrow = 5)+
      ggplot2::xlab('Date')+
      ggplot2::ylab(paste0(var.plotname,' (',var.units,')'))+
      ggplot2::ggtitle(paste0('Box ',b-1))+
      ggplot2::theme_classic()+
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        panel.grid = ggplot2::element_blank()
      )
  }
  
  pdf(paste0(plot.dir,plot.name,'.pdf'),width = 14, height = 8, onefile = T)
  for(i in 1:length(plot.ls)){ gridExtra::grid.arrange(plot.ls[[i]])}
  dev.off()
  
}
