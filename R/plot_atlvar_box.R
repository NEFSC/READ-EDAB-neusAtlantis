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

# nc.file ='C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/SatPhyto_Forcing_Fixed_Levels_3/neus_output.nc'
# variable.name = 'Diatom_N'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/SatPhyto_Forcing_Fixed_Levels_3/'
# dz.index = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/'
# plot.name = 'Diatom_N'

plot_atlvar_box = function(nc.file, variable.name, plot.dir, plot.name,remove.bot,remove.init){
  
  
  `%>%` = dplyr::`%>%`

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
  
  if(remove.init){
    var.data = var.data[,,-1]
    var.time = var.time[-1]
  }
  
  
  for(b in 1:length(boxes)){
    
    var.box = as.data.frame(t(var.data[,b,]))
    # colnames(var.box) = paste0('L',levels)
    colnames(var.box) = levels
    var.box$time = var.time
    var.box.lev = reshape2::melt(var.box,id.vars = 'time')
    colnames(var.box.lev) = c('date','atl.lev','value')
    var.box.lev$atl.lev = as.numeric(var.box.lev$atl.lev)
    var.box.lev$reg.lev = sapply(var.box.lev$atl.lev,function(x){
      if(x < 5){
        return(5-x)
      }else{
        return(x)
      }
    })
    
    var.box.lev$time.group = NA
    var.box.lev$time.group[which(var.box.lev$date < as.POSIXct('1979-01-01',tz = 'UTC'))] = 'early'
    var.box.lev$time.group[which(var.box.lev$date > as.POSIXct('1994-01-01',tz = 'UTC'))] = 'late'
    
    var.box.lev = var.box.lev[which(var.box.lev$value != 0),]
    
    if(remove.bot){
      var.box.lev = var.box.lev %>% dplyr::filter(reg.lev != 5)
    }
    
    
    box.level.means = na.omit(var.box.lev) %>% dplyr::group_by(reg.lev,time.group) %>%
      dplyr::summarize(value.mu = mean(value,na.rm=T))
    box.level.means$x = as.POSIXct('1964-01-01',tz = 'UTC')
    box.level.means$x[box.level.means$time.group=='late'] = as.POSIXct('1994-01-01',tz = 'UTC')
    box.level.means$xend = as.POSIXct('1979-01-01',tz = 'UTC')
    box.level.means$xend[box.level.means$time.group=='late'] = as.POSIXct('2014-12-12',tz = 'UTC')
    
    nlev = length(unique(var.box.lev$reg.lev))
    if(nlev == 0){
      plot.ls[[b]] = ggplot2::ggplot()+ggplot2::geom_blank()
    }else{
      plot.ls[[b]]  = ggplot2::ggplot(data = var.box.lev,ggplot2::aes(x=date, y=value))+
        ggplot2::geom_line(col='red3')+
        ggplot2::geom_segment(data = box.level.means, ggplot2::aes(x = x, xend = xend, y = value.mu,yend = value.mu, lty = time.group),color = 'black',size = 1.2)+
        ggplot2::xlab('Date')+
        ggplot2::ylab(paste0(var.plotname,' (',var.units,')'))+
        ggplot2::ggtitle(paste0('Box ',b-1))+
        ggplot2::theme_classic()+
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          panel.grid = ggplot2::element_blank()
        )+
        ggplot2::scale_linetype_discrete(name = 'Mean Value')+
        ggplot2::facet_wrap( ~reg.lev,nrow = nlev)
      }
  }
  
  pdf(paste0(plot.dir,plot.name,'.pdf'),width = 14, height = 8, onefile = T)
  for(i in 1:length(plot.ls)){ gridExtra::grid.arrange(plot.ls[[i]])}
  dev.off()
  
}


