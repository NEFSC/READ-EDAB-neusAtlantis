#' Plots timeseries of state variables over desired boxes
#' 
#' Creates timeseries plots of statevariables(temperature, salinity, vertical flux).
#' This is based on aggregated ROMS .nc file output. It also assumes that .nc files are 
#' formated to run in hydroconstract. 
#' 
#' @year.dir string. The directory of the output that you want to summarize
#' @statevars.file string. The path to the transport ncdf file
#' @which.box  numeric vector. Vector of faceid you want to plot. Default is all.
#' @which.levels numeric scalar. How many levels to plot (from 1-4) starting at surface. Default is all (4)
#' @plot.dir string. Directory where summary figures should go
#' 
#' @return Returns timeseries plots across entire duration of specified file
#' 
#' #' Created by J. Caracappa


#test
# year.dir = plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1983/'
# which.box = 0:29
# which.levels = 4

plot_roms_statevars = function(year.dir,statevars.file,which.box = 0:29,which.levels = 4, plot.dir){
  
  statevars.nc = ncdf4::nc_open(paste0(year.dir,statevars.file))
  salt = ncdf4::ncvar_get(statevars.nc,'salinity')
  temp = ncdf4::ncvar_get(statevars.nc,'temperature')
  vflux = ncdf4::ncvar_get(statevars.nc,'verticalflux')
  neus.time = ncdf4::ncvar_get(statevars.nc,'time')
  real.time = as.POSIXct(neus.time,origin = '1964-01-01',tz = 'GMT')
  
  salt.plots = temp.plots = vflux.plots = list()
  
  for(b.id in seq_along(which.box) ){
    plot.lab = paste0('Box',which.box[b.id])
    plot.cols = RColorBrewer::brewer.pal(which.levels,'Set1')
    
    #Format box salinity data
    DF.salt = as.data.frame(t(salt[1:which.levels,which.box[b.id] + 1,]))
    colnames(DF.salt) = paste0('lev',1:which.levels)
    DF.salt$time = real.time
    DF.salt = DF.salt[,c(ncol(DF.salt),1:(ncol(DF.salt)-1))]
    DF.salt = tidyr::gather(DF.salt,'neus.level','value',colnames(DF.salt)[2:ncol(DF.salt)])
    DF.salt = na.omit(DF.salt)
    
    #Format box temperature data
    DF.temp = as.data.frame(t(temp[1:which.levels,which.box[b.id] + 1,]))
    colnames(DF.temp) = paste0('lev',1:which.levels)
    DF.temp$time = real.time
    DF.temp = DF.temp[,c(ncol(DF.temp),1:(ncol(DF.temp)-1))]
    DF.temp = tidyr::gather(DF.temp,'neus.level','value',colnames(DF.temp)[2:ncol(DF.temp)])
    DF.temp = na.omit(DF.temp)
    
    #Format box vflux data
    DF.vflux = as.data.frame(t(vflux[1:which.levels,which.box[b.id] + 1,]))
    colnames(DF.vflux) = paste0('lev',1:which.levels)
    DF.vflux$time = real.time
    DF.vflux = DF.vflux[,c(ncol(DF.vflux),1:(ncol(DF.vflux)-1))]
    DF.vflux = tidyr::gather(DF.vflux,'neus.level','value',colnames(DF.vflux)[2:ncol(DF.vflux)])
    DF.vflux = na.omit(DF.vflux)
    
    salt.plots[[b.id]] =ggplot2::ggplot(data = DF.salt, ggplot2::aes(x = time,y = value, color = neus.level))+
      ggplot2::geom_path()+
      ggplot2::ylab(expression('Salinity (psu)'))+
      ggplot2::xlab('')+
      ggplot2::ggtitle(plot.lab)+
      ggplot2::scale_color_manual(values = plot.cols, labels = paste0('Level ',1:which.levels),name = 'Surface = 1')+
      ggplot2::theme_minimal()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
    
    temp.plots[[b.id]] =ggplot2::ggplot(data = DF.temp, ggplot2::aes(x = time,y = value, color = neus.level))+
      ggplot2::geom_path()+
      ggplot2::ylab(expression('Temperature ('~degree*'C )'))+
      ggplot2::xlab('')+
      ggplot2::ggtitle(plot.lab)+
      ggplot2::scale_color_manual(values = plot.cols, labels = paste0('Level ',1:which.levels),name = 'Surface = 1')+
      ggplot2::theme_minimal()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
    
    vflux.plots[[b.id]] =ggplot2::ggplot(data = DF.vflux, ggplot2::aes(x = time,y = value, color = neus.level))+
      ggplot2::geom_path()+
      ggplot2::ylab(expression('Vertical Flux ('~m^3~s^-1~')'))+
      ggplot2::xlab('')+
      ggplot2::ggtitle(plot.lab)+
      ggplot2::scale_color_manual(values = plot.cols, labels = paste0('Level ',1:which.levels),name = 'Surface = 1')+
      ggplot2::theme_minimal()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
  }
  
  pdf(paste0(plot.dir,'Salinity.pdf'),width = 14, height = 6)
  for(b.id in seq_along(which.box)){
    gridExtra::grid.arrange(salt.plots[[b.id]])
    print(b.id)
  }
  dev.off()
  
  pdf(paste0(plot.dir,'Temperature.pdf'),width = 14, height = 6)
  for(b.id in seq_along(which.box)){
    gridExtra::grid.arrange(temp.plots[[b.id]])
    print(b.id)
  }
  dev.off()
  
  pdf(paste0(plot.dir,'Vertical Flux.pdf'),width = 14, height = 6)
  for(b.id in seq_along(which.box)){
    gridExtra::grid.arrange(vflux.plots[[b.id]])
    print(b.id)
  }
  dev.off()
  
  ncdf4::nc_close(statevars.nc)
  
}


