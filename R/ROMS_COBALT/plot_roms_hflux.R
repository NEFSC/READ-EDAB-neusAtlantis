#' Plots timeseries of horizontal fluxes over desired faces
#' 
#' Creates timeseries plots of horizontal fluxes. This is based on aggregated
#' ROMS data .nc files. These are the input files for hydroconstruct. Presently
#' it is intended for yearly files, but will plot the entirety of the file duration.
#' 
#' @year.dir string. The directory of the output that you want to summarize
#' @hflux.file string. The path to the transport ncdf file
#' @which.face  numeric vector. Vector of faceid you want to plot. Default is all.
#' @which.levels numeric scalar. How many levels to plot (from 1-4) starting at surface. Default is all (4)
#' @plot.dir string. Directory where summary figures should go
#' 
#' @return Returns timeseries plots across entire duration of specified file
#' 
#' #' Created by J. Caracappa


#test
# year.dir = plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1980/'
# which.face = 0:150
# which.levels = 4

plot_roms_hflux = function(year.dir,hflux.file,which.face = 0:150,which.levels = 4, plot.dir,plot.year){
  
  year = 
  hflux.nc = ncdf4::nc_open(paste0(year.dir,hflux.file))
  dest_b = ncdf4::ncvar_get(hflux.nc,'dest_boxid')
  source_b = ncdf4::ncvar_get(hflux.nc,'source_boxid')
  transport = ncdf4::ncvar_get(hflux.nc,'transport')
  neus.time = ncdf4::ncvar_get(hflux.nc,'time')
  real.time = as.POSIXct(neus.time,origin = '1964-01-01',tz = 'GMT')
  hflux.plots = list()
  
  for(f.id in seq_along(which.face) ){
    plot.lab = paste0('Face',which.face[f.id],': From-Box',source_b[which.face[f.id]+1],' To-Box',dest_b[which.face[f.id]+1])
    plot.cols = RColorBrewer::brewer.pal(which.levels,'Set1')
    
    DF = as.data.frame(t(transport[1:which.levels,which.face[f.id]+1,]))
    colnames(DF) = paste0('lev',1:which.levels)
    DF$time = real.time
    DF = DF[,c(ncol(DF),1:(ncol(DF)-1))]
    DF = tidyr::gather(DF,'neus.level','value',colnames(DF)[2:ncol(DF)])
    DF = na.omit(DF)
    
    hflux.plots[[f.id]] =ggplot2::ggplot(data = DF, ggplot2::aes(x = time,y = value, color = neus.level))+
      ggplot2::geom_path()+
      ggplot2::ylab(expression('Transport ('~m^3~s^-1*')'))+
      ggplot2::xlab('')+
      ggplot2::ggtitle(plot.lab)+
      ggplot2::scale_color_manual(values = plot.cols, labels = paste0('Level ',1:which.levels),name = 'Surface = 1')+
      ggplot2::theme_minimal()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
  }
  
  pdf(paste0(plot.dir,'Horizontal Flux ',plot.year,'.pdf'),width = 14, height = 6)
  for(f.id in seq_along(which.face)){
    gridExtra::grid.arrange(hflux.plots[[f.id]])
    print(f.id)
  }
  dev.off()
  
  ncdf4::nc_close(hflux.nc)
}

# plot_hflux(year.dir,hflux.file,which.face,which.levels,plot.dir)
