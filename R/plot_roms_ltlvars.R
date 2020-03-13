#' Plots timeseries of LTL variables over desired boxes
#' 
#' Creates timeseries plots of LTL variables(phytoplankton, zooplankton, and bacteria).
#' This is based on aggregated ROMS .nc file output. It also assumes that .nc files are 
#' formated to run in hydroconstract. 
#' 
#' @year.dir string. The directory of the output that you want to summarize
#' @ltlvars.file string. The path to the transport ncdf file
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

plot_roms_ltlvars = function(year.dir,ltlvars.file,which.box = 0:29,which.levels = 4, plot.dir){
  
  ltlvars.nc = ncdf4::nc_open(paste0(year.dir,ltlvars.file))
  boxes = ltlvars.nc$dim$boxes$vals
  ndi = ncdf4::ncvar_get(ltlvars.nc,'ndi')
  nlg = ncdf4::ncvar_get(ltlvars.nc,'nlg')
  nlgz = ncdf4::ncvar_get(ltlvars.nc,'nlgz')
  nmdz = ncdf4::ncvar_get(ltlvars.nc,'nmdz')
  nsm = ncdf4::ncvar_get(ltlvars.nc,'nsm')
  nsmz = ncdf4::ncvar_get(ltlvars.nc,'nsmz')
  silg = ncdf4::ncvar_get(ltlvars.nc,'silg')
  nbact = ncdf4::ncvar_get(ltlvars.nc,'nbact')
  neus.time = ncdf4::ncvar_get(ltlvars.nc,'time')
  real.time = as.POSIXct(neus.time,origin = '1964-01-01',tz = 'GMT')
  
  ndi.plots = nlg.plots = nlgz.plots = nmdz.plots = nsm.plots = nsmz.plots = silg.plots = nbact.plots = list()
  
  reshape.var = function(var.df){
    DF = as.data.frame(t(var.df[1:which.levels,which.box[b.id]+1,]))
    colnames(DF) = paste0('lev',1:which.levels)
    DF$time = real.time
    DF = DF[,c(ncol(DF),1:(ncol(DF)-1))]
    DF = tidyr::gather(DF,'neus.level','value',colnames(DF)[2:ncol(DF)])
    DF = na.omit(DF)
    DF
  }
  
  plot.var = function(var.df,lab){
    g=ggplot2::ggplot(data = var.df, ggplot2::aes(x = time,y = value, color = neus.level))+
      ggplot2::geom_path()+
      ggplot2::ylab(lab)+
      ggplot2::xlab('')+
      ggplot2::ggtitle(plot.lab)+
      ggplot2::scale_color_manual(values = plot.cols, labels = paste0('Level ',1:which.levels),name = 'Surface = 1')+
      ggplot2::theme_minimal()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
    return(g)
  }
  
  make.plot = function(plot.ls,plot.title){
    pdf(paste0(plot.dir,plot.title,'.pdf'),width = 14, height = 6)
    for(b.id in seq_along(which.box)){
      gridExtra::grid.arrange(plot.ls[[b.id]])
      print(b.id)
    }
    dev.off()
  }
  
  for(b.id in seq_along(which.box) ){
    plot.lab = paste0('Box',which.box[b.id])
    plot.cols = RColorBrewer::brewer.pal(which.levels,'Set1')
    
    DF.ndi = reshape.var(ndi)
    ndi.plots[[b.id]] = plot.var(DF.ndi,expression('Diazotroph Biomass (mgN'~m^-3*')'))
    
    DF.nlg = reshape.var(nlg)
    nlg.plots[[b.id]] = plot.var(DF.nlg,expression('Large Phytoplankton Biomass (mgN'~m^-3*')'))
    
    DF.nlgz = reshape.var(nlgz)
    nlgz.plots[[b.id]] = plot.var(DF.nlgz,expression('Large Zooplankton Biomass (mgN'~m^-3*')'))
    
    DF.nmdz = reshape.var(nmdz)
    nmdz.plots[[b.id]] = plot.var(DF.nmdz,expression('Medium Zooplankton Biomass (mgN'~m^-3*')'))
    
    DF.nsm = reshape.var(nsm)
    nsm.plots[[b.id]] = plot.var(DF.nsm,expression('Small Phytoplankton Biomass (mgN'~m^-3*')'))
    
    DF.nsmz = reshape.var(nsmz)
    nsmz.plots[[b.id]] = plot.var(DF.nsmz,expression('Small Zooplankton Biomass (mgN'~m^-3*')'))
    
    DF.silg = reshape.var(silg)
    silg.plots[[b.id]] = plot.var(DF.silg,expression('Large Phyoplankton Biomass (mgS'~m^-3*')'))

    DF.nbact = reshape.var(nbact)
    nbact.plots[[b.id]] = plot.var(DF.nbact,expression('Bacterial Biomass (mgN'~m^-3*')'))
  
  }
  
  make.plot(ndi.plots,'Diazotroph')
  make.plot(nlg.plots,'Large Phyto')
  make.plot(nlgz.plots,'Large Zoo')
  make.plot(nmdz.plots,'Med Zoo')
  make.plot(nsm.plots,'Small Phyto')
  make.plot(nsmz.plots,'Small Zoo')
  make.plot(silg.plots,'Large Phyto Si')
  make.plot(nbact.plots,'Bacteria')
  
  nc_close(ltlvars.nc)
}


