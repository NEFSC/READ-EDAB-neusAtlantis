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

plot_roms_ltlvars = function(year.dir,ltlvars.file,which.box = 0:29,which.levels = 4, plot.dir,scale.volume=F,bgm.file,box.z.key){
  

  
  
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
  
  
  if(scale.volume){
    `%>%` = dplyr::`%>%`
    bgm = rbgm::bgmfile(here::here('Geometry',bgm.file))
    volume = dplyr::select(bgm$boxes,c(.bx0,area))
    z.key = read.csv(here::here('Geometry',box.z.key))
    box.volume = dplyr::left_join(volume,z.key,by='.bx0')
    box.volume$volume = box.volume$area*box.volume$dz
    box.volume$id = 1:nrow(box.volume)
    for(lev in 1:dim(ndi)[1]){
      box.volume2 = box.volume %>% dplyr::group_by(.bx0) %>% 
        dplyr::summarize(rank = which.max(level),
                         id = id[rank],
                         volume = volume[rank],
                         level = level[rank])
      ndi[lev,box.volume2$.bx0+1,]=log(ndi[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      nlg[lev,box.volume2$.bx0+1,]=log(nlg[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      nlgz[lev,box.volume2$.bx0+1,]=log(nlgz[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      nmdz[lev,box.volume2$.bx0+1,]=log(nmdz[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      nsm[lev,box.volume2$.bx0+1,]=log(nsm[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      nsmz[lev,box.volume2$.bx0+1,]=log(nsmz[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      silg[lev,box.volume2$.bx0+1,]=log(silg[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      nbact[lev,box.volume2$.bx0+1,]=log(nbact[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      box.volume = box.volume[-which(box.volume$id %in% box.volume2$id),]
    }
  }
  
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
    DF.nlg = reshape.var(nlg)
    DF.nlgz = reshape.var(nlgz)
    DF.nmdz = reshape.var(nmdz)
    DF.nsm = reshape.var(nsm)
    DF.nsmz = reshape.var(nsmz)
    DF.silg = reshape.var(silg)
    DF.nbact = reshape.var(nbact)
    
    if(scale.volume){
      ndi.plots[[b.id]] = plot.var(DF.ndi,bquote('Diazotroph Biomass (mgN)'))
      nlg.plots[[b.id]] = plot.var(DF.nlg,expression('Large Phytoplankton Biomass (mgN)'))
      nlgz.plots[[b.id]] = plot.var(DF.nlgz,expression('Large Zooplankton Biomass (mgN)'))
      nmdz.plots[[b.id]] = plot.var(DF.nmdz,expression('Medium Zooplankton Biomass (mgN)'))
      nsm.plots[[b.id]] = plot.var(DF.nsm,expression('Small Phytoplankton Biomass (mgN)'))
      nsmz.plots[[b.id]] = plot.var(DF.nsmz,expression('Small Zooplankton Biomass (mgN)'))
      silg.plots[[b.id]] = plot.var(DF.silg,expression('Large Phyoplankton Biomass (mgS)'))
      nbact.plots[[b.id]] = plot.var(DF.nbact,expression('Bacterial Biomass (mgN)'))
    }else{
      ndi.plots[[b.id]] = plot.var(DF.ndi,bquote('Diazotroph Biomass (mgN'~m^-3*')'))
      nlg.plots[[b.id]] = plot.var(DF.nlg,expression('Large Phytoplankton Biomass (mgN'~m^-3*')'))
      nlgz.plots[[b.id]] = plot.var(DF.nlgz,expression('Large Zooplankton Biomass (mgN'~m^-3*')'))
      nmdz.plots[[b.id]] = plot.var(DF.nmdz,expression('Medium Zooplankton Biomass (mgN'~m^-3*')'))
      nsm.plots[[b.id]] = plot.var(DF.nsm,expression('Small Phytoplankton Biomass (mgN'~m^-3*')'))
      nsmz.plots[[b.id]] = plot.var(DF.nsmz,expression('Small Zooplankton Biomass (mgN'~m^-3*')'))
      silg.plots[[b.id]] = plot.var(DF.silg,expression('Large Phyoplankton Biomass (mgS'~m^-3*')'))
      nbact.plots[[b.id]] = plot.var(DF.nbact,expression('Bacterial Biomass (mgN'~m^-3*')'))
    }

  
  }
  
  make.plot(ndi.plots,paste0(plot.prefix,' Diazotroph'))
  make.plot(nlg.plots,paste0(plot.prefix,' Large Phyto'))
  make.plot(nlgz.plots,paste0(plot.prefix,' Large Zoo'))
  make.plot(nmdz.plots,paste0(plot.prefix,' Med Zoo'))
  make.plot(nsm.plots,paste0(plot.prefix,' Small Phyto'))
  make.plot(nsmz.plots,paste0(plot.prefix,' Small Zoo'))
  make.plot(silg.plots,paste0(plot.prefix,' Large Phyto Si'))
  make.plot(nbact.plots,paste0(plot.prefix,' Bacteria'))
  
  ncdf4::nc_close(ltlvars.nc)
}


