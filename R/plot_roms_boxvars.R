#' Plots timeseries of any box-level variable in the spatially-aggregated ROMS files
#' 
#' Creates timeseries plots of box-level variables froms ROMS data aggregated onto atlantis grid
#' This is based on aggregated ROMS .nc file output. It also assumes that .nc files are 
#' formated to run in hydroconstract. 
#' 
#' @year.dir string. The directory of the output that you want to summarize
#' @var.name string. Name of variable from .nc file. Function only intended for state variables within boxes
#' @full.var.name string. Full name of variable including units. used for plotting 
#' @units = string. Units of variable
#' @plot.year = string. Year name for plot. Goes in plot file name
#' @roms.file string. The path to the transport ncdf file
#' @which.boxes  numeric vector. Vector of faceid you want to plot. Default is all.
#' @which.levels numeric scalar. How many levels to plot (from 1-4) starting at surface. Default is all (4)
#' @plot.dir string. Directory where summary figures should go
#' @scale.volume logical. If TRUE, scales up variables by box volume (i.e. total biomass in box instead of density)
#' @bgm.file = string. Full file name of atlantis BGM file
#' @box.z.key = string. Full file name for a csv which provides  key for atlantis boxes (.bx0), levels (level), and level depth interval (dz)
#' 
#' @return Returns timeseries plots across entire duration of specified file
#' 
#' #' Created by J. Caracappa


# test
# year.dir = plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1981/'
# var.name = 'nlg'
# full.var.name = 'Large Phytoplankton'
# units = 'mgN'
# roms.file = 'roms_cobalt_v10_ltl_statevars_1981_neus_atl.nc'
# which.boxes = 0:29
# which.levels = 4
# box.z.key = here::here('Geometry','box_depth_key.csv')
# bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
# scale.volume = F
# plot.year = '1981'


plot_roms_boxvars = function(var.name,full.var.name,units,year.dir,roms.file,which.boxes = 0:29,which.levels = 4, plot.dir,scale.volume=F,bgm.file,box.z.key,plot.year){
  
  boxvar.nc = ncdf4::nc_open(paste0(year.dir,roms.file))
  boxes = boxvar.nc$dim$boxes$vals
  var = ncdf4::ncvar_get(boxvar.nc,var.name)
  neus.time = ncdf4::ncvar_get(boxvar.nc,'time')
  real.time = as.POSIXct(neus.time,origin = '1964-01-01',tz = 'GMT')
  
  if(scale.volume){
    `%>%` = dplyr::`%>%`
    bgm = rbgm::bgmfile(bgm.file)
    volume = dplyr::select(bgm$boxes,c(.bx0,area))
    z.key = read.csv(box.z.key)
    box.volume = dplyr::left_join(volume,z.key,by='.bx0')
    box.volume$volume = box.volume$area*box.volume$dz
    box.volume$id = 1:nrow(box.volume)
    for(lev in 1:dim(var)[1]){
      box.volume2 = dplyr::group_by(box.volume,.bx0)
      box.volume2 =  dplyr::summarize(box.volume2,
                                      rank = which.max(level),
                                      id = id[rank],
                                      volume = volume[rank],
                                      level = level[rank])
      var[lev,box.volume2$.bx0+1,] = log(var[lev,box.volume2$.bx0+1,]*box.volume2$volume,10)
      box.volume = box.volume[-which(box.volume$id %in% box.volume2$id),]
    }
  }
  
  var.plots = list()
  
  reshape.var = function(var.df){
    DF = as.data.frame(t(var.df[1:which.levels,which.boxes[b.id]+1,]))
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
    for(b.id in seq_along(which.boxes)){
      gridExtra::grid.arrange(plot.ls[[b.id]])
      print(b.id)
    }
    dev.off()
  }
  
  
  for(b.id in seq_along(which.boxes) ){
    plot.lab = paste0('Box',which.boxes[b.id])
    plot.cols = RColorBrewer::brewer.pal(which.levels,'Set1')
    
    DF.var = reshape.var(var)
    
    var.plots[[b.id]] = plot.var(DF.var,paste0(full.var.name,' (',units,')'))
    
  }
  
  make.plot(var.plots, paste0(full.var.name,' ',plot.year))

  
  ncdf4::nc_close(boxvar.nc)
}


