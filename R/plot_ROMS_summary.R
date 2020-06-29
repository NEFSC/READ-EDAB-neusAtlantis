#' Generates summary figures for ROMS-aggregated data (pre-hydroconstruct/forcing files)
#' 
#' Creates summary figures based on the post-processed ROMS_COBALT output
#' in the form of time-series of all variables for a given year. Horizontal flows
#' are plotted by face ID, and state variables are by boxes. 
#' 
#' @year.dir string. The directory of the output that you want to summarize
#' @which.face  numeric vector. Vector of faceid you want to plot. Default is all.
#' @which.boxes numeric vector. Vector of boxid you want to plot. Default is all.
#' @plot.hflux  logical. Plot horizontal fluxes?
#' @plot.statevar logical. Plot physcial state variables (i.e. temperature, salinity, vertical flux)?
#' @plot.ltlvar logical. Plot lower trophi level variables (i.e. phytoplankton, zooplankton)?
#' @plot.nutvar logical. Plot nutrient variables (i.e.no3, nh5, sio4, o2)?
#' @which.levels numeric scalar. How many levels to plot (from 1-4) starting at surface. Default is all (4)
#' @plot.dir string. Directory where summary figures should go
#' @scale.volume logical. Should biological state variables be scaled up to box volume (i.e. biomass)
#' @bgm.file string. Name of atlantis bgm file (only if scaling.volume=T)
#' @box.z.key string. Name of box-level-depth key with columes (.bx0,level,dz)
#' 
#' @return Returns timeseries plots across entire duration of specified file
#' 
#' #' Created by J. Caracappa

# year.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT_new_levels/1981/'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Summary_New_Levels/'
# plot.year = 1981
# which.face = 0:150
# which.boxes = 0:29
# plot.hflux = plot.ltlvar = plot.nutvar = F
# plot.statevar = T
# which.levels = 4
# box.z.key = here::here('Geometry','box_depth_key.csv')
# bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')

plot_ROMS_summary = function(year.dir, which.face = 0:150, which.boxes = 0:29,
                           plot.hflux = T, plot.statevar = T, plot.ltlvar = T,plot.nutvar = T,which.levels = 4, plot.dir,
                           scale.volume = F, bgm.file,box.z.key,plot.year){

    nc.files = dir(year.dir,pattern = '*.nc')
    
    # bgm = rbgm::bgmfile(here::here('Geometry','neus_tmerc_RM2.bgm'))
    
    if(plot.hflux){
      hflux.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
      source(here::here('R','plot_roms_hflux.R'))
      plot_roms_hflux(year.dir,hflux.file,which.face,which.levels,plot.dir,plot.year)
    }
    
    if(plot.statevar){
      
      statevars.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='statevars') & all(strsplit(x,'[_.]+')[[1]] != 'ltl')))]
      source(here::here('R','plot_roms_boxvars.R'))
      
      
      plot_roms_boxvars('salinity','Salinity','ppt',year.dir,roms.file = statevars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('temperature','Temperature','deg C',year.dir,roms.file = statevars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('verticalflux', 'Vertical Flux', 'm3 s-1',year.dir,roms.file = statevars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
    }
    
    if(plot.ltlvar){
      
      ltlvars.file = nc.files[which(sapply(nc.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'ltl')))]
      source(here::here('R','plot_roms_boxvars.R'))
      
      plot_roms_boxvars('ndi','Diazotroph','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nlg','Large Phytoplankton N','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nlgz','Large Zooplankton','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nmdz','Medium Zooplankton','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nsm','Small Phytoplankton','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nsmz','Small Zooplankton','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('silg','Large Phtyoplankton Si','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nbact','Bacteria','mg N m-3',year.dir,roms.file = ltlvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)

    }
    
    if(plot.nutvar){
      
      nutvars.file = nc.files[which(sapply(nc.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'nutvars')))]
      source(here::here('R','plot_roms_boxvars.R'))
      
      plot_roms_boxvars('no3','Nitrate','mg NO3 m-3',year.dir,roms.file = nutvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('nh4','Ammonia','mg NH4 m-3',year.dir,roms.file = nutvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('o2','Oxygen','mg O2 m-3',year.dir,roms.file = nutvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      plot_roms_boxvars('sio4','Silicate','mg SiO4 m-3',year.dir,roms.file = nutvars.file,which.boxes,which.levels,plot.dir,scale.volume,bgm.file,box.z.key,plot.year)
      
    }
  
}
# 
# plot_ROMS_summary(year.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing Files/',
#                 plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/',
#                 which.face = 0:150,
#                 which.boxes = 0:29,
#                 which.levels = 4,
#                 plot.hflux = T,
#                 plot.statevar = T,
#                 plot.ltlvar = T,
#                 scale.volume = T,
#                 bgm.file = 'neus_tmerc_RM2.bgm',
#                 box.z.key = 'box_depth_key.csv')
