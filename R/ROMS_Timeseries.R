#' Generates summary figures for ROMS-aggregated data (pre-hydroconstruct/forcing files)
#' 
#' Creates summary figures based on the post-processed ROMS_COBALT output
#' in the form of time-series of all variables for a given year. Horizontal flows
#' are plotted by face ID, and state variables are by boxes. 
#' 
#' @year.dir string. The directory of the output that you want to summarize
#' @which.face  numeric vector. Vector of faceid you want to plot. Default is all.
#' @which.box numeric vector. Vector of boxid you want to plot. Default is all.
#' @plot.hflux  logical. Plot horizontal fluxes?
#' @plot.statevar logical. Plot physcial state variables (i.e. temperature, salinity, vertical flux)?
#' @plot.ltlvar logical. Plot lower trophi level variables (i.e. phytoplankton, zooplankton)?
#' @which.levels numeric scalar. How many levels to plot (from 1-4) starting at surface. Default is all (4)
#' @plot.dir string. Directory where summary figures should go
#' 
#' @return Returns timeseries plots across entire duration of specified file
#' 
#' #' Created by J. Caracappa

# year.dir = plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1980/'
# which.face = 0:150
# which.box = 0:29
# plot.hflux = plot.statevar = plot.ltlvar = T
# which.levels = 4

ROMS_Timeseries = function(year.dir, which.face = 0:150, which.box = 0:29, plot.hflux = T, plot.statevar = T, plot.ltlvar = T,which.levels = 4, plot.dir){

    nc.files = dir(year.dir,pattern = '*.nc')
    
    # bgm = rbgm::bgmfile(here::here('Geometry','neus_tmerc_RM2.bgm'))
    
    if(plot.hflux){
      hflux.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
      source(here::here('R','plot_roms_hflux.R'))
      plot_roms_hflux(year.dir,hflux.file,which.face,which.levels,plot.dir)
    }
    
    if(plot.statevar){
      statevars.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='statevars') & all(strsplit(x,'[_.]+')[[1]] != 'ltl')))]
      source(here::here('R','plot_roms_statevars.R'))
      plot_roms_statevars(year.dir,statevars.file,which.box,which.levels,plot.dir)
    }
    
    if(plot.ltlvar){
      ltlvars.file = nc.files[which(sapply(nc.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'ltl')))]
      source(here::here('R','plot_roms_ltlvars.R'))
      plot_roms_ltlvars(year.dir,ltlvars.file,which.box,which.levels,plot.dir)
    }
  
}

ROMS_Timeseries(year.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1980/',
                plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1980/',
                which.face = 0:150,
                which.box = 0:29,
                which.levels = 4,
                plot.hflux = T, 
                plot.statevar = T,
                plot.ltlvar = T)
