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

year.dir = plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1983/'
which.face = 0:150
which.box = 0:29
plot.hflux = plot.statevar = plot.ltlvar = T
which.levels = 4

ROMS_Timeseries = function(year.dir, which.face = 0:150, which.box = 0:29, plot.hflux = T, plot.statevar = T, plot.ltlvar = T,which.levels = 4, plot.dir){

    nc.files = dir(year.dir,pattern = '*.nc')
    
    bgm = rbgm::bgmfile(here::here('Geometry','neus_tmerc_RM2.bgm'))
    
    if(plot.hflux){
      
      hflux.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
      hflux.nc = ncdf4::nc_open(paste0(year.dir,hflux.file))
      dest_b = ncdf4::ncvar_get(hflux.nc,'dest_boxid')
      source_b = ncdf4::ncvar_get(hflux.nc,'source_boxid')
      transport = ncdf4::ncvar_get(hflux.nc,'transport')
      
      for(f.id in which.face ){
        plot.lab = paste0('Face',f.id,': From-Box',source_b[f.id])
        DF = t(transport[1:which.levels,which(which.face == f.id),])
        colnames(DF) = paste0('lev',1:which.levels)
        
        
      }
      
      
      
    }
    
  
  
  
}