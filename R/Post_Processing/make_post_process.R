#' Master post-processing function routine
#' Includes options to turn on/off separate data or graphic options
#' @run.name = prefix for run output
#' @atl.dir = directory for run output
#' @process.all = whether to process and plot all possible output
#' @plot.XXX = whether to plot specific outputs (and processes relevant data)

make_post_process = function(run.name,
                             run.dir,
                             process.all = T,
                             plot.benthic =F,
                             plot.overall.biomass =F,
                             plot.biomass.timeseries = F,
                             plot.length.age = F,
                             plot.biomass.box=F,
                             plot.c.mum=F,
                             plot.sn.rn=F,
                             plot.recruits=F,
                             plot.numbers.timeseries=F,
                             plot.physics=F,
                             plot.growth.cons=F,
                             plot.cohort=F,
                             plot.diet=T,
                             plot.consumption= T,
                             plot.spatial.biomass=F,
                             plot.spatial.biomass.seasonal = F,
                             plot.LTL=F, 
                             plot.catch =F,
                             plot.mortality=T,
                             plot.max.weight = F){
  
  # Test post-processing procedure with new functions
  library(ncdf4)
  library(dplyr)
  library(atlantistools)
  library(RNetCDF)
  ##Loads post-processing functions
  
  source(here::here('R','Post_Processing','get_atl_paramfiles.R'))
  source(here::here('R','Post_Processing','process_atl_output.R'))
  source(here::here('R','Post_Processing','make_atlantis_diagnostic_figures.R'))
  
  dir.create(paste0(atl.dir,'Post_Processed/'))
  dir.create(paste0(atl.dir,'Post_Processed/Data/'))
  param.dir = here::here ('currentVersion')
  out.dir = paste0(atl.dir,'Post_Processed/Data/')
  fig.dir = paste0(atl.dir,'Post_Processed/')
  
  #Run prefix is the filename prefix in the atlantis output (specified in run.bat)
  run.prefix = 'neus_output'
  
  #Run function that retreives parameter files
  param.ls= get_atl_paramfiles(param.dir = param.dir,
                               atl.dir=atl.dir,
                               include_catch=T)
  
  #Run  post-processing function to generate "result" R object. 
  process_atl_output(
    param.dir = here::here('currentVersion'),
    atl.dir = atl.dir,
    out.dir = out.dir,
    run.prefix = 'neus_output',
    param.ls = param.ls,
    include_catch = T,
    save.out = T,
    agg.scale = 'year',
    spatial.overlap = F,
    large.file = F,
    system = 'linux'
  )
  #If result object saved to file or already exists load it into env.
  # load(paste0(out.dir,'neus_output_postprocessed.rdata'))
  # load(paste0(out.dir,'neus_output_postprocessed.Rdata'))
  
  #Run diagnostic figures/tables script. See function document for more detailed description of figures.
  make_atlantis_diagnostic_figures(
    atl.dir = atl.dir,
    fig.dir = fig.dir,
    out.dir = out.dir,
    param.dir = param.dir,
    run.prefix = 'neus_output',
    run.name = run.name,
    benthic.box =4,
    benthic.level = 4,
    
    param.ls = param.ls,
    
    phytopl.history = here::here('R','phytoplankton_timeseries_biomass_tonnes_1998_2016.csv'),
    zoopl.history = here::here('R','Zooplankton_total_biomass_tonnes_N_20yrs.csv'),
    
    plot.all = F,
    #Turn these on/off for desired output
    plot.benthic =F,
    plot.overall.biomass =F,
    plot.biomass.timeseries = F,
    plot.length.age = F,
    plot.biomass.box=F,
    plot.c.mum=F,
    plot.sn.rn=F,
    plot.recruits=F,
    plot.numbers.timeseries=F,
    plot.physics=F,
    plot.growth.cons=F,
    plot.cohort=F,
    plot.diet=T,
    plot.consumption= T,
    plot.spatial.biomass=F,
    plot.spatial.biomass.seasonal = F,
    plot.LTL=F, 
    plot.catch =F,
    plot.mortality=T,
    plot.max.weight = F
    
  )
  
  
  
}