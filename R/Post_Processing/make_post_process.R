#' Master post-processing function routine
#' Includes options to turn on/off separate data or graphic options
#' @run.name = prefix for run output
#' @atl.dir = directory for run output
#' @process.all = whether to process and plot all possible output
#' @plot.XXX = whether to plot specific outputs (and processes relevant data)

make_post_process = function(run.name,
                             atl.dir,
                             large.file =F,
                             process.all = F,
                             plot.all = F,
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
                             plot.diet=F,
                             plot.consumption= F,
                             plot.spatial.biomass=F,
                             plot.spatial.biomass.seasonal = F,
                             plot.spatial.overlap = F,
                             plot.LTL=F, 
                             plot.catch =F,
                             plot.mortality=F,
                             plot.max.weight = F,
                             benthic.box,
                             benthic.level,
                             
                             run.prefix = 'neus_output',
                             agg.scale= 'year',
                             system ='linux'){
  
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
  param.dir = here::here ('currentVersion','/')
  out.dir = paste0(atl.dir,'Post_Processed/Data/')
  fig.dir = paste0(atl.dir,'Post_Processed/')
  
  #Run function that retreives parameter files
  param.ls= get_atl_paramfiles(param.dir = param.dir,
                               atl.dir=atl.dir,
                               run.prefix = run.prefix,
                               include_catch=T)
  
  #Run  post-processing function to generate "result" R object. 
  process_atl_output(
    param.dir = param.dir,
    atl.dir = atl.dir,
    out.dir = out.dir,
    run.prefix = run.prefix,
    param.ls = param.ls,
    agg.scale = agg.scale,
    large.file = large.file,
    system = system,
    process.all = process.all,
    plot.all = plot.all,
    plot.benthic = plot.benthic,
    plot.overall.biomass = plot.overall.biomass,
    plot.biomass.timeseries = plot.biomass.timeseries,
    plot.length.age = plot.length.age,
    plot.biomass.box=plot.biomass.box,
    plot.c.mum=plot.c.mum,
    plot.sn.rn=plot.sn.rn,
    plot.recruits=plot.recruits,
    plot.numbers.timeseries=plot.numbers.timeseries,
    plot.physics=plot.physics,
    plot.growth.cons=plot.growth.cons,
    plot.cohort=plot.cohort,
    plot.diet=plot.diet,
    plot.consumption= plot.consumption,
    plot.spatial.biomass=plot.spatial.biomass,
    plot.spatial.biomass.seasonal = plot.spatial.biomass.seasonal,
    plot.spatial.overlap = plot.spatial.overlap,
    plot.LTL=plot.LTL, 
    plot.catch =plot.catch,
    plot.mortality=plot.mortality,
    plot.max.weight = plot.max.weight
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
    run.prefix = run.prefix,
    run.name = run.name,
    benthic.box =benthic.box,
    benthic.level = benthic.level,
    
    param.ls = param.ls,
    
    phytopl.history = here::here('R','phytoplankton_timeseries_biomass_tonnes_1998_2016.csv'),
    zoopl.history = here::here('R','Zooplankton_total_biomass_tonnes_N_20yrs.csv'),
    
    plot.all = plot.all,
    plot.benthic = plot.benthic,
    plot.overall.biomass = plot.overall.biomass,
    plot.biomass.timeseries = plot.biomass.timeseries,
    plot.length.age = plot.length.age,
    plot.biomass.box=plot.biomass.box,
    plot.c.mum=plot.c.mum,
    plot.sn.rn=plot.sn.rn,
    plot.recruits=plot.recruits,
    plot.numbers.timeseries=plot.numbers.timeseries,
    plot.physics=plot.physics,
    plot.growth.cons=plot.growth.cons,
    plot.cohort=plot.cohort,
    plot.diet=plot.diet,
    plot.consumption= plot.consumption,
    plot.spatial.biomass=plot.spatial.biomass,
    plot.spatial.biomass.seasonal = plot.spatial.biomass.seasonal,
    plot.LTL=plot.LTL, 
    plot.catch =plot.catch,
    plot.mortality=plot.mortality,
    plot.max.weight = plot.max.weight
    
  )
  
  
  
}