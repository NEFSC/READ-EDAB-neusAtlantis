# Test post-processing procedure with new functions

##Loads post-processing functions
source(here::here('R','get_atl_paramfiles.R'))
source(here::here('R','process_atl_output.R'))
source(here::here('R','make_atlantis_diagnostic_figures.R'))

#Define local/git directories for atlantis output, parameter files, and desired location for figures/tables

#Run name is the actual run name. Can be the same or different than run.prefix (e.g. "Fixed_Migration_ATL120")

run.name = 'ZM_Spatial_Final'


atl.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/')

dir.create(paste0(atl.dir,'Post_Processed/'))
param.dir = here::here ('currentVersion')
out.dir = paste0(atl.dir,'Post_Processed/')



#Run prefix is the filename prefix in the atlantis output (specified in run.bat)
run.prefix = 'neus_output'


#Run function that retreives parameter files
param.ls= get_atl_paramfiles(param.dir = param.dir,
                             atl.dir=atl.dir,
                             include_catch=T)

#Run  post-processing function to generate "result" R object. 

process_atl_output( param.dir = here::here('currentVersion'),
  atl.dir = atl.dir,
  out.dir = out.dir,

  run.prefix = 'neus_output',
  include_catch = T,
  save.out = T, #If T, saves to file, if F returns to current environment
  bgm.file = param.ls$bgm,
  groups.file = param.ls$func.groups,
  init.file = param.ls$init.nofill,
  biol.prm = param.ls$biol.prm,
  run.prm = param.ls$run.prm,
  main.nc = param.ls$main.nc,
  prod.nc = param.ls$prod.nc,
  dietcheck = param.ls$dietcheck,
  ssb = param.ls$ssb,
  yoy = param.ls$yoy,
  catch.file = param.ls$catch,
  totcatch.file = param.ls$catchtot,
  spatial.overlap = F #Warning takes a long time to run
)

#If result object saved to file or already exists load it into env.
# load(paste0(out.dir,'neus_output_postprocessed.rdata'))
load(paste0(out.dir,'neus_output_postprocessed.Rdata'))
library(ncdf4)
library(dplyr)
#Run diagnostic figures/tables script. See function document for more detailed description of figures.
make_atlantis_diagnostic_figures(
  atl.dir = atl.dir,
  out.dir = out.dir,
  param.dir = param.dir,
  run.prefix = 'neus_output_test',
  run.name = run.name,
  result = result,
  benthic.box =4,
  benthic.level = 4,
  
  param.ls = param.ls,
  
  # bgm.file = param.ls$bgm,
  # group.file = param.ls$func.groups,
  # biol.prm = param.ls$biol.prm,
  
  phytopl.history = here::here('R','phytoplankton_timeseries_biomass_tonnes_1998_2016.csv'),
  zoopl.history = here::here('R','Zooplankton_total_biomass_tonnes_N_20yrs.csv'),
 
  #Turn these on/off for desired output
  plot.benthic =F,
  plot.overall.biomass = F,
  plot.biomass.timeseries = T,
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
  plot.LTL=F,
  plot.catch =T
)
  