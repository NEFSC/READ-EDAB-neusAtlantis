# Test post-processing procedure with new functions
library(ncdf4)
library(dplyr)
library(atlantistools)
##Loads post-processing functions
source(here::here('R','get_atl_paramfiles.R'))
source(here::here('R','process_atl_output.R'))
source(here::here('R','make_atlantis_diagnostic_figures.R'))

#Define local/git directories for atlantis output, parameter files, and desired location for figures/tables

#Run name is the actual run name. Can be the same or different than run.prefix (e.g. "Fixed_Migration_ATL120")

run.name = 'Dev_01112022'

atl.dir = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/',run.name,'/')

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
  diet.agg.time = 'month',
  spatial.overlap = F,
  large.file = F
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
  
  # bgm.file = param.ls$bgm,
  # group.file = param.ls$func.groups,
  # biol.prm = param.ls$biol.prm,
  
  phytopl.history = here::here('R','phytoplankton_timeseries_biomass_tonnes_1998_2016.csv'),
  zoopl.history = here::here('R','Zooplankton_total_biomass_tonnes_N_20yrs.csv'),
 
  plot.all = F,
  #Turn these on/off for desired output
  plot.benthic =F,
  plot.overall.biomass = T,
  plot.biomass.timeseries = T,
  plot.length.age = F,
  plot.biomass.box=T,
  plot.c.mum=T,
  plot.sn.rn=T,
  plot.recruits=T,
  plot.numbers.timeseries=T,
  plot.physics=T,
  plot.growth.cons=T,
  plot.cohort=T,
  plot.diet=T,
  plot.consumption= F,
  plot.spatial.biomass=F,
  plot.spatial.biomass.seasonal = F,
  plot.LTL=F,
  plot.catch =T,
  plot.mortality=F

)
  
