#' General post-processing wrapper routine for NEUS
#' User Params to change:
#' @run.name = prefix for run output
#' @atl.dir = directory for run output
#' @process.all = whether to process and plot all possible output
#' @plot.XXX = whether to plot specific outputs (and processes relevant data)

library(atlantisprocessing)
# source(here::here('R','Post_Processing','make_post_process.R'))

run.name = 'GOO_ddepend3_noT'
atl.dir = here::here('Atlantis_Runs',run.name,'')
param.dir = here::here('currentVersion','/')
run.prefix = 'neus_output'
param.ls = get_atl_paramfiles(param.dir,atl.dir,run.prefix = run.prefix,include_catch = T)

process_atl_output(
  param.dir = param.dir,
  atl.dir= atl.dir,
  out.dir = file.path(atl.dir, "/Post_Processed/Data/"),
  run.prefix = run.prefix,
  param.ls = param.ls,
  agg.scale= 'year',
  large.file = F,
  system = 'linux',
  process.all = F,
  plot.all = F,
  plot.benthic = F,
  plot.overall.biomass = F,
  plot.biomass.timeseries = F,
  plot.length.age = F,
  plot.biomass.box = T,
  plot.c.mum = F,
  plot.sn.rn = F,
  plot.recruits = F,
  plot.numbers.timeseries = T,
  plot.physics = F,
  plot.growth.cons = F,
  plot.cohort = F,
  plot.diet = F,
  plot.consumption = F,
  plot.spatial.biomass = F,
  plot.spatial.biomass.seasonal = F,
  plot.catch = F,
  plot.mortality = F,
  plot.weight = F,
  plot.spatial.overlap = F
)

# run.name = 'HER_TS_dist_ddepend0'
# atl.dir = 
# # 
# # process.all = T
# # plot.all = T
# # 
# # large.file = F
# # 
# # plot.benthic =T
# # plot.overall.biomass =T
# # plot.biomass.timeseries = T
# # plot.length.age = T
# # plot.biomass.box=T
# # plot.c.mum=T
# # plot.sn.rn=T
# # plot.recruits=T
# # plot.numbers.timeseries=T
# # plot.physics=T
# # plot.growth.cons=T
# # plot.cohort=T
# # plot.diet=T
# # plot.consumption= T
# # plot.spatial.biomass=T
# # plot.spatial.biomass.seasonal = T
# # plot.spatial.overlap = T
# # plot.LTL=T
# # plot.catch =T
# # plot.mortality=T
# # plot.max.weight = T
# 
# process_atl_output(
#   
#   run.name = run.name,
#   atl.dir = atl.dir,
#   
#   process.all = F,
#   plot.all = F,
#   
#   large.file = F,
#   
#   plot.benthic =F,
#   plot.overall.biomass =F,
#   plot.biomass.timeseries = F,
#   plot.length.age = F,
#   plot.biomass.box=T,
#   plot.c.mum=F,
#   plot.sn.rn=F,
#   plot.recruits=F,
#   plot.numbers.timeseries=F,
#   plot.physics=F,
#   plot.growth.cons=F,
#   plot.cohort=F,
#   plot.diet=F,
#   plot.consumption= F,
#   plot.spatial.biomass=F,
#   plot.spatial.biomass.seasonal = F,
#   plot.spatial.overlap = F,
#   plot.LTL=F, 
#   plot.catch =F,
#   plot.mortality=F,
#   plot.max.weight = F,
#   
#   benthic.box = 1,
#   benthic.level = 4,
#   
#   run.prefix = 'neus_output',
#   agg.scale= 'year',
#   system ='linux'
#   
# )

