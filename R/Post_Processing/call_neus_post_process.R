#' General post-processing wrapper routine for NEUS
#' User Params to change:
#' @run.name = prefix for run output
#' @atl.dir = directory for run output
#' @process.all = whether to process and plot all possible output
#' @plot.XXX = whether to plot specific outputs (and processes relevant data)

source(here::here('R','Post_Processing','make_post_process.R'))

run.name = 'v6681_Calib_5'
atl.dir = here::here('Atlantis_Runs',run.name,'')
# 
# process.all = T
# plot.all = T
# 
# large.file = F
# 
# plot.benthic =T
# plot.overall.biomass =T
# plot.biomass.timeseries = T
# plot.length.age = T
# plot.biomass.box=T
# plot.c.mum=T
# plot.sn.rn=T
# plot.recruits=T
# plot.numbers.timeseries=T
# plot.physics=T
# plot.growth.cons=T
# plot.cohort=T
# plot.diet=T
# plot.consumption= T
# plot.spatial.biomass=T
# plot.spatial.biomass.seasonal = T
# plot.spatial.overlap = T
# plot.LTL=T
# plot.catch =T
# plot.mortality=T
# plot.max.weight = T

make_post_process(
  
  run.name = run.name,
  atl.dir = atl.dir,
  
  process.all = F,
  plot.all = F,
  
  large.file = F,
  
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
  plot.consumption= F,
  plot.spatial.biomass=F,
  plot.spatial.biomass.seasonal = F,
  plot.spatial.overlap = F,
  plot.LTL=F, 
  plot.catch =F,
  plot.mortality=F,
  plot.max.weight = F,
  
  benthic.box = 1,
  benthic.level = 4,
  
  run.prefix = 'neus_output',
  agg.scale= 'year',
  system ='linux'
  
)

