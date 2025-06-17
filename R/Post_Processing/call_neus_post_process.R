#' General post-processing wrapper routine for NEUS
#' User Params to change:
#' @run.name = prefix for run output
#' @atl.dir = directory for run output
#' @process.all = whether to process and plot all possible output
#' @plot.XXX = whether to plot specific outputs (and processes relevant data)

source(here::here('R','Post_Processing','make_post_process.R'))

run.name = 'devplusfleets'
atl.dir = here::here('Atlantis_Runs',run.name,'/')
# atl.dir = '/net/work3/EDAB/atlantis/Andy_Proj/devplusfleets/'
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
  plot.biomass.timeseries = T,
  plot.length.age = T,
  plot.biomass.box=T,
  plot.c.mum=F,
  plot.sn.rn=T,
  plot.recruits=F,
  plot.numbers.timeseries=T,
  plot.physics=T,
  plot.growth.cons=F,
  plot.cohort=F,
  plot.diet=T,
  plot.consumption= F,
  plot.spatial.biomass=F,
  plot.spatial.biomass.seasonal = F,
  plot.spatial.overlap = F,
  plot.catch =T,
  plot.spatial.catch =T,
  plot.catch.fleet =T,
  plot.mortality=T,
  plot.weight = T,
  
  benthic.box = 1,
  benthic.level = 4,
  
  run.prefix = 'neus_output',
  agg.scale= 'year',
  system ='linux'
  
)

