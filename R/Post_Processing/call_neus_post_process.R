#' General post-processing wrapper routine for NEUS
#' User Params to change:
#' @run.name = prefix for run output
#' @atl.dir = directory for run output
#' @process.all = whether to process and plot all possible output
#' @plot.XXX = whether to plot specific outputs (and processes relevant data)

source(here::here('R','Post_Processing','make_post_process.R'))

make_post_process(
  
  run.name = 'Dev_11032022',
  atl.dir = here::here('Atlantis_Runs',run.name,''),
  
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
  plot.max.weight = F
  
  
)

