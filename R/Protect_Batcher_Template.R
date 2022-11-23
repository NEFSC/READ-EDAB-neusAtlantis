#Script to generate batcher runs with multiple groups of parameter changes

#Define Batch Directory
batch.dir = 'Batcher_MaxCohort_1'
dir.create(here::here('Atlantis_Runs',batch.dir))

base.bio.prm = here::here('currentVersion','at_biology.prm')
base.init = here::here('neus_init.nc')

##Batcher Description:
###4 Runs of misc mumC changes
# source(here::here('R','make_mumC_param_files.R'))
# run.prefix = 'misc_mumC_6'
# 
# set.dir = here::here('Atlantis_Runs',batch.dir,run.prefix)
# dir.create(set.dir)
# 
# mumC.files = make_mumC_parameter_files(
#   n.run = 4,
#   #Choose parameter interval. If TRUE, interval is log-scaled (Better for multiple orders of magnitude)
#   log.scaling = F,
#   #Choose the run set's name
#   run.prefix = run.prefix,
#   #Choose the run set's directory 
#   project.dir = here::here('Atlantis_Runs',run.prefix,''),
#   #Choose the parameter directory
#   param.dir = here::here('currentVersion/'),
#   #Choose the name of the run index (i.e. the Set up file)
#   run.index = read.csv(here::here('Setup_Files',paste0(run.prefix,'.csv')),as.is = T),
#   #Choose the name for the expanded run index
#   run.index.long = here::here('Setup_Files',paste0(run.prefix,'_long.csv'))
# )
# 
# run.dirs = paste0(run.prefix,'_',0:3)
# mumC.setup = data.frame(
#   Run = run.dirs ,
#   OutputDir = paste0(batch.dir,'/',run.prefix,'/',run.dirs,'/'),
#   BiolPrm = paste0('at_biology_',run.prefix,'_',0:3),
#   RunPrm = 'at_run.prm',
#   HarvestPrm = 'at_harvest.prm',
#   InitNC = 'neus_init.nc'
# )

###4 Runs of misc BHalpha changes
# source(here::here('R','make_BHalpha_param_files.R'))
# BHalpha.prefix = 'misc_BHalpha_3'
# BH.alpha.files = make_BHalpha_param_files(
#   #Choose number of runs
#   n.run = 4,
#   #Choose parameter interval. If TRUE, interval is log-scaled (Better for multiple orders of magnitude)
#   log.scaling = F,
#   #Choose the run set's name
#   run.prefix = BHalpha.prefix,
#   #Choose the run set's directory 
#   project.dir = here::here('Atlantis_Runs/',BHalpha.prefix,''),
#   #Choose the parameter directory
#   param.dir = here::here('currentVersion',''),
#   #Choose the name of the run index (i.e. the Set up file)
#   run.index = read.csv(here::here('Setup_Files',paste0(BHalpha.prefix,'.csv')),as.is = T),
#   #Choose the name for the expanded run index
#   run.index.long = here::here('Setup_Files',paste0(BHalpha.prefix,'_long.csv'))
# )

#Edit mumC age distribution
source(here::here('R/edit_param_mum_shape.R'))
source(here::here('R/make_age_distribution.R'))

run.prefix = 'misc_mumC_shape'
steepness = seq(2,5,length.out =5)
mumC_shape_files = character()

set.dir = here::here('Atlantis_Runs',batch.dir,run.prefix)
dir.create(set.dir)

# plot(0,0,type='n',ylim = c(0,0.5),xlim = c(1,10))
for(i in 1:length(steepness)){
  
  # new.age.dist =sn::dsn(x =1:10,xi = peak.age[i],omega = peak.age[i],alpha =-peak.age[i]/2)
  new.age.dist = make_age_distribution(peak.age = 3.5,steepness = steepness[i] )
  # lines(new.age.dist,col = i)
  new.age.dist = new.age.dist/mean(new.age.dist)
  mumC_shape_files[i] = paste0('at_biology_',run.prefix,'_',i-1,'.prm')

  reshape_mumC(bio.prm = here::here('currentVersion','at_biology.prm'),
               groups = c('RED','WIF','DSH','MEN','WTF','SSH'),
               mum.shape = new.age.dist,
               overwrite = F,
               new.file.name = here::here('currentVersion',mumC_shape_files[i]),
               change.mum = T,
               change.C = F
  )
}

run.dirs = paste0(run.prefix,'_',0:(length(steepness)-1))
mumC.shape.setup = data.frame(
  Run = run.dirs ,
  OutputDir = paste0(batch.dir,'/',run.prefix,'/',run.dirs,'/'),
  BiolPrm = mumC_shape_files,
  RunPrm = 'at_run.prm',
  HarvestPrm = 'at_harvest.prm',
  InitNC = 'neus_init.nc'
)

#Edit initial age distribution
# source(here::here('R','edit_init_age_distribution.R'))
# steepness.init = seq(2,3.5,length.out =3)
# 
# init.age.files = character()
# run.prefix = 'age_dist_1'
# 
# set.dir = here::here('Atlantis_Runs',batch.dir,run.prefix)
# dir.create(set.dir)
# 
# for(i in 1:3){
#   init.age.files[i] = paste0('neus_init_',run.prefix,'_',i-1,'.nc')
#   
#   edit_init_age_distribution(
#     bio.prm =  here::here('currentVersion','at_biology.prm'),
#     fgs.file = here::here('currentVersion','neus_groups.csv'),
#     init.file = here::here('currentVersion','neus_init.nc'),
#     ss.cat.file = here::here('data','StockSmart_Abundance_Units_Conversion.csv'),
#     ss.conv.file = here::here('data-raw','StockSmart_Conversions.csv'),
#     box.prop.file = here::here('diagnostics','Group_Box_Proportions.csv'),
#     peak.age = 3.5,
#     steepness = steepness.init[i],
#     ref.run.dir = here::here('Atlantis_Runs','Dev_07282022','Post_Processed','Data/'),
#     prescribed.age.scale = F,
#     age.scale =  c(0.005,0.03,0.2,0.2,0.175,0.15,0.1,0.075,0.05,0.015),
#     init.size.age = here::here('diagnostics','Initial_Size_Age.csv'),
#     ss.adj.abund.file =  here::here('diagnostics','StockSmart_Adjusted_Abundance.csv'),
#     overwrite = F,
#     new.init.file = here::here('currentVersion',init.age.files[i]),
#     groups = 'GOO'
#   )
# }
# 
# run.dirs = paste0(run.prefix,'_',0:2)
# init.age.setup = data.frame(
#   Run = run.dirs ,
#   OutputDir = paste0(batch.dir,'/',run.prefix,'/',run.dirs,'/'),
#   BiolPrm = 'at_biology.prm',
#   RunPrm = 'at_run.prm',
#   HarvestPrm = 'at_harvest.prm',
#   InitNC = init.age.files
# )
# 
# #Edit KDENR
# source(here::here('R','edit_param_KDENR.R'))
# 
# run.prefix = 'misc_KDENR_1'
# kdenr.scale = seq(0.25,0.75,0.25)
# groups = c('PLA','TAU','OPT')
# kdenr.files = character()
# 
# set.dir = here::here('Atlantis_Runs',batch.dir,run.prefix)
# dir.create(set.dir)
# 
# for(i in 1:3){
#   
#   kdenr.files[i] = paste0('at_biology_',run.prefix,'_',i-1,'.prm')
#   
#   # file.copy(here::here('currentVersion','at_biology.prm'),kdenr.file[i],overwrite =T)
#   kdenr.vals = get_param_KDENR(base.bio.prm)%>%
#     filter(group %in% groups)
#   
#   new.kdenr = as.numeric(kdenr.vals$KDENR)*kdenr.scale[i]
#   
#   edit_param_KDENR(bio.prm = base.bio.prm,
#                   KDENR = new.kdenr,
#                   group.name = groups,
#                   overwrite = F,
#                   new.file.name = here::here('currentVersion',kdenr.files[i]))
# }
# 
# run.dirs = paste0(run.prefix,'_',0:2)
# kdenr.setup = data.frame(
#   Run = run.dirs ,
#   OutputDir = paste0(batch.dir,'/',run.prefix,'/',run.dirs,'/'),
#   BiolPrm = kdenr.files,
#   RunPrm = 'at_run.prm',
#   HarvestPrm = 'at_harvest.prm',
#   InitNC = 'neus_init.nc'
# )
# 
# #Edit initial RN 
# source(here::here('R','edit_param_RN_init.R'))
# 
# run.prefix = 'RN_init_1'
# new.scale = seq(1,2,length.out = 3)
# rn.init.files = character()
# 
# set.dir = here::here('Atlantis_Runs',batch.dir,run.prefix)
# dir.create(set.dir)
# 
# for(i in 1:3){
#   
#   rn.init.files[i] = paste0('neus_init_',run.prefix,'_',i-1,'.nc')
#   
#   edit_param_RN_init(
#     init.file =  here::here('currentVersion','neus_init.nc'),
#     new.filename = here::here('currentVersion',rn.init.files[i]),
#     fgs.file =  here::here('currentVersion','neus_groups.csv'),
#     group.change = 'BPF',
#     new.scale = new.scale[i],
#     overwrite = F,
#     out.file = here::here('diagnostics','Initial_Size_Age.csv')
#   )
# }
# run.dirs = paste0(run.prefix,'_',0:2)
# rn.init.setup = data.frame(
#   Run = run.dirs ,
#   OutputDir = paste0(batch.dir,'/',run.prefix,'/',run.dirs,'/'),
#   BiolPrm = 'at_biology.prm',
#   RunPrm = 'at_run.prm',
#   HarvestPrm = 'at_harvest.prm',
#   InitNC = rn.init.files
# )

# batcher.setup.df = bind_rows(mumC.setup,mumC.shape.setup,init.age.setup,kdenr.setup,rn.init.setup)
batcher.setup.df = mumC.shape.setup

write.csv(batcher.setup.df,file = here::here('Setup_Files',paste0(batch.dir,'.csv')),row.names = F)

source(here::here('R','atlantis_batcher.r'))
atlantis_batcher(
  batcherFilename = here::here('Setup_Files',paste0(batch.dir,'.csv')),
  userName            = 'jcara',
  CHECK_TIME_INTERVAL = 60,
  NUM_TO_RUN          = 4,
  CONTAINER_TYPE      = 'podman',
  param.dir = here::here('currentVersion',''),
  output.dir = here::here('Atlantis_Runs','')
  )
