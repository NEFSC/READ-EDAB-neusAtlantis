#Reads in setup file and executes batcher
# install.packages('dplyr')
library(dplyr)

proj.dir = '/contrib/fishing_sensitivity/neus-atlantis/'
# proj.dir = here::here('/')

batch.prefix = 'sca_newdist_1'
dir.create(paste0(proj.dir,'Atlantis_Runs/',batch.prefix,'/'))
dir.create(paste0(proj.dir,'Setup_Files/'))

source(paste0(proj.dir,'R/edit_param_BH.R'))

alpha.scale = 1:10
group = 'MAK'

bh.orig = get_param_BH(paste0(proj.dir,'currentVersion/at_biology.prm')) %>%
  as.data.frame()%>%
  mutate(alpha = as.numeric(as.character(alpha)),
         beta = as.numeric(as.character(beta)))
base.group = bh.orig[which(bh.orig$group == group),]
new.bio.names = character()

for(i in 1:length(alpha.scale)){
  
  new.bio.names[i] = paste0(proj.dir,'currentVersion/at_biology_',i,'.prm')
  edit_param_BH(bio.prm = paste0(proj.dir,'currentVersion/at_biology.prm'),
                group.name = group,
                alpha = base.group$alpha[1] * alpha.scale[i],
                beta = base.group$beta,
                overwrite = F,
                new.file.name = new.bio.names[i]
                )
  print(new.bio.names[i])
}


setup.df = data.frame(
  Run = paste0(batch.prefix,1:length(alpha.scale)),
  OutputDir = paste0(batch.prefix,'/',batch.prefix,'_',1:10,'/'),
  BiolPrm = paste0('at_biology_',1:length(alpha.scale),'.prm'),
  RunPrm = 'at_run.prm',
  HarvestPrm = 'at_harvest.prm',
  InitNC = 'neus_init.nc',
  ForcePrm = 'at_force_LINUX.prm',
  Status = 'Incomplete'
)

write.csv(setup.df,paste0(proj.dir,'Setup_Files/',batch.prefix,'.csv'),row.names = F)

source(paste0(proj.dir,'R/atlantis_batcher_cloud.r'))

atlantis_batcher(
  batcherFilename =paste0(proj.dir,'Setup_Files/',batch.prefix,'.csv'),
  userName            = 'jcara',
  CHECK_TIME_INTERVAL = 600,
  NUM_TO_RUN          = 3,
  CONTAINER_TYPE      = 'podman',
  param.dir = paste0(proj.dir,'currentVersion/'),
  output.dir = paste0(proj.dir,'Atlantis_Runs/')
)
