#Reads in setup file and executes batcher

proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
batch.prefix = 'fish_sens_catch_scalar_species_1'
dir.create(paste0(proj.dir,'Atlantis_Runs/',batch.dir))

source(paste0(proj.dir,'R/atlantis_batcher.r'))

atlantis_batcher(
  batcherFilename = paste0(proj.dir,'Setup_Files/',paste0(batch.prefix,'.csv')),
  userName            = 'jcara',
  CHECK_TIME_INTERVAL = 600,
  NUM_TO_RUN          = 1,
  CONTAINER_TYPE      = 'podman',
  param.dir = paste0(proj.dir,'currentVersion/'),
  output.dir = pasete0(proj.dir,'Atlantis_Runs/',batch.prefix,'/')
)
