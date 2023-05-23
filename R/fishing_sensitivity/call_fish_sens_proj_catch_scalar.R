#Reads in setup file and executes batcher

batch.prefix = 'fish_sens_catch_scalar_species_1'
dir.create(here::here('Atlantis_Runs',batch.dir))

source(here::here('R','atlantis_batcher.r'))

atlantis_batcher(
  batcherFilename = here::here('Setup_Files',paste0(batch.prefix,'.csv')),
  userName            = 'jcara',
  CHECK_TIME_INTERVAL = 600,
  NUM_TO_RUN          = 1,
  CONTAINER_TYPE      = 'podman',
  param.dir = here::here('currentVersion',''),
  output.dir = here::here('Atlantis_Runs',batch.prefix,'/')
)
