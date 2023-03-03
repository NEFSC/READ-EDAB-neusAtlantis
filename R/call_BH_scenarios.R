repo.dir = '/net/work3/EDAB/atlantis/Joe_Proj/'
batch.dir = 'BH_convert_1'
dir.create(paste0('/net/work3/EDAB/atlantis/Shared_Data/',batch.dir,'/'))

source(paste0(repo.dir,'/R/atlantis_batcher.r'))

atlantis_batcher(
  batcherFilename = paste0(repo.dir,'/Setup_Files/',paste0(batch.dir,'.csv')),
  userName            = 'jcara',
  CHECK_TIME_INTERVAL = 60,
  NUM_TO_RUN          = 12,
  CONTAINER_TYPE      = 'podman',
  param.dir = paste0(repo.dir,'currentVersion/'),
  output.dir = '/net/work3/EDAB/atlantis/Shared_Data/'
)