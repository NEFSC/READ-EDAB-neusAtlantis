
proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'

source(paste0(proj.dir,'/R/fishing_sensitivity/make_catch_file_projected_mean.R'))

make_catch_file_projected_mean(
  fgs.file = paste0(proj.dir,'currentVersion/neus_groups.csv'),
  original_catch_file = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts'),
  start.time = 20805-(365*10),
  end.time = 20805,
  duration = 365*100,
  new_catch_file =  paste0(proj.dir,'currentVersion/CatchFiles/total_catch_100yr_proj.ts'),
  overwrite = F
)
