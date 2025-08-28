#Process ecosystem state from run set output using atlantiseof package
# system('sudo dnf install udunits2-devel')
# remotes::install_github('NOAA-EDAB/atlantiseof@joe_branch')
# remotes::install_github('NOAA-EDAB/stocksmart')
# install.packages('R.utils')
library(tictoc)
`%>%` = dplyr::`%>%`
#Get setup file
experiment.id = 'catch_thresholds_eof_3'
setup.df = read.csv(here::here('Setup_Files','catch_thresholds_eof_setup.csv'))

#make output directory
output.dir = paste0('/atlantisdisk/',experiment.id,'/analysis/')
if(!dir.exists(output.dir)){
  system(paste0('sudo mkdir ',output.dir))
}

run.dirs = paste0('/atlantisdisk/',experiment.id,'/',experiment.id,'_',setup.df$run,'/')
#Convert detailed diet for each run

survdat.url = "https://github.com/NOAA-EDAB/atlantiseof/raw/refs/heads/dev/data-raw/survey_lenagewgt.rds"
temp_file = tempfile(fileext = '.rds')
download.file(survdat.url, destfile = temp_file, mode = 'wb')
survdat.data = readRDS(temp_file)

i=1
for(i in 1:length(run.dirs)){

  run.ind.t = atlantiseof::make_eco_indicators_time(param.dir = here::here('currentVersion'),
                                    atl.dir = run.dirs[i],
                                   group.index =  url('https://raw.githubusercontent.com/NOAA-EDAB/atlantiseof/refs/heads/joe_branch/data-raw/neus_species_index.csv'),
                                   fgs.file = here::here('currentVersion','neus_groups.csv'),
                                   dietSource = 'detdiet',
                                   timeRange = 1:5,
                                   cloud = T,
                                   survdat.data =survdat.data
                                   )
  
  run.ind.mean = atlantiseof::make_eco_indicators(param.dir = here::here('currentVersion'),
                                                       atl.dir = run.dirs[i],
                                                       group.index =  url('https://raw.githubusercontent.com/NOAA-EDAB/atlantiseof/refs/heads/joe_branch/data-raw/neus_species_index.csv'),
                                                       fgs.file = here::here('currentVersion','neus_groups.csv'),
                                                       dietSource = 'detdiet',
                                                       timeRange = 1:5,
                                                  cloud = T
  )
  
  param.ls = atlantisprocessing::get_atl_paramfiles(param.dir = here::here('currentVersion'),
                                         atl.dir = run.dirs[i],
                                         run.prefix  = 'neus_output',
                                         include_catch = T
                                        )
  
  #do some post processing
  system(paste0('sudo mkdir ',run.dirs[i],'data'))
  system(paste0('sudo chmod -R 777 ',run.dirs[i],'data'))
  if(run.ind.mean$catch.tot == 0){
    atlantisprocessing::process_atl_output(param.dir = here::here('currentVersion'),
                                           atl.dir = run.dirs[i],
                                           out.dir = paste0(run.dirs[i],'data'),
                                           run.prefix = 'neus_output',
                                           param.ls = param.ls,
                                           plot.length.age = T,
                                           plot.biomass.timeseries = T,plot.numbers.timeseries = T,plot.catch = F)
  }else{
    atlantisprocessing::process_atl_output(param.dir = here::here('currentVersion'),
                                           atl.dir = run.dirs[i],
                                           out.dir = paste0(run.dirs[i],'data'),
                                           run.prefix = 'neus_output',
                                           param.ls = param.ls,
                                           plot.length.age = T,
                                           plot.biomass.timeseries = T,plot.numbers.timeseries = T,plot.catch = T)
  }

                                         
  #calculate PPR and PPC
  ppc = atlantiseof::get_ppc(param.dir = here::here('currentVersion',''),
                             atl.dir = run.dirs[i],
                             fgs = here::here('currentVersion','neus_groups.csv'),
                             dietSource = 'detDiet',
                             timeRange = 1:5)
  
  #Write to atlantisarchive for download
  export.dir = paste0('/atlantisarchive/Joseph.Caracappa/',experiment.id,'/',experiment.id,'_',i,'/')
  system(paste0('sudo mkdir -p ',export.dir))
  system(paste0('sudo chmod 777 -R ',export.dir))
  files.export = c('biomass.rds','biomass_age.rds','length_age.rds','numbers_age.rds','catch.rds')
  file.copy(paste0(run.dirs[i],'data/',files.export),paste0(export.dir,files.export))
  
  file.copy(paste0(run.dirs[i],'neus_outputDetDiet_processed.gz'),paste0(export.dir,'neus_outputDetDiet_processed.gz'))
  
  saveRDS(run.ind.t,paste0(export.dir, 'eco_indicators_ts.rds'))
  saveRDS(run.ind.mean, paste0(export.dir,'eco_indicators_mean.rds'))
  saveRDS(ppc, paste0(export.dir,'ppc.rds'))
  
}
