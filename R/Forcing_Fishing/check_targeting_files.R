proj.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'

source(paste0(proj.dir,'R/Forcing_Fishing/get_forcing_ts.r'))

experiment.id = 'eof_targeting_3'
experiment.dir = paste0(proj.dir,'currentVersion/',experiment.id)
setup = read.csv(paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv')) %>% 
  dplyr::mutate(mean.catch = NA)

orig.catch = get_forcing_ts(file_path = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts'),
                            code = NULL, time = 'year') |> 
  dplyr::rename(catch.orig = 'Value',
                year = 'Time') %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(value.orig = sum(catch.orig,na.rm=T)) %>% 
  dplyr::filter(year > 25) %>% 
  dplyr::pull(value.orig) %>% 
  mean()



for( i in 1:nrow(setup)){
  
  this.file = paste0(experiment.dir,'/total_catch_',setup$run.id[i],'.ts')
  base.catch.y = get_forcing_ts(file_path = this.file, code = NULL, time = 'year') |> 
    dplyr::rename(catch.orig = 'Value',
                  year = 'Time') %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(value = sum(catch.orig,na.rm=T)) %>% 
    dplyr::filter(year > 25)

  setup$mean.catch[i] = mean(base.catch.y$value,na.rm=T)
      
}

setup.diff =setup %>% 
  dplyr::mutate(real.catch.scalar = mean.catch/orig.catch,
                scalar.diff = real.catch.scalar - catch.scalar)
setup.diff %>% 
  group_by(catch.scalar) %>% 
  summarise(mean.diff = mean(scalar.diff,na.rm=T))

summary(setup.diff)


