source(here::here('R','edit_param_pprey.R'))

new.pprey = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Diet/DOG_cannibal.csv',as.is = T)

atl.dir  = here::here('currentVersion','/')

benth.eat = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Diet/DOG_cannibal.csv')

edit_param_pprey(
  atl.dir = atl.dir,
  biol.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis2/currentVersion/at_biology.prm',
  fgs.file = paste0(atl.dir,'neus_groups.csv'), 
  pred.list = 'SCA',
  prey.list = 'PL',
  pprey.vals = '0.05',
  overwrite = T,
  new.file.name = NA
)atl.dir  = here::here('currentVersion','/')

