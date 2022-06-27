source(here::here('R','edit_param_pprey.R'))

# new.pprey = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Diet/DOG_cannibal.csv',as.is = T)

atl.dir  = here::here('currentVersion','/')

diet.change = read.csv('C:/Users/joe92/Documents/Atlantis/Diet Changes/Diet_Audit_Adjust_06242022.CSV')

get_pprey_vals(atl.dir = atl.dir,
               biol.file = here::here('currentVersion','at_biology.prm'),
               fgs.file = here::here('currentVersion','neus_groups.csv'),
               spp.names = 'LOB',
               is.pred = F,
               remove.zero = T)
            
edit_param_pprey(
  atl.dir = atl.dir,
  # biol.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/at_biology.prm',
  biol.file = here::here('currentVersion','at_biology.prm'),
  fgs.file = paste0(atl.dir,'neus_groups.csv'),
  pred.list = diet.change[,1],
  prey.list = diet.change[,2],
  pprey.vals = diet.change[,3],
  overwrite = T,
  new.file.name = NA
)

