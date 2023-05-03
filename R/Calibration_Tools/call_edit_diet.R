library(dplyr)
source(here::here('R','edit_param_pprey.R'))

# new.pprey = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Diet/DOG_cannibal.csv',as.is = T)

atl.dir  = here::here('currentVersion','/')

# diet.change = read.csv('C:/Users/joe92/Documents/Atlantis/Diet Changes/Diet_Adjust_07082022.csv')
diet.change = read.csv(here::here('Setup_Files','MAK_fix_2.csv'))


X=get_pprey_vals(atl.dir = atl.dir,
               biol.file = here::here('currentVersion','at_biology.prm'),
               fgs.file = here::here('currentVersion','neus_groups.csv'),
               spp.names = 'MAK',
               is.pred = F,
               remove.zero = T)
write.csv(X, here::here('Setup_Files','MAK_fix_2.csv'),row.names = F)

group = 'MAK'
pred.names = c(paste0(1,group,1),paste0(2,group,1),paste0(1,group,2),paste0(2,group,2))
pred.ls = list()
for(i in 1:length(pred.names)){
  pred.ls[[i]] = get_pprey_vals(atl.dir = atl.dir,
                                biol.file = here::here('currentVersion','at_biology.prm'),
                                fgs.file = here::here('currentVersion','neus_groups.csv'),
                                spp.names = pred.names[i],
                                is.pred = T,
                                remove.zero = T)
  }
bind_rows(pred.ls)

# diet.change = data.frame(pred = pred.names, prey = 'GOO', value = c(.000025,0,0.000125,0))

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

# edit_param_pprey(
#   atl.dir = atl.dir,
#   # biol.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/at_biology.prm',
#   biol.file = here::here('currentVersion','at_biology.prm'),
#   fgs.file = paste0(atl.dir,'neus_groups.csv'),
#   pred.list = '2GOO2',
#   prey.list = 'GOO',
#   pprey.vals = 0.0001,
#   overwrite = T,
#   new.file.name = NA
# )

