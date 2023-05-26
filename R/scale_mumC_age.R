#Script to scale Mum_C for age groups
library(dplyr)
source(here::here('R','edit_param_mum_age.R'))
source(here::here('R','edit_param_C_age.R'))

group.names = 'HER'

mum.scale = 0.75
C.scale = 1.5

bio.prm = here::here('currentVersion','at_biology.prm')

mum.base.age = get_param_mum_age(bio.prm) %>%
  filter(group %in% group.names)%>%
  mutate_at(vars(mum1:mum10),funs(as.numeric))
c.base.age = get_param_C_age(bio.prm)%>%
  filter(group %in% group.names)%>%
  mutate_at(vars(C1:C10),funs(as.numeric))

group.id = which(group.names == mum.base.age[,1])

new.mum = mum.base.age[group.id,2:11]*mum.scale
new.C = c.base.age[group.id,2:11]*C.scale

edit_param_mum_age(bio.prm ,
                   overwrite = T,
                   new.mum = new.mum,
                   single.group = T,
                   group.name = group.names)

edit_param_C_age(bio.prm,
                 overwrite = T,
                 new.C = new.C,
                 single.group = T,
                 group.name = group.names)
