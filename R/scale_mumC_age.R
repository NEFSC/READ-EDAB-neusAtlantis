#Script to scale Mum_C for age groups
source(here::here('Joe_Proj','R','edit_param_mum_age.R'))
source(here::here('Joe_Proj','R','edit_param_C_age.R'))

group.names = 'COD'

mum.scale = 1.216
C.scale = 1.216

bio.prm = here::here('Joe_Proj','currentVersion','at_biology.prm')

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
