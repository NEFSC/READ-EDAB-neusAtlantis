#Script to scale Mum_C for age groups
library(dplyr)
source(here::here('R','edit_param_BH.R'))

#Write all groups 
# base.BH = readRDS(here::here('data','reference_BH_params.rds'))
base.BH = get_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'))%>%
  mutate(alpha = as.numeric(alpha),beta = as.numeric(beta))

# edit_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'),
#               group.name = base.BH$Code,
#               alpha = base.BH$alpha,
#               beta = base.BH$beta,
#               overwrite = T)

group.name = 'GOO'

alpha.scale = 2
beta.scale =0.5

new.alpha = base.BH$alpha[which(base.BH$group == group.name)]*alpha.scale
new.beta = base.BH$beta[which(base.BH$group == group.name)]*beta.scale

edit_param_BH(bio.prm = here::here('currentVersion','at_biology.prm'),
              group.name = group.name,
              alpha = new.alpha,
              beta = new.beta,
              overwrite = T
              )
