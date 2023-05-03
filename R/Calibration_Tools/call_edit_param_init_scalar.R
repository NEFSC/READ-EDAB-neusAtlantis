#wrapper to edit init scalars

source(here::here('R','edit_param_init_scalar.R'))

run.prm = here::here('currentVersion','at_run.prm')
fgs = here::here('currentVersion','neus_groups.csv')

init.scalar = get_param_init_scalar(run.prm = run.prm,
                      groups.file = fgs)

spp.change = c('SDF','LOB','MAK')
new.scalar = c(1.5,1.5,1.5)

for(i in 1:length(spp.change)){
  id = which(init.scalar$group == spp.change[i])
  init.scalar$init.scalar[id] = new.scalar[i]
}

edit_param_init_scalar(run.prm = run.prm,
                       groups.file = fgs,
                       new.init.scalar = init.scalar,
                       overwrite = T)
