source(here::here('R','edit_param_init_scalar.R'))

init = get_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                      groups.file = here::here('currentVersion','neus_groups.csv'),
                      write.output = F)
spp = 'HAL'

new.init = init
current.scale = init$init.scalar[which(init$group == spp)]
new.scale = current.scale * 0.1
new.init$init.scalar[which(init$group == spp)] = new.scale

edit_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                       groups.file = here::here('currentVersion','neus_groups.csv'),
                       new.init.scalar = new.init,
                       overwrite = T)
