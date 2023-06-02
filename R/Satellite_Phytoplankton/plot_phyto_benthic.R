library(ncdf4)
library(dplyr)
library(atlantistools)
library(ggplot2)

run.dir = here::here('Atlantis_Runs','phyto_svel_1','phyto_svel_1_0','')
# run.dir = here::here('Atlantis_Runs','phyto_svel_1','phyto_svel_1_6','')

main.nc = nc_open(paste0(run.dir,'neus_output.nc'))

phyto.names = c('Diatom','PicoPhytopl','DinoFlag')

bps = atlantistools::load_bps(fgs = here::here('currentVersion','neus_groups.csv'),
                              init = here::here('currentVersion','neus_init.nc'))
bboxes = atlantistools::get_boundary(boxinfo = atlantistools::load_box(here::here('currentVersion','neus_tmerc_RM2.bgm')))

data = atlantistools::load_nc(paste0(run.dir,'neus_output.nc'),
                              select_groups = phyto.names,
                              select_variable = 'N',
                              fgs = here::here('currentVersion','neus_groups.csv'),
                              bps = bps,
                              bboxes = bboxes,
                              prm_run = here::here('currentVersion','at_run.prm'))

ggplot(data, aes(x= time, y = atoutput, color = factor(layer)))+
  geom_line()+
  facet_grid(species~polygon,scale = 'free_y')+
  ggsave(here::here('Figures','phyto_svel_1_0.png'))
  
