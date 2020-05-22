dir = here::here()
roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/'
plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Summary/'
years = 1981:2014
# years = 1981
year.dirs = paste0(roms.dir,years,'/')

source(here::here('R','plot_ROMS_summary.R'))


for(i in 1:length(year.dirs)){
  
  plot_ROMS_summary(year.dir = year.dirs[i],
                  which.face = 0:150,
                  which.box = 0:29,
                  plot.hflux = T,
                  plot.statevar = T,
                  plot.ltlvar = T,
                  which.levels =4,
                  plot.dir = plot.dir,
                  plot.year = years[i],
                  box.z.key = here::here('Geometry','box_depth_key.csv'),
                  bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
  )
  print(years[i])
}


