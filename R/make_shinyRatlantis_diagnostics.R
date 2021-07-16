#Script to run sinyRAtlantis bio.prm diagnostics
library('shiny')
library('dplyr')
library('DT')
library('ncdf4')
library('stringr')
library('shinyrAtlantis')
library('ReactiveAtlantis')

###Define output and parameter directory
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/CheckPersist_1/'
param.dir = here::here('currentVersion')
####

bgm.file = paste0(param.dir,'/neus_tmerc_RM2.bgm')
grp.file = paste0(param.dir,'/neus_groups.csv')
bio.file = paste0(param.dir,'/at_biology.prm')

#at_biology.prm explore
obj.bio <- make.sh.prm.object(bgm.file, grp.file, bio.file)
sh.prm(obj.bio)

#init.nc explore
obj.init <- make.sh.init.object(bgm.file, nc.file)
sh.init(obj.init)

salinity.file = here::here('currentVersion','tsfiles','Annual_Files','GLORYS_tempsalt_force_2000.nc')
temperature.file = salinity.file
exchange.file = here::here('currentVersion','tsfiles','Annual_Files','flow_2000.nc')
cum.depth = c(0,50,120,300,500)

input.object <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)

sh.forcings(input.object)