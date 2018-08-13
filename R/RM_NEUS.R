###----------------------------------------------------------------
### USING ShinyRAtlantis TO DISPLAY AND CHANGE DATA FOR NEUS ATLANTIS v1.0 and v1.5 - RM 20170214
### data may be loaded from saved work:
### load("H:/1 RM/0 R workspaces/NEUS_shiny_initial_conditions_and_biol_PRM_v15_vs_v10.RData")
###________________________________________________

# library(shinyrAtlantis)
# library(stringr)
### example files ####
# # create distribution 
# bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
# obj <- make.sh.dist.object(bgm.file)
# sh.dist(obj)
# 
# # create prm file
# bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
# grp.file <- system.file("extdata", "AntarcticGroups.csv", package = "shinyrAtlantis")
# prm.file <- system.file("extdata", "SO90_biol.prm", package = "shinyrAtlantis")
# obj <- make.sh.prm.object(bgm.file, grp.file, prm.file)
# sh.prm(obj)
# 
# 
# # create forcings 
# salinity.file    <- "GBR108_salt.nc"       # this file is not included in the package
# temperature.file <- "GBR108_temp.nc"       # this file is not included in the package
# bgm.file         <- "gbr_box_03012012.bgm" # this file is not included in the package
# cum.depth <- c(0,5,10,20,50,100,200,3000)  # cumulative water layer depths
# input.object <- make.sh.forcings.object(
#   bgm.file         = bgm.file,
#   exchange.file    = exchange.file,
#   cum.depth        = cum.depth,
#   temperature.file = temperature.file,
#   salinity.file    = salinity.file
# )
# sh.forcings(input.object)
# 
# 
# # Create init file
# bgm.file <-system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
# nc.file <- system.file("extdata", "input.nc", package = "shinyrAtlantis")
# obj <- make.sh.init.object(bgm.file, nc.file)
# sh.init(obj)

#######################################
# NEUS v1.5
##### Horizontal distribution creator - enter depth range, gives probability within boxes, copy to PRM file
library(shinyrAtlantis)
library(tidyverse)
library(stringr)
library(rbgm)
library(bgmfiles)

wd=getwd()


### Plot NEUS (from rbgm package)
# wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
# setwd(wd2)
# bgm.file <- ("neus30_v15.bgm")
# bgm=bgmfile(bgm.file)
# plot(boxSpatial(bgm), axes=T, col=grey(seq(0,1, length=nrow(bgm$boxes))))
# 
# (sldf=faceSpatial(bgm))
# plot(sldf, col = rainbow(nrow(sldf)), lwd = 2, add=T)
# text(do.call(rbind, lapply(coordinates(sldf), function(x) apply(x[[1]], 2, mean))), 
#      labels = gsub("ace", "", sldf$label), cex = 0.5, col = rainbow(nrow(sldf)), pos = 3)
# 
# breakproj <- function(x) {
#   paste(strsplit(x, " ")[[1]], collapse = "\n")
# }
# boxes <- boxSpatial(bgm)
# plot(boxes, axes=T)
# text(coordinates(boxes), lab = bgm$boxes$.bx0)
# plot(boundarySpatial(bgm), lwd = 4, col = "grey")
# plot(boxSpatial(bgm), add = TRUE)

library(mapview)
library(sf)
library(rbgm)
library(bgmfiles)
library(viridis)
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
bgm.file <- ("neus30_v15.bgm")
neus <- bgmfile(bgm.file)
box <- boxSpatial(neus)
# projection(box) <- "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=30000000 +scale=1"
projection(box) <- "+proj=tmerc +lat_0=40.5 +lon_0=-70.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +x_0=1000000 +y_0=3000000 +k=1"
box$colour <- substr(viridis::viridis(nrow(box)), 1, 7)
box$colour[box$boundary] <- NA
mapview(box, color = box$colour)

#________________________________________________________________________________________
### CREATE FORCING FILES FOR VIEWING WITH SHINYRATLANTIS ###
###
###
# ran Cam Ainsworth's checkwinding program and found face order for faces 37,63,74,106,and 107
# were reversed in BGM file (means flows went opposite direction), fixed in Test_Winding_passes.bgm, need to fix in others

#testing new hydro data 20170925
wd="/home/ryan/Git/temp"
wd2="/home/ryan/Git/atneus_RM"
# salinity.file='/home/ryan/Git/temp/NEUS_saln_365.nc'
# salinity.file='/home/ryan/Git/temp/NEUS_saln_2008_1.nc'
salt='/home/ryan/Git/temp/RM_salt_test.nc'
# temperature.file='/home/ryan/Git/temp/NEUS_temp_365.nc'
# temperature.file='/home/ryan/Git/temp/NEUS_temp_2008_1.nc'
temp='/home/ryan/Git/temp/RM_temp_test.nc'
# exchange.file='/home/ryan/Git/RM_NEUS_hydro_2008_1.nc' # old file, pre depth correction
# exchange.file='/home/ryan/Git/temp/RM_NEUS_hydro_2008_1.nc'
exchange.file='/home/ryan/Git/temp/RM_hyrdo_test.nc'
bgm.file="/home/ryan/Git/atneus_RM/test_winding_passes.bgm"
cum.depth <- c(0,50,120,300,500)

# 10m_dz vertical decimation
salt='/home/ryan/Git/temp/10m_dz/salt3.nc' #'/home/ryan/Git/temp/archive/cat/salt3.nc' #'salt.nc'
temp='/home/ryan/Git/temp/10m_dz/temp3.nc' #'/home/ryan/Git/temp/archive/cat/temp3.nc' #'temp.nc'
hydro='/home/ryan/Git/temp/10m_dz/hydro3.nc' #'/home/ryan/Git/temp/archive/cat/hydro3.nc' #'hydro.nc'

#25m_dz vertical decimation
salt='/home/ryan/Git/temp/archive/cat/salt2.nc' #'salt.nc'
temp='/home/ryan/Git/temp/archive/cat/temp2.nc' #'temp.nc'
hydro='/home/ryan/Git/temp/archive/cat/hydroF.nc' #'hydro.nc'

#10m_dz vertical decimation, new dz structure from Cecilie Hansen, ALL BOXES
salt='/home/ryan/Git/temp/10m_dz_allBoxes_newdzStructure/salt3.nc'
temp='/home/ryan/Git/temp/10m_dz_allBoxes_newdzStructure/temp3.nc'
hydro='/home/ryan/Git/temp/10m_dz_allBoxes_newdzStructure/hydro3.nc'

# 5m_dz all boxes, new dz order
hydro='/home/ryan/Git/temp/5m_dz/hydro3.nc'
salt='/home/ryan/Git/temp/5m_dz/salt3.nc'
temp='/home/ryan/Git/temp/5m_dz/temp3.nc'

# 1m_dz all boxes, new dz order
hydro='/home/ryan/Git/temp/1m_dz/hydro.nc'
salt='/home/ryan/Git/temp/1m_dz/salt3.nc'
temp='/home/ryan/Git/temp/1m_dz/temp3.nc'

# 50m_dz, dynamic boxes only
hydro='/home/ryan/Git/temp/50m_dz_dynamicboxesonly/hydro3.nc'
salt='/home/ryan/Git/temp/50m_dz_dynamicboxesonly/salt3.nc'
temp='/home/ryan/Git/temp/50m_dz_dynamicboxesonly/temp3.nc'

# 50m_dz, all boxes, modded flux code 10/24/2017
hydro='/home/ryan/Git/temp/50m_dz_allboxes_codemod/hydroF.nc'
salt='/home/ryan/Git/temp/50m_dz_allboxes_codemod/salt3.nc'
temp='/home/ryan/Git/temp/50m_dz_allboxes_codemod/temp3.nc'

# 50m_dz, dyn boxes u_east and v_north 20171206
hydro='/home/ryan/Git/temp/50m_dynboxes_20171206/hydro3.nc'
salt='/home/ryan/Git/temp/50m_dynboxes_20171206/salt3.nc'
temp='/home/ryan/Git/temp/50m_dynboxes_20171206/temp3.nc'

hydro='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180103/flowOut.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180103/saltOut.nc'
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180103/tempOut.nc'

temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/2009data/tempOut2009.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/2009data/saltOut2009.nc'
hydro='/home/ryan/Dunn/RM hydro/ROMS_doppio/2009data/flowOut2009.nc'

# new hydro, fixed code 20180325, direction misrepresented before, now flows are dyanmic and weak or static and high...
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/2008_fix_20180325/tempOut2008_fix_0325.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/2008_fix_20180325/saltOut2008_fix_0325.nc'
hydro='/home/ryan/Dunn/RM hydro/ROMS_doppio/2008_fix_20180325/flowOut2008_fix_0325.nc'

# try scaling flows in hydroconstruct by 3600 -> does not seem to have effect... (hyperdiffusion fix)
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0328/tempOut2008_fix_0328.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0328/saltOut2008_fix_0328.nc'
hydro='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0328/flowOut2008_fix_0328.nc'

### remove hyperdiffusion fix, scale set to 1
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flowOut2008_fix_0329.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/saltOut2008_fix_0329.nc'
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/tempOut2008_fix_0329.nc'

# used flux time (=flux * 3600) used with hyperdiffusion fix on flows
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/fluxtime hyper fix/tempOut2008_fix_0329b.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/fluxtime hyper fix/saltOut2008_fix_0329b.nc'
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/fluxtime hyper fix/flowOut2008_fix_0329b.nc'

temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/tempOut2008_fix_0329a.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/saltOut2008_fix_0329a.nc'
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/flowOut2008_fix_0329a.nc'

# fluxtime, hyper fix option 2
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/better full box hyper fix /tempOut2008_fix_0329a.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/better full box hyper fix /saltOut2008_fix_0329a.nc'
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/better full box hyper fix /flowOut2008_fix_0329a.nc'

# flux hyper fix 2
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/better full box hyper fix /orig flux hyper fix opt2/saltOut2008_fix_0329a.nc'
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/better full box hyper fix /orig flux hyper fix opt2/tempOut2008_fix_0329a.nc'
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0329/flux hyper fix/better full box hyper fix /orig flux hyper fix opt2/flowOut2008_fix_0329a.nc'

# fluxtime, hyperfix 1
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/tempOut2008_fix_0330fluxtime1.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/saltOut2008_fix_0330fluxtime1.nc'
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/flowOut2008_fix_0330fluxtime1.nc'

# flux, no hyper fix
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/tempOut2008_fix_0330flux0.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/saltOut2008_fix_0330flux0.nc'
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/flowOut2008_fix_0330flux0.nc'

#fluxtime, hyper=2 (no hyper did not work)
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/flowOut2008_fix_0330fluxtime2.nc'
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/tempOut2008_fix_0330fluxtime2.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/saltOut2008_fix_0330fluxtime2.nc'

# fluxtime, hyper=2, dt=86400
flow='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/flowOut2008_fix_0330fluxtime2_a.nc'
temp='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/tempOut2008_fix_0330fluxtime2_a.nc'
salt='/home/ryan/Dunn/RM hydro/ROMS_doppio/20180327/0330/saltOut2008_fix_0330fluxtime2_a.nc'

test<- make.sh.forcings.object(
  bgm.file         = "/home/ryan/Git/atneus_RM/neus_tmerc_RM.bgm",
  exchange.file    = hydro,
  cum.depth        = c(0,50,120,300,500),
  temperature.file = temp,
  salinity.file    = salt
)
sh.forcings(test)


### SETAS linux added 20180327
hydro='/home/ryan/AtlRuns/SETAS/SETas_model_New_Trunk/inputs/forcisets/SETAS_VMPAhydroA.nc'
salt='/home/ryan/AtlRuns/SETAS/SETas_model_New_Trunk/inputs/forcisets/SETAS_VMPAsalt.nc'
temp='/home/ryan/AtlRuns/SETAS/SETas_model_New_Trunk/inputs/forcisets/SETAS_VMPAtemp.nc'
bgm.file='/home/ryan/AtlRuns/SETAS/SETas_model_New_Trunk/VMPA_setas.bgm'
setasZ=rev(c(1300, 450, 150, 50, 30, 20, 0)) # taken from initial conditions dz
setas<- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = hydro,
  cum.depth        = cum.depth,
  temperature.file = temp,
  salinity.file    = salt
)
sh.forcings(setas) # does not seem to be right... 20180328





# Create forcings - NC files shortened to remove wonky wobbles at end 730 days (2 years) -> 644 days, new BGM
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
salinity.file    <- "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles/Salt_neus_RM_shortened.nc" #Salt_neus.nc"
salinity.file='/home/ryan/Git/atneus_RM/tsfiles/Salt_neus_RM_shortened.nc'
temperature.file <- "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles/Temp_neus_RM_shortened.nc" #Temp_neus.nc"
temperature.file='/home/ryan/Git/atneus_RM/tsfiles/Temp_neus_RM_shortened.nc'
exchange.file    <- "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles/Hydro_neus_RM_shortened.nc"
exchange.file='/home/ryan/Git/atneus_RM/tsfiles/Hydro_neus_RM_shortened.nc'
bgm.file         <- "test_winding_passes.bgm" #neus30_v15_notsohighvertmix.bgm" #"neus30_v15.bgm"
cum.depth <- c(0,50,120,300,500)  # cumulative water layer depths
# cum.depth <- c(0,25,75,125,200)  # cumulative water layer depths
# cum.depth <- c(50,70,180,200,500)  # cumulative water layer depths
# cum.depth <- c(0,25,50,75,100,150,200)  # cumulative water layer depths from nominal dz init file
NEUS_1.5.TestWinding_shortened_force <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)
### Create forcings - as original NEUS 1.0, but with S and T orders switched to proper place
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
salinity.file    <- "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles/RM_salt2.nc"
temperature.file <- "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles/RM_Temp_neus.nc"
exchange.file    <- "C:/Users/ryan.morse/Documents/GitHub/atneus_RM/tsfiles/Hydro_neus.nc"
bgm.file         <- "neus30_v15.bgm"
cum.depth <- c(0,50,120,300,500)  # cumulative water layer depths
# cum.depth <- c(0,25,75,125,200)  # cumulative water layer depths
# cum.depth <- c(50,70,180,200,500)  # cumulative water layer depths
# cum.depth <- c(0,25,50,75,100,150,200)  # cumulative water layer depths from nominal dz init file
NEUS_1.5_STreorder_force <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)

### TEST DIFFERENCE between 2 new sets of forcings (still wrong) to see if BGM file values made a difference in creating files:
#----------------------------------------------------------------------------------
### NEW 20170711 forcings with neus_30_v15
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
salinity.file    <- "C:/Users/ryan.morse/Desktop/AtlantisGuide/fromJason/AustraliaCSIRO/flows/Dunn/RM made with converted flows and neus30v15 bgm/20170711RMneussalt.nc"
temperature.file <- "C:/Users/ryan.morse/Desktop/AtlantisGuide/fromJason/AustraliaCSIRO/flows/Dunn/RM made with converted flows and neus30v15 bgm/20170711RMneustemp.nc"
exchange.file    <- "C:/Users/ryan.morse/Desktop/AtlantisGuide/fromJason/AustraliaCSIRO/flows/Dunn/RM made with converted flows and neus30v15 bgm/20170711RMneushydro.nc"
bgm.file         <- "neus30_v15.bgm" 
cum.depth <- c(0,50,120,300,500)  # cumulative water layer depths
NEUS_1.5_originalbgm <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)
# NEW 20170711 forcings with neus_30_v15_notsohighvertmix
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
salinity.file    <- "C:/Users/ryan.morse/Desktop/AtlantisGuide/fromJason/AustraliaCSIRO/flows/Dunn/RM made with converted flows and notsohighvertmixBGM/20170711RMneussalt2.nc"
temperature.file <- "C:/Users/ryan.morse/Desktop/AtlantisGuide/fromJason/AustraliaCSIRO/flows/Dunn/RM made with converted flows and notsohighvertmixBGM/20170711RMneustemp2.nc"
exchange.file    <- "C:/Users/ryan.morse/Desktop/AtlantisGuide/fromJason/AustraliaCSIRO/flows/Dunn/RM made with converted flows and notsohighvertmixBGM/20170711RMneushydro2.nc"
bgm.file         <- "neus30_v15_notsohighvertmix.bgm" #"neus30_v15.bgm"
cum.depth <- c(0,50,120,300,500)  # cumulative water layer depths
NEUS_1.5_notsohighvertmix <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)
#-----------------------------------------------------------------------------------
#linux
wd2='/home/ryan/Git/atneus_RM'
setwd(wd2)
salinity.file    <- 'RM_salt2.nc' #"Salt_neus.nc"
temperature.file <- 'RM_Temp_neus.nc' #"Temp_neus.nc"
exchange.file    <- "Hydro_neus.nc"
bgm.file         <- "test_winding_passes.bgm" #neus30_v15.bgm"
cum.depth <- c(0,50,120,300,500)  # cumulative water layer depths
# cum.depth <- c(0,50,100,150,200)  # cumulative water layer depths
# cum.depth <- c(0,50,120,300,500)  # cumulative water layer depths
# cum.depth <- c(0,25,50,75,100,150,200)  # cumulative water layer depths from nominal dz init file

#SETAS
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
salinity.file    <- 'D:/SETAS/inputs/forcisets/SETAS_VMPAsalt.nc'
temperature.file <- 'D:/SETAS/inputs/forcisets/SETAS_VMPAtemp.nc'
exchange.file    <- 'D:/SETAS/inputs/forcisets/SETAS_VMPAhydroA.nc'
bgm.file         <- 'D:/SETAS/VMPA_setas.bgm'
cum.depth <- c(0,5,10,20,50,100,200,3000)
SETAS_force <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)

# NEUS v1.0 (from Rob Gamble)
wd4='D:/AtlantisRun/NEUS v1 Base Effort'
setwd(wd4)
exchange.file    <- 'D:/AtlantisRun/NEUS v1 Base Effort/Hydro_neus.nc'
salinity.file    <- 'D:/AtlantisRun/NEUS v1 Base Effort/Salt_neus.nc'
temperature.file <- 'D:/AtlantisRun/NEUS v1 Base Effort/Temp_neus.nc'
bgm.file         <- 'D:/AtlantisRun/NEUS v1 Base Effort/neus30_2012_old.bgm'
cum.depth <- c(0,50,120,300,500)
NEUS_1.0_force <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)


### VIEW FORCING FILES CREATED ABOVE ####
sh.forcings(NEUS_1.0_force) # original NEUS 1.0 files
sh.forcings(NEUS_1.5_STreorder_force) # update bgm, reorder S and T
sh.forcings(NEUS_1.5.shortened_force) # change vertmix, horixmic bgm, time series shortend 730->644 days
sh.forcings(NEUS_1.5.TestWinding_shortened_force)

# NEW RM forcing flows 2017 (still wrong) see if BGM file makes a difference
sh.forcings(NEUS_1.5_originalbgm)
sh.forcings(NEUS_1.5_notsohighvertmix)

#leftover name...
sh.forcings(input.object)
#__________________________________________________________________________________________


### View and CHANGE spatial distributions 
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
setwd(wd2)
bgm.file <- ("neus30_v15_notsohighvertmix.bgm")
NEUS_15_dist <- make.sh.dist.object(bgm.file)
sh.dist(NEUS_15_dist)
#______________




### View initial conditions file and box structure
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_gavdev/atneus'
wd2='/home/ryan/Git/atneus_RM'
setwd(wd2)

NEUS_15_gav_init <- make.sh.init.object("neus30_v15.bgm", "new_init.nc")
NEUS_15_init <- make.sh.init.object("neus30_v15.bgm", "RMinit_2.nc") #changed sed deposit feed, bact, meiobenthos
NEUS_15_RM3init=make.sh.init.object('C:/Users/ryan.morse/Documents/GitHub/atneus_RM/neus30_v15.bgm', 'C:/Users/ryan.morse/Documents/GitHub/atneus_RM/RMinit_3test.nc')
NEUS_15_newestinit=make.sh.init.object(
  'C:/Users/ryan.morse/Documents/GitHub/atneus_RM/neus_tmerc_RM.bgm',
  # 'C:/Users/ryan.morse/Documents/GitHub/atneus_RM/RMinit_newvalues2017.nc'
  'C:/Users/ryan.morse/Documents/GitHub/atneus_RM/RMinit_2018.nc'
)

# 20180620
bgm.file <- ("neus_tmerc_RM.bgm")
NEUS_15_init=make.sh.init.object(bgm.file, '20180710init.nc') #'RMinit_newvalues2017.nc')


wd3='C:/Users/ryan.morse/Atlantis-NEUS-1_0/SETAS'
wd3='/home/ryan/AtlRuns/SETAS/SETas_model_New_Trunk'
setwd(wd3)
SETAS_init <- make.sh.init.object("VMPA_setas.bgm", "INIT_VMPA_Jan2015.nc") #changed sed deposit feed, bact, meiobenthos

wd4='D:/AtlantisRun/NEUS v1 Base Effort'
# wd4='/home/ryan/AtlRuns/NEUS v1/Base Effort/rm'
wd4='/home/ryan/AtlRuns/NEUS v1/Base Effort'
setwd(wd4)
bgm.file         <- 'D:/AtlantisRun/NEUS v1 Base Effort/neus30_2012_old.bgm'
init.file         <- 'D:/AtlantisRun/NEUS v1 Base Effort/inneus_2007.nc'
NEUS_10_init <- make.sh.init.object(bgm.file, init.file) #changed sed deposit feed, bact, meiobenthos


sh.init(NEUS_15_init)
sh.init(NEUS_15_gav_init)
sh.init(NEUS_10_init)
sh.init(SETAS_init)
sh.init(NEUS_15_RM3init)
sh.init(NEUS_15_newestinit)

tt1=NEUS_10_init$df.nitrogen # check RN SN values for length weight relationships
tt15=NEUS_15_init$df.nitrogen # check RN SN values for length weight relationships
tts=SETAS_init$df.nitrogen # check RN SN values for length weight relationships

write.table(tts, file='SETAS_N.csv', col.names=T, row.names=F, sep=',')



grp.file=paste(wd2, '/NeusGroups_v15_unix.csv', sep='')
bgm.file=paste(wd2, '/neus_tmerc_RM.bgm', sep='')
cum.depths=c(0,50,120,300,500)
csv.name='RMinit_template.csv'
make.init.csv(grp.file, bgm.file, cum.depths, csv.name)


#________________



### View biology prm file NEUS v1.0 vs NEUS v1.5
# NEUS_15 prm file
wd2='C:/Users/ryan.morse/Documents/GitHub/atneus_RM'
wd2='/home/ryan/Git/atneus_RM'
setwd(wd2)
## bgm.file <- ("neus30_v15.bgm")
## grp.file <- ('NeusGroups_v15_unix.csv')
## prm.file <- ('at_biol_neus_v15_working_RM.prm')
## prm.file=('at_biol_neus_v15_scaled_diet_20170410.prm')
# update 7/21/17 RM# 
grp.file <- ('NeusGroups_v15_unix.csv') # ALL GROUPS
grp.file <- ('NeusGroups_v15_LTLonly.csv') # JUST LTL
bgm.file <- ("neus_tmerc_RM.bgm")
prm.file=('at_biol_neus_v15_scaled_diet_20180608.prm')#/home/ryan/Git/atneus_RM/at_biol_neus_v15_scaled_diet_20180227.prm
NEUS_15_prm <- make.sh.prm.object(bgm.file, grp.file, prm.file)
sh.prm(NEUS_15_prm)
# NEUS_10 prm file
wd4='D:/AtlantisRun/NEUS v1 Base Effort'
setwd(wd4)
bgm.file <- ("neus30_2012.bgm")
grp.file <- ('functionalGroups.csv')
prm.file=('at_biol_neus_DE.prm')
NEUS_10_prm <- make.sh.prm.object(bgm.file, grp.file, prm.file)
sh.prm(NEUS_10_prm)

wd3='C:/Users/ryan.morse/Atlantis-NEUS-1_0/SETAS'
wd3='/home/ryan/AtlRuns/SETAS/SETas_model_New_Trunk'
setwd(wd3)
bgm.file <- ("VMPA_setas.bgm")
grp.file <- ('SETasGroupsDem_NoCep.csv')
prm.file=('VMPA_setas_biol_fishing_Trunk.prm')
SETAS_prm <- make.sh.prm.object(bgm.file, grp.file, prm.file)
sh.prm(SETAS_prm)

#________________


