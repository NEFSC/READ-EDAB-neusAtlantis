#Script to create initial scallop biomass distribution from survdat pull

# remotes::install_github("NEFSC/NEFSC-Spatial")
library(sf)
library(terra)
library(dplyr)

#Calculate proportion of each survey strata attributable to each atlantis box
data("Shellfish_Strata",package = 'NEFSCspatial')
force(Shellfish_Strata)
survdat.shp = terra::vect(Shellfish_Strata)

survdat.area.orig = data.frame(STRATA_ID = survdat.shp$STRATA_ID,
                               area.tot = terra::expanse(survdat.shp))

neusBox <- terra::vect(here::here("Geometry","gis","Neus_ll_0p01.shp"))
crs(survdat.shp) = crs(neusBox)

# select boxes. remove islands
boxids <- neusBox$BOX_ID[which(neusBox$BOX_ID != c("23","24"))]

neus.survdat.ls = list()
for(i in 1:length(boxids)){
  
  neus.box.shp = neusBox[which(neusBox$BOX_ID == boxids[i])]
  neus.survdat.shp = terra::mask(survdat.shp,neus.box.shp)
  
  neus.survdat.int = terra::intersect(survdat.shp,neus.box.shp)
  
  if(length(neus.survdat.int) == 0){
    next()
  }
  neus.survdat.ls[[i]] = data.frame(box =boxids[i],
                                    STRATA_ID = as.numeric(neus.survdat.int$STRATA_ID),
                               area = terra::expanse(neus.survdat.int)) %>%
    group_by(box,STRATA_ID)%>%
    summarise(area = sum(area,na.rm=T))%>%
    left_join(survdat.area.orig)%>%
    mutate(area.prop = area/area.tot)%>%
    rename(STRATUM = 'STRATA_ID')
}

neus.survdat.conv = bind_rows(neus.survdat.ls)

#Read in scallop data and take proportional biomass
data = readRDS(here::here('data-raw','scallopSurveyFromSurvdat.rds'))$survdat%>%
  select(STRATUM, YEAR, BIOMASS)%>%
  group_by(STRATUM, YEAR)%>%
  summarise(BIOMASS = mean(BIOMASS,na.rm=T))
  

data.join =neus.survdat.conv %>%
  left_join(data)%>%
  mutate(biomass.wgt = BIOMASS * area.prop)%>%
  group_by(YEAR,box)%>%
  summarise(biomass = sum(biomass.wgt,na.rm=T))%>%
  group_by(YEAR)%>%
  mutate(biomass.tot = sum(biomass,na.rm=T),
         biomass.prop = biomass/biomass.tot)%>%
  group_by(box)%>%
  summarise(biomass.prop = round(mean(biomass.prop,na.rm=T),2))%>%
  mutate(box =as.numeric(box))%>%
  arrange(box)

data.join$biomass.prop = round(data.join$biomass.prop/sum(data.join$biomass.prop),2)

new.prop =data.frame(box = 0:29)%>%
  left_join(data.join)%>%
  mutate(biomass.prop = ifelse(is.na(biomass.prop), 0, biomass.prop))

source(here::here('R','Calibration_Tools','edit_param_init_dist.R'))  
edit_param_init_dist(
  init.file = here::here('currentVersion','neus_init.nc'),
  fgs.file = here::here('currentVersion','neus_groups.csv'),
  group.name = 'SCA',
  new.props = new.prop$biomass.prop,
  overwrite = T,
  new.file.name = here::here('currentVersion','neus_init_test.nc'),
  bgm.file = here::here('Geometry','neus_tmerc_RM2.bgm')
)

