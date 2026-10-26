#Script to create scallop distriubtion by box from survey
library(terra)
library(dplyr)
library(ggplot2)

data.orig = readRDS(here::here('data','sweptAreabiomassScallop.RDS')) |> 
  filter(variable == 'tot.biomass') |> 
  mutate(ID = 1:n()) |>
  as.data.frame()

# neusBox <- sf::st_read(here::here("Geometry","gis"),layer="Neus_ll_0p01",quiet=T)

# select boxes. remove islands
# boxids <- neusBox |> 
#   dplyr::filter(BOX_ID != c("23","24") ) |>
#   dplyr::pull(BOX_ID)

# #FALL
# biomassNEUSfall <- NULL
# for (boxid in boxids){ 
#   biomassBox <- survdat::calc_swept_area(surveyData=data.orig,
#                                          areaPolygon = neusBox,
#                                          areaDescription="BOX_ID", 
#                                          filterByArea = boxid, 
#                                          filterBySeason = 'all',
#                                          tidy=T)
#   biomassBox$box <- boxid
#   biomassNEUSfall <- rbind(biomassNEUSfall,biomassBox)
# }
# # 
# data.unique.coords <- data.orig |>
  # dplyr::mutate(unique_spatial_id = row_number()) # Create a unique ID for each distinct coordinate pair

#Get NEUS shape file
# neus.shp = terra::vect(here::here('Geometry','gis','NEUS_ll_0p01.shp'))
# neus.shp.names = terra::names(neus.shp)
# print(neus.shp.names)
# box.name = 'BOX_ID'
# plot(neus.shp)

#convert survey coordinates to spatvector
# data.coords = terra::vect(data.unique.coords,geom = c('LON','LAT'),crs = 'EPSG:4326')
# 
# if (terra::crs(data.coords) != terra::crs(neus.shp)) {
#   data.coords <- terra::project(data.coords, terra::crs(neus.shp))
# }
# 
# #Extract box names at points in data.coords
# extracted.data = terra::extract(neus.shp, data.coords)
# extracted.box = extracted.data |> 
#   select(id.y, BOX_ID)
# 
# #Join unique points with names
# data.with.box = data.unique.coords |>
#   left_join(extracted.box, by = c('unique_spatial_id' = 'id.y'))

# ggplot(data.with.box,aes(x = LON, y = LAT, color = BOX_ID))+
#   geom_point()

sca.surv.box = data.orig |> 
  # left_join(data.with.box, by = c('LAT','LON')) |> 
  filter(variable == 'tot.biomass') |>
  group_by(YEAR,box) |> 
  summarise(biomass = sum(value,na.rm=T)) |> 
  mutate(box = as.numeric(box),
         biomass= ifelse(box %in% c(0,23:29),0,biomass) ,
         YEAR = as.numeric(YEAR))
  

#Get proportion of biomass per box by year
sca.surv.tot = sca.surv.box |>
  group_by(YEAR) |> 
  summarise(biomass.tot = sum(biomass,na.rm=T))

sca.surv.box.prop = sca.surv.box |>
  left_join(sca.surv.tot)|>
  mutate(biomass.prop = biomass/biomass.tot,
         box = as.numeric(box)) |> 
  arrange(YEAR,box)

ggplot(sca.surv.box.prop, aes(x = box, y = biomass.prop, group = as.factor(YEAR),color = as.factor(YEAR)))+
  geom_line()

sca.1900s = sca.surv.box.prop |>
  filter(YEAR <2000)|>
  group_by(box)|>
  summarise(biomass.prop = mean(biomass.prop,na.rm=T))


sca.all = sca.surv.box.prop |>
  group_by(box)|>
  summarise(biomass.prop = mean(biomass.prop,na.rm=T))

sca.1900s.prop = rep(0, 30)
box.match.1900s = match(sca.1900s$box,0:29)
sca.1900s.prop[box.match.1900s[!is.na(box.match.1900s)]] = sca.1900s$biomass.prop[!is.na(box.match.1900s)] 
sca.1900s.prop = sca.1900s.prop/sum(sca.1900s.prop)


# box.match.all = match(sca.all$box,0:29)
# sca.all.prop = box.blank[box.match.all[!is.na(box.match.all)]] = sca.all$biomass.prop[!is.na(box.match.all)] |>
#   signif(2)


source(here::here('R','Calibration_Tools','edit_param_init_dist.R'))

#manually adjust box 7 to be lower for calibration
sca.1900s.prop[c(5,8)] = 0.005
sca.1900s.prop=sca.1900s.prop/sum(sca.1900s.prop)

edit_param_init_dist(init.file = here::here('currentVersion','neus_init.nc'),
                     fgs.file = here::here('currentVersion','neus_groups.csv'),
                     group.name = 'SCA',
                     new.props = sca.1900s.prop,
                     overwrite = T,
                     bgm.file = here::here('currentVersion','neus_tmerc_RM2.bgm'))
