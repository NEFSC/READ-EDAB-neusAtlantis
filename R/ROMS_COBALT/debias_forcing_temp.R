# Script to use NCEI climatology to debias ROMS forcing files

library(ncdf4)
library(dplyr)
library(sf)

source(here::here('R','flatten_force.R'))
source(here::here('R','nc_to_array.R'))
source(here::here('R','duplicate_force_year.R'))

#Converts ncei filename to date
filename2date = function(filename){
  file.split = strsplit(filename,'[_|.]+')[[1]]
  year.code =file.split[2]
  month.code = strsplit(file.split[3],'t')[[1]][2]
  if(year.code == '6574'){
    year.range ='1965-1974'
  } else if(year.code == '7584'){
    year.range = '1975-1984'
  } else if(year.code == '8594'){
    year.range = '1985-1994'
  } else if(year.code == '95A4'){
    year.range = '1995-2004'
  } else if(year.code == 'A5B2'){
    year.range = '2005-2012'
  } else {
    year.rangte = NA
  }
  month.name = months(as.Date(paste0('2000-',month.code,'-01')))
  out.list = list(year.range = year.range,month.name = month.name)
  return(out.list)
}

#Set directories
force.dir = here::here('currentVersion','tsfiles','Annual_Files')
ncei.dir = 'C:/Users/joseph.caracappa/Documents/NOAA_NCEI_NWA_Data/Temperature_monthly_1_10deg/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/bias_corrected_tempsalt/'

#box shape file
box.shp = read_sf(here::here('Geometry','neus_ll_0p01.shp'))

#Climatology codes
clim.df = data.frame(decade = c('7584','8594','95A4','A5B2'),start = c(1975,1985,1995,2005), end = c(1984,1994,2004,2014))
months =sprintf('%02d',1:12)
time.combs = expand.grid(clim.df$decade,months)
colnames(time.combs) = c('decade','month')
clim.key = left_join(clim.df,time.combs)
clim.key$time.id = 1:nrow(clim.key)


#list all tempsalt forcing files
force.files = list.files(force.dir,'roms_tempsalt*')
years = as.numeric(sort(gsub(".*(\\d{4}).+","\\1",force.files)))
force.files = force.files[which(years>=1981)]
years = as.numeric(sort(gsub(".*(\\d{4}).+","\\1",force.files)))

#Determine month-decade bias for each box/level
# temp.bias = array(NA,dim = c(4,30,))
roms.monthly.ls = list()
ncei.monthly.ls = list()
d=1



for(d in 1:nrow(clim.df)){
  
  force.dec.files = force.files[which(years >= clim.df$start[d] & years <= clim.df$end[d])]
  
  #Aggregate month,box,level temperature within Roms files for each decade
  roms.ls = list()
  f=1
  for(f in 1:length(force.dec.files)){
    roms.flat = flatten_force(paste0(force.dir,'/'),force.dec.files[f])
    roms.ls[[f]] = roms.flat %>% filter(var.name == 'temperature') 
  }
  decade.roms = bind_rows(roms.ls)
  rm(roms.ls)
  roms.monthly = decade.roms%>%
    group_by(month,box,level) %>% 
    summarize(roms.temp = mean(value,na.rm=T))
  roms.monthly$decade = clim.df$decade[d]
  roms.monthly.ls[[d]] = roms.monthly
  
  #Aggregate month,box,level 
  ncei.dec.files = list.files(ncei.dir,paste0('nwa_',clim.df$decade[d]),full.names = F)
  ncei.ls = list()
  f = 1
  for(f in 1:length(ncei.dec.files)){
    #pull each ncei file and flaten it
    ncei.flat = nc_to_array(nc.dir = ncei.dir, nc.file =ncei.dec.files[f],var.name = 't_an',lon.range = c(-90,-0),lat.range = c(0,90))
    
    # depth.match  = data.frame(level = ncei.flat$neus_depths)
    # depth.match$depth = 1:nrow(depth.match)
    # 
    #reform into dataframe
    ncei.data = reshape2::melt(ncei.flat$data,varnames = c('lon','lat','depth'),value.name = 't_an')
    ncei.data$lat = ncei.flat$lat_bnds[3,ncei.data$lat]
    ncei.data$lon = ncei.flat$lon_bnds[3,ncei.data$lon]
    
    pnts_sf = st_as_sf(ncei.data, coords = c('lon','lat'),crs = st_crs(box.shp))
    
    box.match = pnts_sf %>% 
      mutate(intersection = as.integer(st_intersects(geometry,box.shp)),
             box = if_else(is.na(intersection),'',box.shp$BOX_ID[intersection])) %>%
      filter(!is.na(intersection)) %>%
      # left_join(depth.match) %>%
      # filter(!is.na(level)) %>%
      group_by(box,depth) %>%
      summarize(ncei.temp = mean(t_an,na.rm=T)) %>%
      dplyr::rename(level = depth) %>% 
      filter(!is.na(ncei.temp))
    box.match$month = sprintf('%02d',which(month.name == ncei.flat$month))
    box.match$decade = clim.df$decade[d]
    box.match = as.data.frame(box.match) %>% dplyr::select(-geometry)
    
    ncei.ls[[f]] = box.match
    print(f)
  }
  ncei.monthly.ls[[d]] = dplyr::bind_rows(ncei.ls)
  
}

roms.monthly = dplyr::bind_rows(roms.monthly.ls)
roms.monthly  = roms.monthly %>% filter(level != 0) %>% ungroup()
ncei.monthly = dplyr::bind_rows(ncei.monthly.ls) %>% ungroup()
ncei.monthly$box = as.numeric(ncei.monthly$box)

save(roms.monthly,ncei.monthly, file = paste0(ncei.dir, 'ROMS and NCEI Monthly Decadal Averages.'))
load( paste0(ncei.dir, 'ROMS and NCEI Monthly Decadal Averages.'))

monthly.temp.all = roms.monthly %>% left_join(ncei.monthly) %>% filter(!is.na(roms.temp) & !is.na(ncei.temp))
monthly.temp.all$bias = monthly.temp.all$roms.temp- monthly.temp.all$ncei.temp


##Apply bias correction to ROMS data and rewrite temperature values

for(i in 1:length(force.files)){
  
  which.decade = findInterval(years[i],clim.df$start)
  force.nc = nc_open(paste0(force.dir,'/',force.files[i]))
  force.temp = ncvar_get(force.nc,'temperature')
  ndays = dim(force.temp)[3]
  dates = as.POSIXct(force.nc$dim$t$vals,origin = '1964-01-01 00:00:00',tz = 'UTC')
  month.date = format(dates,format = '%m')
  for(d in 1:ndays){
      
    day.bias = monthly.temp.all %>% filter(month == month.date[d] & decade == clim.df$decade[which.decade] )
    # ungroup(day.bias) %>% dplyr::select(level,box,bias) %>% tidyr::spread(level*box)
    day.bias2 = reshape2::dcast(dplyr::select(ungroup(day.bias),level,box,bias),level~box,value.var = 'bias')
    day.bias2 = as.matrix(day.bias2[,2:ncol(day.bias2)])
    
    temp.dat = force.temp[1:4,c(1:23,26:30),d]
    temp.new = temp.dat - day.bias2
    force.temp[1:4,c(1:23,26:30),d] = temp.new
    force.temp[5,c(1:23,26:30),d] = temp.new[1,]
  }
  
  nc_close(force.nc)
  base.name = strsplit(force.files[i],'\\.nc')
  file.copy(from = paste0(force.dir,'/',force.files[i]),
            to = paste0(out.dir,base.name,'_debias.nc'))
  new.force.nc = nc_open(paste0(out.dir,base.name,'_debias.nc'),write = T)
  ncvar_put(new.force.nc,'temperature',force.temp)  
  nc_close(new.force.nc)
  print(i)
}

#Do the same for spinup years. take into account leap year (repeat 12/31)

years.spinup = 1964:1980
for(i in 1:length(years.spinup)){
  
  duplicate_force_year(
    force.dir = out.dir,
    reference.file = 'roms_tempsalt_force_1981_debias.nc',
    start.year = 1964,
    new.year = years.spinup[i],
    is.boxvar = T,
    t.dim.name = 't'
  )
  print(i)
}
