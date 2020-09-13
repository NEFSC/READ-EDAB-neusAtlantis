# Script to transform export flux data into annual DL forcing files

library(ncdf4)
library(RNetCDF)
library(dplyr)

#set satellite phytoplankton directory
satphyto.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/'

#load in export flux data
data.pe = read.csv(paste0(satphyto.dir,'Data/export_flux_allyears.csv'))

#Read in dz.csv file
dz = read.csv(here::here('Geometry','dz.csv'))
box.lev = apply(dz[,2:5],1,function(x) return(sum(!is.na(x))))

years = sort(as.numeric(unique(data.pe$ref.year)))

out.dir = paste0(satphyto.dir,'Forcing_DL/')

boxes = 0:29
#Loop over all years, structure array, save as netCDF
y=1
for(y in 1:length(years)){
  
  #Create year-day index
  year.date.index = data.frame(date = seq.Date(as.Date(paste0(years[y],'-01-01')),as.Date(paste0(years[y],'-12-31')),by = 'days'))
  year.date.index$doy = 1:nrow(year.date.index)
  
  #subset to year and add doy index
  dat.year = data.pe %>% 
    filter(ref.year == years[y])
  
  #create data array
  dat.array = array(NA, dim = c(5,30,nrow(year.date.index)))
  
  #Loop through boxes and assign to array
  b=1
  for(b in 1:length(boxes)){
    dat.box = dat.year %>%
      filter(box == boxes[b]) %>%
      arrange(doy)
    
    dat.array[box.lev[b],b,dat.box$doy] = dat.box$values
  }
  
  t_tot = as.numeric(difftime(as.POSIXct(year.date.index$date,tz='UTC'),as.POSIXct('1964-01-01 00:00:00',tz='UTC'),units = 'secs'))
  
  #Test t_tot dates
  # as.POSIXct(t_tot,origin = '1964-01-01 00:00:00',tz = 'UTC')
  
  # call new netCDF file
  filename = paste0(out.dir,'Satphyto_Forcing_DL_',years[y],'.nc')
  
  nc.file = RNetCDF::create.nc(filename)
  
  RNetCDF::dim.def.nc(nc.file, "t", unlim=TRUE)
  RNetCDF::dim.def.nc(nc.file, "b", 30)
  RNetCDF::dim.def.nc(nc.file, "z", 5)
  
  RNetCDF::var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
  #Define Variables
  RNetCDF::var.def.nc(nc.file, 'Lab_Det_N', 'NC_DOUBLE', c('z','b','t'))
  #Assign Fill Value
  RNetCDF::att.put.nc(nc.file, 'Lab_Det_N', '_FillValue', "NC_DOUBLE", -999)
  #Assign 
  RNetCDF::att.put.nc(nc.file, 'Lab_Det_N', 'missing_value', 'NC_DOUBLE',-999)
  #Assign valid_min
  RNetCDF::att.put.nc(nc.file, 'Lab_Det_N', 'valid_min', 'NC_DOUBLE', -999)
  #Assing valid_max
  RNetCDF::att.put.nc(nc.file, 'Lab_Det_N', 'valid_max', 'NC_DOUBLE', 99999)
  #Assign units
  RNetCDF::att.put.nc(nc.file, 'Lab_Det_N', 'units','NC_CHAR', 'mg N m-3')  
  #Assign long_name
  RNetCDF::att.put.nc(nc.file,'Lab_Det_N','long_name','NC_CHAR','Labile Detrital Nitrogen')
  
  #Put variable values
  RNetCDF::var.put.nc(nc.file,'Lab_Det_N',dat.array)
  
  
  RNetCDF::att.put.nc(nc.file, "t", "units", "NC_CHAR", 'seconds since 1964-01-01 00:00:00 UTC')
  RNetCDF::att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'NEUS_Atlantis_Obs_Hindcast')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  RNetCDF::var.put.nc(nc.file, "t", t_tot)
  
  
  RNetCDF::close.nc(nc.file)
}


#Call spinup function

#copy to obs hindcast directory
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_DL/'
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/labile_detritus_DOY_spinup/'
from.files = paste0(force.dir,'Satphyto_Forcing_DL_',1998:2017,'.nc')
file.copy(from.files,obs.dir,overwrite = T)


#Make spinup files
years = 1964:1997
source(here::here('R','make_force_spinup.R'))
for(i in 1:length(years)){
  # out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/'
  
  make_force_spinup(
    do.hydroconstruct = F,
    out.dir = obs.dir,
    trans.prefix = NA,
    statevar.prefix = NA,
    anyvar.prefix = 'Satphyto_Forcing_DL_',
    transport.file = NA,
    statevar.file = NA,
    # anyvar.file = paste0(obs.dir,'Satphyto_Forcing_DL_1998.nc'),
    anyvar.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/Det_DOY_Climatology.nc',
    anyvar.out = obs.dir,
    force.dir = obs.dir,
    start.year = 1964,
    new.year = years[i],
    mid.layer = 'dynamic',
    bot.layer = 'dynamic',
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
  )
}

from.files = paste0(obs.dir,'Satphyto_Forcing_DL_',1964:2017,'.nc')
git.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'
file.copy(from.files,git.dir,overwrite = T)
