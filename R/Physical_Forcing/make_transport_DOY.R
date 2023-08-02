#' Script to create a DOY average of transport 
#' Differs from boxvariables, on box-face-time scale
#' 

library(ncdf4)
library(dplyr)

#Build transport file names
trans.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/transport/'
trans.prefix = 'GLORYS_Atlantis_Transport_'
years = 1993:2017
file.names = paste0(trans.dir,trans.prefix,years,'.nc')

trans.year.ls = list()
#loop through transport files, format to long-format data.frame, add DOY
for(f in 1:length(file.names)){
  
  nc.file = nc_open(file.names[f])
  
  #read unchanging variables
  if(f == 1){
    dest_boxid = ncvar_get(nc.file,'dest_boxid')  
    source_boxid = ncvar_get(nc.file,'source_boxid')
    pt1_x = ncvar_get(nc.file,'pt1_x')
    pt2_x = ncvar_get(nc.file,'pt2_x')
    pt1_y = ncvar_get(nc.file,'pt1_y')
    pt2_y = ncvar_get(nc.file,'pt2_y')
  }
  
  trans.dat = ncvar_get(nc.file,'transport')
  
  #Loop through levels, reshape to long format
  trans.lev.ls = list()
  for(l in 1:4){
    lev.dat = as.data.frame(t(trans.dat[l,,]))
    colnames(lev.dat) = 1:151
    lev.dat$DOY = 1:dim(lev.dat)[1]
    lev.dat = reshape2::melt(lev.dat,id.vars = 'DOY')
    lev.dat$level = l
    trans.lev.ls[[l]] = lev.dat
  }
  trans.dat.df = bind_rows(trans.lev.ls)
  
  trans.year.ls[[f]]= trans.dat.df
  print(f)
}

#combine annual transport ans save
trans.year.df = bind_rows(trans.year.ls)
colnames(trans.year.df) = c('DOY','face','transport','level')
save(list = c('dest_boxid','source_boxid','pt1_x','pt2_x','pt1_y','pt2_y','trans.year.df'),
     file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/transport_longform_allyears.R')

#clear some memory
rm(trans.year.ls,trans.lev.ls,lev.dat,trans.dat.df)

#Create DOY mean
trans.doy.df = trans.year.df %>%
  group_by(face,level,DOY) %>% 
  summarize(transport = mean(transport,na.rm=T))

#Format DOY mean back into array (4x151x366)

trans.dat.array = array(NA,dim = c(4,151,366))

#loop through boxes then levels (different vars have different number of levels)
faces = 1:151

for(f in 1:length(faces)){
  dat.var.face = filter(trans.doy.df,face == faces[f])
  test = reshape2::dcast(dat.var.face,level~DOY,value.var = 'transport')[,-1]
  nlev = length(unique(dat.var.face$level))
  for(l in 1:nlev){
    trans.dat.array[l,f,] = as.numeric(test[l,])
  }
  print(f)
}

#Define "dummy year" so that there are dates assigned (must be leap year due to 366 length DOY)
ref.year = 1964
date.seq = seq.Date(as.Date(paste0(ref.year,'-01-01')),as.Date(paste0(ref.year,'-12-31')),by = 'day')
time.vals = as.numeric(difftime(date.seq,as.POSIXct('1964-01-01 00:00:00',tz = 'UTC'),tz = 'UTC',units = 'secs'))

#Make netCDF for transport

filename='C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/transport_DOY_climatology.nc'

#define dimensions
timedim=ncdim_def("time", "", 1:length(time.vals), unlim=T, create_dimvar = F) #as.double(t_tot)
leveldim=ncdim_def("level", "", 1:4, create_dimvar = F)
facesdim=ncdim_def("faces", "", 1:151, create_dimvar = F)

#create variables
#NB!!!!!! Unlimited rec needs to be on the right - otherwise R complains!
#origMissVal_ex=0.0
var.time=ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
var.face=ncvar_def("faces", "", facesdim, longname="Face IDs", prec='integer')
var.lev=ncvar_def("level","",leveldim,longname="layer index; 1=near surface",prec="integer")
var.trans=ncvar_def("transport","m3/s",list(leveldim,facesdim,timedim),0,prec="float")
var.destb=ncvar_def("dest_boxid","id", facesdim,longname="ID of destination box", prec="integer")
var.sourceb=ncvar_def("source_boxid","id", facesdim,longname="ID of source box",prec="integer")
var.pt1x=ncvar_def("pt1_x", "degree_east", facesdim, longname = "x-coord of pt 1 of face", prec='float')
var.pt2x=ncvar_def("pt2_x", "degree_east", facesdim, longname = "x-coord of pt 2 of face", prec='float')
var.pt1y=ncvar_def("pt1_y", "degree_north", facesdim, longname = "y-coord of pt 1 of face", prec='float')
var.pt2y=ncvar_def("pt2_y", "degree_north", facesdim, longname = "y-coord of pt 1 of face", prec='float')

nc_transp=nc_create(filename,list(var.time,var.face, var.lev, var.destb,var.sourceb, var.pt1x, var.pt2x, var.pt1y, var.pt2y,var.trans))

#assign global attributes to file
ncatt_put(nc_transp,0,"title","Transport file, NEUS")
ncatt_put(nc_transp,0,"geometry","neus_tmerc_RM.bgm")
ncatt_put(nc_transp,0,"parameters","")

#assign attributes to variables
ncatt_put(nc_transp,var.time,"dt",86400,prec="double")

#assign variables to file
ncvar_put(nc_transp,var.trans,trans.dat.array, count=c(4,151,366))
ncvar_put(nc_transp,var.time,time.vals)
ncvar_put(nc_transp,var.lev,1:4)
ncvar_put(nc_transp,var.face,0:150)
ncvar_put(nc_transp,var.destb,dest_boxid)
ncvar_put(nc_transp,var.pt1x,pt1_x)
ncvar_put(nc_transp,var.pt1y,pt1_y)
ncvar_put(nc_transp,var.pt2x,pt2_x)
ncvar_put(nc_transp,var.pt2y,pt2_y)
ncvar_put(nc_transp,var.sourceb,source_boxid)

nc_close(nc_transp)
