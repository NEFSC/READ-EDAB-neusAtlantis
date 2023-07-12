#Read in climatology, apply delta, format time
library(dplyr)
library(ncdf4)

data.dir = 'C:/Users/Joseph.Caracappa/Documents/Cobia/'
out.dir =  'C:/Users/Joseph.Caracappa/Documents/Cobia/Atlantis_Format/'

layer.index.mat = read.csv(here::here('Geometry','box_layer_index.csv'),header = F, as.is = T) %>% as.matrix
layer.index.vec = as.vector(layer.index.mat)

delta.t = readRDS(file = paste0(data.dir,'Atlantis_Format/CM2_6_Atlantis_temperature_delta.rds'))
file_db = readRDS(file = paste0(data.dir,'Atlantis_Format/CM2_6_Atlantis_temperature_delta_db.rds'))%>%
  mutate(date= as.Date(date),
         month = as.numeric(format(date,format = '%m')),
         year = as.numeric(format(date, format = '%Y')),
         ID = 1:n())

years = 2021:2100

start.year = 1964

statevar.base.file = here::here('currentVersion','tsfiles','Annual_Files','GLORYS_tempsalt_force_2020.nc')
statevar.base = nc_open(statevar.base.file,write = F)

temp.base = ncvar_get(statevar.base,'temperature')

ntime = dim(delta.t)[3]

delta.t.reorder = array(NA, dim = c(5,30,ntime))
blank.mat = matrix(NA,nrow = 5, ncol =30)

for( i in 1:ntime){
  
  dat.time = delta.t[,,i]
  dat.time.vec = as.vector(dat.time)
  dat.time.new = dat.time.vec[layer.index.vec]
  dat.time.mat = blank.mat
  dat.time.mat[1:4,]= matrix(dat.time.new,nrow = 4)
  dat.time.mat[5,] = dat.time.mat[1,]
  delta.t.reorder[,,i] = dat.time.mat
}

date2month = format(seq.Date(as.Date('2021-01-01'),as.Date('2021-12-31'), by = 1),format = '%m')

for(y in 1:length(years)){
  
  #make new copy of file
  new.statevar.file = paste0(out.dir,'CM2_6_tempsalt_force_',years[y],'.nc')
  
  file_db.yr = filter(file_db,year == years[y])
  
  #Adjust time marker
  t1 = seq.Date(as.Date(paste0(years[y],'-01-01 00:00:00')),as.Date(paste0(years[y],'-12-31 00:00:00')),'days')
  time.vals =as.numeric(difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0('1964-01-01 00:00:00'),tz = 'UTC'),units = 'secs'))
  
  temp.year = temp.base
  
  #add deltas to temperature
  for(j in 1:length(time.vals)){
    
    month.match = as.numeric(date2month[j])
    which.delta = file_db.yr$ID[which(file_db.yr$month == month.match)]
    
    temp.year[,,j] = temp.base[,,j]+delta.t.reorder[,,13]
    
    temp.year = temp.year[,,1:length(time.vals)]
  }
  
  #define netCDF variables
  timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
  var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  
  leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)
  boxesdim = ncdf4::ncdim_def('boxes','',1:30,create_dimvar = F)
  
  var.time=ncdf4::ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
  var.box=ncdf4::ncvar_def("boxes", "", boxesdim, longname="Box IDs", prec='integer')
  var.lev=ncdf4::ncvar_def("level","",leveldim,longname="layer index; 1=near surface; positice=down" ,prec="integer")
  
  var.def.ls = list(var.time,var.box,var.lev)
  
  #Modify and append statevar file
  var.names = names(statevar.base$var)
  var.units = c('psu','deg C','m3/s')
  
  #If leap year, append values to add extra day
  new.var.dat.ls = list()
  for(v in 1:length(var.names)){
    var.dat = ncdf4::ncvar_get(statevar.base,var.names[v])
    var.def.ls[[v+3]] = var.vertflux=ncdf4::ncvar_def(var.names[v],var.units[v],list(leveldim, boxesdim, timedim),-999,prec="float")
    dims = dim(var.dat)
    if(years[y] %% 4 == 0){
      
      dims[3] = 366
      new.var.dat = array(NA,dim = dims)
      last.dat = var.dat[,,365]
      new.var.dat[,,1:365] = var.dat[,,1:365]
      new.var.dat[,,366] = last.dat       
      
    } else {
      new.var.dat = var.dat[,,1:365]
    }
    new.var.dat.ls[[v]] = new.var.dat[1:4,,]
  }
  
  #Build new NC File
  nc_varfile = ncdf4::nc_create(new.statevar.file,var.def.ls)
  
  #assign global attributes to file
  ncdf4::ncatt_put(nc_varfile,0,"title","Box averaged properties file, NEUS")
  ncdf4::ncatt_put(nc_varfile,0,"geometry","neus_tmerc_RM.bgm")
  ncdf4::ncatt_put(nc_varfile,0,"parameters","")
  
  #assign attributes to variables
  ncdf4::ncatt_put(nc_varfile,var.time,"dt",86400,prec="double")
  
  for(v in 1:length(var.names)){
    ncdf4::ncvar_put(nc_varfile,var.def.ls[[v+3]],new.var.dat.ls[[v]], count=c(4,30,length(time.vals)),verbose = F)  
  }
  ncdf4::ncvar_put(nc_varfile,var.time,time.vals,verbose = F)
  ncdf4::ncvar_put(nc_varfile,var.lev,1:4)
  ncdf4::ncvar_put(nc_varfile,var.box,0:29)
  
  ncdf4::ncatt_put(nc_varfile,'time','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
  ncdf4::nc_close(nc_varfile)
}

nc_close(statevar.base.file)
