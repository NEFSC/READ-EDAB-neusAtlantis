#Script to copy last year of vflux for N years

library(ncdf4)
library(dplyr)

 years = 2018:2021
 
 ref.year = 2017
 
 ecco.dir = 'D:/ECCO/' 
 
 i=3
 for(i in 1:length(years)){
   
   dir.create(paste0(ecco.dir,'/vflux_daily/',years[i]))
   
   ref.nc.file = paste0(ecco.dir,'vflux_daily/',ref.year,'/ECCO_vflux_Atlantis_',ref.year,'.nc')
   new.nc.file = paste0(ecco.dir,'vflux_daily/',years[i],'/ECCO_vflux_Atlantis_',years[i],'.nc')
   
   ref.r.file = paste0(ecco.dir,'vflux_daily/',ref.year,'/ECCO_vflux_Atlantis_statevars_',ref.year,'.R')
   new.r.file = paste0(ecco.dir,'vflux_daily/',years[i],'/ECCO_vflux_Atlantis_statevars_',years[i],'.R')
   
   file.copy(ref.nc.file,new.nc.file,overwrite = T)
   file.copy(ref.r.file,new.r.file,overwrite = T)
   
   nc = nc_open(new.nc.file,write = T)
   load(new.r.file)
  
   t1 = seq.Date(as.Date(paste0(years[i],'-01-01 00:00:00')),as.Date(paste0(years[i],'-12-31 00:00:00')),'days')
   time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0('1964-01-01 00:00:00'),tz = 'UTC'),units = 'secs')
   
   
   if(years[i] %% 4 == 0){
    dat = ncvar_get(nc,'verticalflux')  
    
    new.dat = array(NA,dim = c(4,30,366))
    new.dat[,,1:365] = dat
    new.dat[,,366] = dat[,,365]
    
    timedim=ncdim_def("time", "", 1:length(time.vals), unlim=T, create_dimvar = F) #as.double(t_tot)
    leveldim=ncdim_def("level", "", 1:4, create_dimvar = F)
    boxesdim=ncdim_def("boxes", "", 1:30, create_dimvar = F)
    var.vertflux=ncvar_def("verticalflux","m3/s",list(leveldim, boxesdim, timedim),-999,longname="vertical flux averaged over floor of box",prec="float")
    var.time=ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
    
    ncvar_put(nc,var.vertflux,new.dat, count=c(4,30, 366))
    
    vertical_flux = new.dat
    save(vertical_flux,file = new.r.file)
   }
   
   ncvar_put(nc,'time',time.vals)
   
   nc_close(nc)
 }
 

