# This script fills in the missing data betwen 1/1/1980 and 1/31/1980 in ROMS_COBALT data
# Uses data from same dates in 1981

library(ncdf4)

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/'

#transport

#pull from 1981
transport.81.nc = nc_open(paste0(roms.dir,'/transport/roms_output_transport_tohydro_1981.nc'))
trans.81 = ncvar_get(transport.81.nc,'transport')[,,1:31]
nc_close(transport.81.nc)

t1 = seq.Date(as.Date('1980-01-01 00:00:00'),as.Date('1980-12-31 00:00:00'),'days')
time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct('1964-01-01 00:00:00',tz = 'UTC'),units = 'secs')

timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)

#put into 1980
transport.80.nc = nc_open(paste0(roms.dir,'/transport/roms_output_transport_tohydro_1980.nc'),write=T)
facesdim = ncdf4::ncdim_def('faces','',1:151,create_dimvar = F)
trans.80 = ncvar_get(transport.80.nc,'transport')
new.trans = array(NA,dim=c(4,151,366))
new.trans[,,32:366] = trans.80[,,32:366]
new.trans[,,1:31] = trans.81
var.trans = ncdf4::ncvar_def('transport','m3/s',list(leveldim,facesdim,timedim),0,prec='float')
ncdf4::ncvar_put(transport.80.nc,var.trans,new.trans,count = c(4,151,366))

var.time = ncdf4::ncvar_def('time',paste0('seconds since 1964-01-01 00:00:00 +10'),timedim,prec='double')
ncdf4::ncatt_put(transport.80.nc,'time','units',paste0('seconds since 1964-01-01 00:00:00 +10'))

ncdf4::ncvar_put(transport.80.nc,var.time,time.vals)
ncdf4::nc_close(transport.80.nc)

#statevars

#pull from 1981
statevars.81.nc = nc_open(paste0(roms.dir,'/statevars/roms_output_statevars_tohydro_1981.nc'))
temp.81 = ncvar_get(statevars.81.nc,'temperature')[,,1:31]
salt.81 = ncvar_get(statevars.81.nc,'salinity')[,,1:31]
vflux.81 = ncvar_get(statevars.81.nc,'verticalflux')[,,1:31]

nc_close(statevars.81.nc)

#put into 1980
statevars.80.nc = nc_open(paste0(roms.dir,'/statevars/roms_output_statevars_tohydro_1980.nc'),write=T)
boxesdim = ncdf4::ncdim_def('boxes','',1:30,create_dimvar = F)

temp.80 = ncvar_get(statevars.80.nc,'temperature')
salt.80 = ncvar_get(statevars.80.nc,'salinity')
vflux.80 = ncvar_get(statevars.80.nc,'verticalflux')

new.temp = new.salt = new.vflux = array(NA,dim=c(4,30,366))

new.temp[,,32:366] = temp.80[,,32:366]
new.salt[,,32:366] = salt.80[,,32:366]
new.vflux[,,32:366] = vflux.80[,,32:366]

new.temp[,,1:31] = temp.81
new.salt[,,1:31] = salt.81
new.vflux[,,1:31] = vflux.81

var.vertflux=ncdf4::ncvar_def("verticalflux","m3/s",list(leveldim, boxesdim, timedim),-999,longname="vertical flux averaged over floor of box",prec="float")
var.temp=ncdf4::ncvar_def("temperature","degree_C",list(leveldim, boxesdim, timedim),-999,longname="temperature volume averaged",prec="float")
var.salt=ncdf4::ncvar_def("salinity","psu",list(leveldim,boxesdim,timedim),-999,longname="salinity volume averaged",prec="float")

ncdf4::ncvar_put(statevars.80.nc,var.vertflux,new.vflux, count=c(4,30,366))
ncdf4::ncvar_put(statevars.80.nc,var.salt,new.salt, count=c(4,30,366))
ncdf4::ncvar_put(statevars.80.nc,var.temp,new.temp, count=c(4,30,366))

ncdf4::ncatt_put(statevars.80.nc,'time','units',paste0('seconds since 1964-01-01 00:00:00 +10'))

ncdf4::ncvar_put(statevars.80.nc,var.time,time.vals)
ncdf4::nc_close(statevars.80.nc)

#ltl statevars

#pull from 1981
ltlvars.81.nc = nc_open(paste0(roms.dir,'/ltl_statevars/roms_output_ltl_statevars_tohydro_1981.nc'))

ndi.81 = ncvar_get(ltlvars.81.nc,'ndi')[,,1:31]
nlg.81 = ncvar_get(ltlvars.81.nc,'nlg')[,,1:31]
nlgz.81 = ncvar_get(ltlvars.81.nc,'nlgz')[,,1:31]
nmdz.81 = ncvar_get(ltlvars.81.nc,'nmdz')[,,1:31]
nsm.81 = ncvar_get(ltlvars.81.nc,'nsm')[,,1:31]
nsmz.81 = ncvar_get(ltlvars.81.nc,'nsmz')[,,1:31]
silg.81 = ncvar_get(ltlvars.81.nc,'silg')[,,1:31]
nbact.81 = ncvar_get(ltlvars.81.nc,'nbact')[,,1:31]

nc_close(ltlvars.81.nc)

#put into 1980
ltlvars.80.nc = nc_open(paste0(roms.dir,'/ltl_statevars/roms_output_ltl_statevars_tohydro_1980.nc'),write=T)

ndi.80 = ncvar_get(ltlvars.80.nc,'ndi')
nlg.80 = ncvar_get(ltlvars.80.nc,'nlg')
nlgz.80 = ncvar_get(ltlvars.80.nc,'nlgz')
nmdz.80 = ncvar_get(ltlvars.80.nc,'nmdz')
nsm.80 = ncvar_get(ltlvars.80.nc,'nsm')
nsmz.80 = ncvar_get(ltlvars.80.nc,'nsmz')
silg.80 = ncvar_get(ltlvars.80.nc,'silg')
nbact.80 = ncvar_get(ltlvars.80.nc,'nbact')

new.ndi = new.nlg = new.nlgz = new.nmdz = new.nsm = new.nsmz = new.silg = new.nbact = array(NA,dim=c(4,30,366))

new.ndi[,,32:366] = ndi.80[,,32:366]
new.nlg[,,32:366] = nlg.80[,,32:366]
new.nlgz[,,32:366] = nlgz.80[,,32:366]
new.nmdz[,,32:366] = nmdz.80[,,32:366]
new.nsm[,,32:366] = nsm.80[,,32:366]
new.nsmz[,,32:366] = nsmz.80[,,32:366]
new.silg[,,32:366] = silg.80[,,32:366]
new.nbact[,,32:366] = nbact.80[,,32:366]

new.ndi[,,1:31] = ndi.81
new.nlg[,,1:31] = nlg.81
new.nlgz[,,1:31] = nlgz.81
new.nmdz[,,1:31] = nmdz.81
new.nsm[,,1:31] = nsm.81
new.nsmz[,,1:31] = nsmz.81
new.silg[,,1:31] = silg.81
new.nbact[,,1:31] = nbact.81


var.ndi=ncdf4::ncvar_def('ndi','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Diazotroph Nitrogen',prec='float')
var.nlg=ncdf4::ncvar_def('nlg','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Phyotplankton Nitrogen',prec='float')
var.nlgz=ncdf4::ncvar_def('nlgz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Zooplankton Nitrogen',prec='float')
var.nmdz=ncdf4::ncvar_def('nmdz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Medium Zooplankton Nitrogen',prec='float')
var.nsm=ncdf4::ncvar_def('nsm','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Small Phytoplankton Nitrogen',prec='float')
var.nsmz=ncdf4::ncvar_def('nsmz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Small Zooplankton Nitrogen',prec='float')
var.silg=ncdf4::ncvar_def('silg','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Phytoplankton Silicon',prec='float')
var.nbact=ncdf4::ncvar_def('nbact','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Bacterial Nitrogen',prec='float')

ncdf4::ncvar_put(ltlvars.80.nc,var.ndi,new.ndi, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.nlg,new.nlg, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.nlgz,new.nlgz, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.nmdz,new.nmdz, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.nsm,new.nsm, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.nsmz,new.nsmz, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.silg,new.silg, count=c(4,30,366))
ncdf4::ncvar_put(ltlvars.80.nc,var.nbact,new.nbact, count=c(4,30,366))

ncdf4::ncatt_put(ltlvars.80.nc,'time','units',paste0('seconds since 1964-01-01 00:00:00 +10'))

ncdf4::ncvar_put(ltlvars.80.nc,var.time,time.vals)
ncdf4::nc_close(ltlvars.80.nc)

#run hydroconstruct

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/'
# setwd(paste0(roms.dir,'Forcing_Files/'))
# orig.prm = readLines(paste0(roms.dir,'Forcing_Files/roms_cobalt_hydroconstruct_v2.prm'))
# orig.bat = readLines(paste0(roms.dir,'Forcing_Files/hydroconstruct_run_template.bat'))
# 
# transport.file = paste0(roms.dir,'ROMS_COBALT output/transport/roms_output_transport_tohydro_1980.nc')
# statevar.file = paste0(roms.dir,'ROMS_COBALT output/statevars/roms_output_statevars_tohydro_1980.nc')
# 
# orig.sub = gsub(pattern = 'transport.nc',replacement = transport.file,x = orig.prm)
# orig.sub = gsub(pattern = 'vtrans.nc',replacement = statevar.file,x=orig.sub)
# orig.sub = gsub(pattern = 'tempsalt.nc',replacement = statevar.file,x = orig.sub)
# orig.sub = gsub(pattern = 'tstop 1',replacement = paste0('tstop ',length(time.vals)),x =orig.sub)
# 
# #save as yearly temp param file
# writeLines(orig.sub,con = paste0(roms.dir,'Forcing_Files/roms_cobalt_hydroconstruct_temp.prm'))
# 
# #sub batch file values
# bat.sub = gsub(pattern = 'flow_year',replacement = paste0('flow_',years[yr]),x = orig.bat)
# bat.sub = gsub(pattern = 'salt_year',replacement = paste0('salt_',years[yr]),x = bat.sub)
# bat.sub = gsub(pattern = 'temp_year',replacement = paste0('temp',years[yr]),x = bat.sub)
# bat.sub = gsub(pattern = 'volume_year',replacement = paste0('volume',years[yr]),x = bat.sub)
# bat.sub = gsub(pattern = 'roms_cobalt_hydroconstruct_v2.prm','roms_cobalt_hydroconstruct_temp.prm', x= bat.sub)
# 
# #save batch as temp file
# writeLines(bat.sub, con = paste0(roms.dir,'Forcing_Files/hydroconstruct_run_temp.bat'))
# 
# #Run hydroconstruct with system()
# 
# shell(paste0(roms.dir,'Forcing_Files/hydroconstruct_run_temp.bat'))
