#' Creates forcing files based on given model year
#' 
#' In order to spinup the ROMS_COBALT forced model, 
#' additional forcing files need to be generated 
#' based on looping a year of ROMS_COBALT output.
#' This function generates the aggregated forcing
#' files, formats the time dimension based on a 
#' specified start time, and calls on hydroconstruct
#' to generate the atlantis forcing files. This assumes 
#' a yearly ROMS output.
#' 
#' @transport.file string. path to roms transport file to copy
#' @statevar.file string. path to state variables (temp and salt) file to copy
#' @ltlvar.file string. path to the LTL variable (phyto and zoo) file to copy
#' @roms.out.dir string. Path to location of ROMS output files
#' @hydro.command string. Command to be run by hydroconstruct
#' @force.dir string. Path to location of forcing files
#' @start.year string. Year model starts (assumes 01-01-'start.year')
#' @new.year string.New year for forcing file
#' @param.temp string. Name of template hydroconstruct parameter file to modify
#' @bat.temp string. Name of template batch file (that runs hydroconstruct) to modify
#' 
#' @return Yearly ROMS and Atlantis formatted forcing files. 
#' 
#' Author: J. Caracappa

# roms.out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/'
# # file2copy = 'roms_output_transport_tohydro_1981.nc'
# transport.file = paste0(roms.out.dir,'transport/roms_output_transport_tohydro_1981.nc')
# statevar.file = paste0(roms.out.dir,'statevars/roms_output_statevars_tohydro_1981.nc')
# ltlvar.file = paste0(roms.out.dir,'ltl_statevars/roms_output_ltl_statevars_tohydro_1981.nc')
# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/'
# start.year = 1964
# new.year = 1964
# param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/roms_cobalt_hydroconstruct_v2.prm'
# bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/hydroconstruct_run_template.bat'

duplicate.forcing.year = function(transport.file,
                                  statevar.file,
                                  ltlvar.file,
                                  roms.out.dir,
                                  force.dir,
                                  start.year,
                                  new.year,
                                  param.temp,
                                  bat.temp){
  setwd(force.dir)
  #Make a copy of the replicated year
  new.trans.file = paste0(roms.out.dir,'transport/roms_output_transport_tohydro_',new.year,'.nc')
  new.statevar.file = paste0(roms.out.dir,'statevars/roms_output_statevars_tohydro_',new.year,'.nc')
  new.ltlvar.file = paste0(roms.out.dir,'ltl_statevars/roms_output_ltl_statevars_tohydro_',new.year,'.nc')
  
  file.copy(transport.file,new.trans.file,overwrite= T)
  file.copy(statevar.file,new.statevar.file,overwrite=T)
  file.copy(ltlvar.file,new.ltlvar.file,overwrite=T)
  
  #Modify and Append transport.nc
  transport.nc = ncdf4::nc_open(new.trans.file,write = T)
  
  t1 = seq.Date(as.Date(paste0(new.year,'-01-01 00:00:00')),as.Date(paste0(new.year,'-12-31 00:00:00')),'days')
  time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0(start.year,'-01-01 00:00:00'),tz = 'UTC'),units = 'secs')
  
  timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
  leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)
  
  #If leap year, append values to add extra day
  if(new.year %% 4 == 0){
    facesdim = ncdf4::ncdim_def('faces','',1:151,create_dimvar = F)
    transport = ncdf4::ncvar_get(transport.nc,'transport')
    dims = dim(transport)
    dims[3] = 366
    new.array = array(NA,dim = dims)
    last.day = transport[,,365]
    new.array[,,1:365] = transport[,,1:365]
    new.array[,,366] = last.day
    
    var.trans = ncdf4::ncvar_def('transport','m3/s',list(leveldim,facesdim,timedim),0,prec='float')
    ncdf4::ncvar_put(transport.nc,var.trans,new.array,count = c(4,151,366))
  } 
  
  var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  ncdf4::ncatt_put(transport.nc,'time','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
  
  ncdf4::ncvar_put(transport.nc,var.time,time.vals)
  ncdf4::nc_close(transport.nc)

  #Modify and append statevar file
  statevar.nc = ncdf4::nc_open(new.statevar.file,write = T)
  
  #If leap year, append values to add extra day
  if(new.year %% 4 == 0){
    boxesdim = ncdf4::ncdim_def('boxes','',1:30,create_dimvar = F)
    temp = ncdf4::ncvar_get(statevar.nc,'temperature')
    salt = ncdf4::ncvar_get(statevar.nc,'salinity')
    vflux = ncdf4::ncvar_get(statevar.nc,'verticalflux')
    
    dims = dim(temp)
    dims[3] = 366
    new.temp = new.salt = new.vflux = array(NA,dim = dims)
    last.temp = temp[,,365]
    last.salt = salt[,,365]
    last.vflux = vflux[,,365]
    
    new.temp[,,1:365] = temp[,,1:365]
    new.salt[,,1:365] = salt[,,1:365]
    new.vflux[,,1:365] = vflux[,,1:365]
    
    new.temp[,,366] = last.temp
    new.salt[,,366] = last.salt
    new.vflux[,,366] = last.vflux
        
    var.vertflux=ncdf4::ncvar_def("verticalflux","m3/s",list(leveldim, boxesdim, timedim),-999,longname="vertical flux averaged over floor of box",prec="float")
    var.temp=ncdf4::ncvar_def("temperature","degree_C",list(leveldim, boxesdim, timedim),-999,longname="temperature volume averaged",prec="float")
    var.salt=ncdf4::ncvar_def("salinity","psu",list(leveldim,boxesdim,timedim),-999,longname="salinity volume averaged",prec="float")
    
    ncdf4::ncvar_put(statevar.nc,var.vertflux,new.vflux, count=c(4,30,366))
    ncdf4::ncvar_put(statevar.nc,var.salt,new.salt, count=c(4,30,366))
    ncdf4::ncvar_put(statevar.nc,var.temp,new.temp, count=c(4,30,366))
    
    } 
  
  # var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  ncdf4::ncatt_put(statevar.nc,'time','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
  
  ncdf4::ncvar_put(statevar.nc,var.time,time.vals)
  ncdf4::nc_close(statevar.nc)
  
  #Modify and append ltl statevar file
  ltlvar.nc = ncdf4::nc_open(new.ltlvar.file,write = T)
  
  #If leap year, append values to add extra day
  if(new.year %% 4 == 0){
    
    ndi = ncdf4::ncvar_get(ltlvar.nc,'ndi')
    nlg = ncdf4::ncvar_get(ltlvar.nc,'nlg')
    nlgz= ncdf4::ncvar_get(ltlvar.nc,'nlgz')
    nmdz = ncdf4::ncvar_get(ltlvar.nc,'nmdz')
    nsm = ncdf4::ncvar_get(ltlvar.nc,'nsm')
    nsmz = ncdf4::ncvar_get(ltlvar.nc,'nsmz')
    silg = ncdf4::ncvar_get(ltlvar.nc,'silg')
    nbact = ncdf4::ncvar_get(ltlvar.nc,'nbact')
    
    dims = dim(ndi)
    dims[3] = 366
    
    new.ndi = new.nlg = new.nlgz = new.nmdz = new.nsm = new.nsmz = new.silg = new.nbact = array(NA,dim=dims)
    
    last.ndi = ndi[,,365]
    last.nlg = nlg[,,365]
    last.nlgz = nlgz[,,365]
    last.nmdz = nmdz[,,365]
    last.nsm = nsm[,,365]
    last.nsmz = nsmz[,,365]
    last.silg = silg[,,365]
    last.nbact = nbact[,,365]
    
    new.ndi[,,1:365] = ndi[,,1:365]
    new.nlg[,,1:365] = nlg[,,1:365]
    new.nlgz[,,1:365] = nlgz[,,1:365]
    new.nmdz[,,1:365] = nmdz[,,1:365]
    new.nsm[,,1:365] = nsm[,,1:365]
    new.nsmz[,,1:365] = nsmz[,,1:365]
    new.silg[,,1:365] = silg[,,1:365]
    new.nbact[,,1:365] = nbact[,,1:365]

    new.ndi[,,366] = last.ndi
    new.nlg[,,366] = last.nlg
    new.nlgz[,,366] = last.nlgz
    new.nmdz[,,366] = last.nmdz
    new.nsm[,,366] = last.nsm
    new.nsmz[,,366] = last.nsmz
    new.silg[,,366] = last.silg
    new.nbact[,,366] = last.nbact
    
    var.ndi=ncdf4::ncvar_def('ndi','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Diazotroph Nitrogen',prec='float')
    var.nlg=ncdf4::ncvar_def('nlg','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Phyotplankton Nitrogen',prec='float')
    var.nlgz=ncdf4::ncvar_def('nlgz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Zooplankton Nitrogen',prec='float')
    var.nmdz=ncdf4::ncvar_def('nmdz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Medium Zooplankton Nitrogen',prec='float')
    var.nsm=ncdf4::ncvar_def('nsm','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Small Phytoplankton Nitrogen',prec='float')
    var.nsmz=ncdf4::ncvar_def('nsmz','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Small Zooplankton Nitrogen',prec='float')
    var.silg=ncdf4::ncvar_def('silg','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Large Phytoplankton Silicon',prec='float')
    var.nbact=ncdf4::ncvar_def('nbact','mg N / m^3',list(leveldim,boxesdim,timedim),-999,longname = 'Bacterial Nitrogen',prec='float')
    
    ncdf4::ncvar_put(ltlvar.nc,var.ndi,new.ndi, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.nlg,new.nlg, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.nlgz,new.nlgz, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.nmdz,new.nmdz, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.nsm,new.nsm, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.nsmz,new.nsmz, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.silg,new.silg, count=c(4,30,366))
    ncdf4::ncvar_put(ltlvar.nc,var.nbact,new.nbact, count=c(4,30,366))
  
  } 
  
  # var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  ncdf4::ncatt_put(ltlvar.nc,'time','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
  
  ncdf4::ncvar_put(ltlvar.nc,var.time,time.vals)
  ncdf4::nc_close(ltlvar.nc)
  
  #Run Hydroconstruct with proper start year
  
  param.lines = readLines(param.temp)
  bat.lines = readLines(bat.temp)
  
  #sub parameter values
  param.sub = gsub(pattern = 'transport.nc',replacement = new.trans.file,x = param.lines)
  param.sub = gsub(pattern = 'vtrans.nc',replacement = new.statevar.file,x=param.sub)
  param.sub = gsub(pattern = 'tempsalt.nc',replacement = new.statevar.file,x = param.sub)
  param.sub = gsub(pattern = 'reference_year 1964',replacement = paste0('reference_year ',start.year),x=param.sub)
  param.sub = gsub(pattern = 'tstop 1',replacement = paste0('tstop ',length(time.vals)),x =param.sub)
  
  #save as yearly temp param file
  writeLines(param.sub,con = paste0(force.dir,'roms_cobalt_hydroconstruct_temp.prm'))
  
  #sub batch file values
  bat.sub = gsub(pattern = 'flow_year',replacement = paste0('flow_',new.year),x = bat.lines)
  bat.sub = gsub(pattern = 'salt_year',replacement = paste0('salt_',new.year),x = bat.sub)
  bat.sub = gsub(pattern = 'temp_year',replacement = paste0('temp',new.year),x = bat.sub)
  bat.sub = gsub(pattern = 'volume_year',replacement = paste0('volume',new.year),x = bat.sub)
  bat.sub = gsub(pattern = 'roms_cobalt_hydroconstruct_v2.prm','roms_cobalt_hydroconstruct_temp.prm', x= bat.sub)
  
  #save batch as temp file
  writeLines(bat.sub, con = paste0(force.dir,'hydroconstruct_run_temp.bat'))
  
  #Run hydroconstruct with system()
  
  
  shell(paste0(force.dir,'hydroconstruct_run_temp.bat'))
  
  
}

# duplicate.forcing.year(
#   roms.out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/',
#   # file2copy = 'roms_output_transport_tohydro_1981.nc',
#   # transport.file = paste0(roms.out.dir,'transport/roms_output_transport_tohydro_1981.nc'),
#   transport.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/transport/roms_otuput_transport_tohydro_1981.nc',
#   # statevar.file = paste0(roms.out.dir,'statevars/roms_output_statevars_tohydro_1981.nc'),
#   statevar.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/roms_output_statevars_tohydro_1981.nc',
#   # ltlvar.file = paste0(roms.out.dir,'ltl_statevars/roms_output_ltl_statevars_tohydro_1981.nc'),
#   ltlvar.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/roms_output_ltl_statevars_tohydro_1981.nc',
#   force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/',
#   start.year = 1964,
#   new.year = 1964,
#   param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/roms_cobalt_hydroconstruct_v2.prm',
#   bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/hydroconstruct_run_template.bat'
#   
# )
