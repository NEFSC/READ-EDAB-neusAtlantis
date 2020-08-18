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
#' @out.dir string. Path to location of ROMS output files
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

# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'
# trans.prefix = 'GLORYS_Atlantis_Transport_'
# statevar.prefix = 'Obs_Hindcast_statevars_'
# # transport.file = paste0(out.dir,'transport/GLORYS_Atlantis_Transport_1993.nc')
# transport.file = NA
# # statevar.file = paste0(out.dir,'statevars/Obs_Hindcast_statevars_1993.nc')
# statevar.file = NA
# phyto.file = paste0(out.dir,'phyto_statevars/SatPhyto_Forcing_1998.nc')
# phyto.prefix = 'SatPhyto_Forcing_'
# force.dir = paste0(out.dir,'Forcing_Files/')
# start.year = 1964
# new.year = 1964
# param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm'
# bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'

make_force_spinup = function(trans.prefix,
                             statevar.prefix,
                             phyto.prefix,
                             transport.file,
                             statevar.file,
                             phyto.file,
                             out.dir,
                             force.dir,
                             start.year,
                             new.year,
                             param.temp,
                             bat.temp){
  setwd(force.dir)
  #Make a copy of the replicated year
  if(!is.na(transport.file)){
    new.trans.file = paste0(out.dir,paste0('transport/',trans.prefix,new.year,'.nc'))
    file.copy(transport.file,new.trans.file,overwrite= T)
  }
  if(!is.na(statevar.file)){
    new.statevar.file = paste0(out.dir,paste0('statevars/',statevar.prefix,new.year,'.nc'))
    file.copy(statevar.file,new.statevar.file,overwrite=T)
  }
  if(!is.na(phyto.file)){
    new.phyto.file = paste0(out.dir,paste0('phyto_statevars/',phyto.prefix,new.year,'.nc'))
    file.copy(phyto.file,new.phyto.file,overwrite=T)
  }

  t1 = seq.Date(as.Date(paste0(new.year,'-01-01 00:00:00')),as.Date(paste0(new.year,'-12-31 00:00:00')),'days')
  time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0(start.year,'-01-01 00:00:00'),tz = 'UTC'),units = 'secs')
  timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
  var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  
  leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)
  boxesdim = ncdf4::ncdim_def('boxes','',1:30,create_dimvar = F)
  
  #Modify and Append transport.nc
  if(!is.na(transport.file)){
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
    
    
    ncdf4::ncatt_put(transport.nc,'time','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
    
    ncdf4::ncvar_put(transport.nc,var.time,time.vals)
    ncdf4::nc_close(transport.nc)
  }
  
  if(!is.na(statevar.file)){
    #Modify and append statevar file
    statevar.nc = ncdf4::nc_open(new.statevar.file,write = T)
    
 
    #If leap year, append values to add extra day
    if(new.year %% 4 == 0){
      
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
  }
  
  #Phyto Statevariables
  if(!is.na(phyto.file)){
    leveldim = ncdf4::ncdim_def('level','',1:5,create_dimvar = F) 
    
    timedim = ncdf4::ncdim_def('t','',1:length(time.vals),unlim = T,create_dimvar = F)
    var.time = ncdf4::ncvar_def('t',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
    
    #Modify and append statevar file
    phyto.nc = ncdf4::nc_open(new.phyto.file,write = T)
    
    
    #If leap year, append values to add extra day
    if(new.year %% 4 == 0){
      
      diatom.n = ncdf4::ncvar_get(phyto.nc,'Diatom_N')
      dinoflag = ncdf4::ncvar_get(phyto.nc,'DinoFlag_N')
      picophyto = ncdf4::ncvar_get(phyto.nc,'PicoPhytopl_N')
      diatom.s = ncdf4::ncvar_get(phyto.nc,'Diatom_S')
      
      dims = dim(diatom.n)
      dims[3] = 366
      new.diatom.n = new.dinoflag = new.picophyto = new.diatom.s = array(NA,dim = dims)
      
      last.diatom.n = diatom.n[,,365]
      last.dinoflag = dinoflag[,,365]
      last.picophyto = picophyto[,,365]
      last.diatom.s = diatom.s[,,365]
      
      new.diatom.n[,,1:365] = diatom.n[,,1:365]
      new.dinoflag[,,1:365] = dinoflag[,,1:365]
      new.picophyto[,,1:365] = picophyto[,,1:365]
      new.diatom.s[,,1:365] = diatom.s[,,1:365]
      
      new.diatom.n[,,366] = last.diatom.n
      new.dinoflag[,,366] = last.dinoflag
      new.picophyto[,,366] = last.picophyto
      new.diatom.s[,,366] = last.diatom.s
      
      var.diatom.n = ncdf4::ncvar_def("Diatom_N","mg N m-3",list(leveldim, boxesdim, timedim),-999,longname="Diatom Nitrogen",prec="float")
      var.dinoflag = ncdf4::ncvar_def("DinoFlag_N","mg N m-3",list(leveldim, boxesdim, timedim),-999,longname="Dinoflagellate Nitrogen",prec="float")
      var.picophyto = ncdf4::ncvar_def("PicoPhytopl_N","mg N m-3",list(leveldim, boxesdim, timedim),-999,longname="Picophytoplankton Nitrogen",prec="float")
      var.diatom.s = ncdf4::ncvar_def("Diatom_S","mg Si m-3",list(leveldim, boxesdim, timedim),-999,longname="Diatom Silicon",prec="float")
      
      ncdf4::ncvar_put(phyto.nc,var.diatom.n, new.diatom.n,count=c(5,30,366) )
      ncdf4::ncvar_put(phyto.nc,var.dinoflag, new.dinoflag, count=c(5,30,366))
      ncdf4::ncvar_put(phyto.nc,var.picophyto,new.picophyto,count=c(5,30,366))
      ncdf4::ncvar_put(phyto.nc,var.diatom.s, new.diatom.s,count=c(5,30,366))
      
    } 
    
    # var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
    ncdf4::ncatt_put(phyto.nc,'t','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
    
    ncdf4::ncvar_put(phyto.nc,var.time,time.vals)
    ncdf4::nc_close(phyto.nc)
  }
  
  #Run Hydroconstruct with proper start year
  if(!is.na(transport.file) & !is.na(statevar.file)){

    t.start = as.numeric(difftime(as.Date(paste0(new.year,'-01-01 00:00:00'),tz='UTC'),as.Date('1964-01-01 00:00:00',tz = 'UTC'),'days'))
    t.stop =(t.start + length(time.vals))-1
    
    param.lines = readLines(param.temp)
    bat.lines = readLines(bat.temp)
    
    #sub parameter values
    param.sub = gsub(pattern = 'transport.nc',replacement = new.trans.file,x = param.lines)
    param.sub = gsub(pattern = 'vtrans.nc',replacement = new.statevar.file,x=param.sub)
    param.sub = gsub(pattern = 'tempsalt.nc',replacement = new.statevar.file,x = param.sub)
    param.sub = gsub(pattern = 'reference_year 1964',replacement = paste0('reference_year ',start.year),x=param.sub)
    param.sub = gsub(pattern = 'tstop 1',replacement = paste0('tstop ',t.stop),x =param.sub)
    param.sub = gsub(pattern = 'tstart 1',replacement = paste0('tstart ',t.start),x=param.sub)
    # param.sub = gsub(pattern = 'tstop 1',replacement = paste0('tstop ',length(time.vals)),x =param.sub)
    
    #save as yearly temp param file
    writeLines(param.sub,con = paste0(force.dir,'obs_hindcast_hydroconstruct_temp.prm'))
    
    #sub batch file values
    bat.sub = gsub(pattern = 'flow_year',replacement = paste0('flow_',new.year),x = bat.lines)
    bat.sub = gsub(pattern = 'salt_year',replacement = paste0('salt_',new.year),x = bat.sub)
    bat.sub = gsub(pattern = 'temp_year',replacement = paste0('temp_',new.year),x = bat.sub)
    bat.sub = gsub(pattern = 'volume_year',replacement = paste0('volume',new.year),x = bat.sub)
    bat.sub = gsub(pattern = 'obs_hindcast_hydroconstruct.prm','obs_hindcast_hydroconstruct_temp.prm', x= bat.sub)
    
    #save batch as temp file
    writeLines(bat.sub, con = paste0(force.dir,'hydroconstruct_run_temp.bat'))
    
    #Run hydroconstruct with system()
    
    
    shell(paste0(force.dir,'hydroconstruct_run_temp.bat'))
  }

  
  
}

