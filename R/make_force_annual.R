#Script to generate forcing files using hydrocontstruct (This is mainly for the fluxes files, but temp and salt work as well)
#see make_force_statevar_alternate.R for alternative forcing generation

trans.dir = 'C:/Users/joseph.caracappa/Documents/GLORYS/'
statevar.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/statevars/'
atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/'
setwd(paste0(atl.dir,'Forcing_Files/'))
orig.prm = readLines(paste0(atl.dir,'Forcing_Files/obs_hindcast_hydroconstruct_template.prm'))
orig.bat = readLines(paste0(atl.dir,'Forcing_Files/hydroconstruct_run_template.bat'))
trans.prefix = 'GLORYS_Atlantis_'
statevar.prefix = 'Obs_Hindcast_statevars_'
years = 1993:2017
yr=1
for(yr in 1:length(years)){
  transport.file = paste0(trans.dir,'Atlantis_Format/',years[yr],'/',trans.prefix,'transport_',years[yr],'.nc')
  vtrans.file = paste0(statevar.dir,statevar.prefix,years[yr],'.nc')
  tempsalt.file = paste0(statevar.dir,statevar.prefix,years[yr],'.nc')
  dumm.nc = ncdf4::nc_open(transport.file)
  nt = length(dumm.nc$dim$time$vals)
  ncdf4::nc_close(dumm.nc)
  
  t.start = as.numeric(difftime(as.Date(paste0(years[yr],'-01-01 00:00:00'),tz='UTC'),as.Date('1964-01-01 00:00:00',tz = 'UTC'),'days'))
  t.stop =(t.start + nt)-1
  
  #sub parameter values
  orig.sub = gsub(pattern = 'transport.nc',replacement = transport.file,x = orig.prm)
  orig.sub = gsub(pattern = 'vtrans.nc',replacement = vtrans.file,x=orig.sub)
  orig.sub = gsub(pattern = 'tempsalt.nc',replacement = tempsalt.file,x = orig.sub)
  orig.sub = gsub(pattern = 'tstop 1',replacement = paste0('tstop ',t.stop),x =orig.sub)
  orig.sub = gsub(pattern = 'tstart 1',replacement = paste0('tstart ',t.start),x=orig.sub)
  
  #save as yearly temp param file
  writeLines(orig.sub,con = paste0(atl.dir,'Forcing_Files/obs_hindcast_hydroconstruct_temp.prm'))
  
  #sub batch file values
  bat.sub = gsub(pattern = 'flow_year',replacement = paste0('flow_',years[yr]),x = orig.bat)
  bat.sub = gsub(pattern = 'salt_year',replacement = paste0('salt_',years[yr]),x = bat.sub)
  bat.sub = gsub(pattern = 'temp_year',replacement = paste0('temp_',years[yr]),x = bat.sub)
  bat.sub = gsub(pattern = 'volume_year',replacement = paste0('volume',years[yr]),x = bat.sub)
  bat.sub = gsub(pattern = 'obs_hindcast_hydroconstruct.prm','obs_hindcast_hydroconstruct_temp.prm', x= bat.sub)
  
  #save batch as temp file
  writeLines(bat.sub, con = paste0(atl.dir,'Forcing_Files/hydroconstruct_run_temp.bat'))
  
  #Run hydroconstruct with system()
  
  shell(paste0(atl.dir,'Forcing_Files/hydroconstruct_run_temp.bat'))
  
  
}



