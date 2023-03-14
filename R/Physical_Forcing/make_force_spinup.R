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
#' @do.hydroconstruct logical. Whether desired to run hydroconstruct
#' @transport.file string. path to roms transport file to copy
#' @statevar.file string. path to state variables (temp and salt) file to copy
#' @anyvar.file string. path to the any forcing variable (phyto and zoo) file to copy
#' @out.dir string. Path to location of ROMS output files
#' @hydro.command string. Command to be run by hydroconstruct
#' @force.dir string. Path to location of forcing files
#' @start.year string. Year model starts (assumes 01-01-'start.year')
#' @new.year string.New year for forcing file
#' @param.temp string. Name of template hydroconstruct parameter file to modify
#' @bat.temp string. Name of template batch file (that runs hydroconstruct) to modify
#' @dynamic.mid.layer logical. If TRUE NA placed for non-surface, if FALSE, NA
#' @dynamic.bot.layer logical. If TRUE NA placed for bottom layer, else NA
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
# anyvar.file = paste0(out.dir,'phyto_statevars/SatPhyto_Forcing_1998.nc')
# anyvar.prefix = 'SatPhyto_Forcing_'
# force.dir = paste0(out.dir,'Forcing_Files/')
# start.year = 1964
# new.year = 1964
# param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm'
# bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'

make_force_spinup = function(do.hydroconstruct,
                             trans.prefix,
                             statevar.prefix,
                             anyvar.prefix,
                             transport.file,
                             statevar.file,
                             anyvar.file,
                             out.dir,
                             anyvar.out,
                             force.dir,
                             start.year,
                             new.year,
                             param.temp,
                             bat.temp,
                             mid.layer = 'normal',
                             bot.layer = 'normal'){
 
  #Make a copy of the replicated year
  if(!is.na(transport.file)){
    setwd(force.dir)
    new.trans.file = paste0(out.dir,paste0('transport/',trans.prefix,new.year,'.nc'))
    # file.copy(transport.file,new.trans.file,overwrite= T)
  }
  if(!is.na(statevar.file)){
    setwd(force.dir)
    new.statevar.file = paste0(out.dir,paste0('statevars/',statevar.prefix,new.year,'.nc'))
    # file.copy(statevar.file,new.statevar.file,overwrite=T)
  }
  if(!is.na(anyvar.file)){
    new.anyvar.file = paste0(anyvar.out,paste0(anyvar.prefix,new.year,'.nc'))
    # file.copy(anyvar.file,new.anyvar.file,overwrite=T)
  }

  t1 = seq.Date(as.Date(paste0(new.year,'-01-01 00:00:00')),as.Date(paste0(new.year,'-12-31 00:00:00')),'days')
  time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0(start.year,'-01-01 00:00:00'),tz = 'UTC'),units = 'secs')
  timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
  var.time = ncdf4::ncvar_def('time',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  
  leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)
  boxesdim = ncdf4::ncdim_def('boxes','',1:30,create_dimvar = F)
  
  #Modify and Append transport.nc
  if(!is.na(transport.file)){
    transport.nc = ncdf4::nc_open(transport.file,write = F)
    
    t1 = seq.Date(as.Date(paste0(new.year,'-01-01 00:00:00')),as.Date(paste0(new.year,'-12-31 00:00:00')),'days')
    time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0(start.year,'-01-01 00:00:00'),tz = 'UTC'),units = 'secs')
    
    timedim = ncdf4::ncdim_def('time','',1:length(time.vals),unlim = T,create_dimvar = F)
    leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)
    facesdim = ncdf4::ncdim_def('faces','',1:151,create_dimvar = F)
    
    #Built new transport file
    var.time=ncdf4::ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
    var.face=ncdf4::ncvar_def("faces", "", facesdim, longname="Face IDs", prec='integer')
    var.lev=ncdf4::ncvar_def("level","",leveldim,longname="layer index; 1=near surface",prec="integer")
    var.trans=ncdf4::ncvar_def("transport","m3/s",list(leveldim,facesdim,timedim),0,prec="float")
    var.destb=ncdf4::ncvar_def("dest_boxid","id", facesdim,longname="ID of destination box", prec="integer")
    var.sourceb=ncdf4::ncvar_def("source_boxid","id", facesdim,longname="ID of source box",prec="integer")
    var.pt1x=ncdf4::ncvar_def("pt1_x", "degree_east", facesdim, longname = "x-coord of pt 1 of face", prec='float')
    var.pt2x=ncdf4::ncvar_def("pt2_x", "degree_east", facesdim, longname = "x-coord of pt 2 of face", prec='float')
    var.pt1y=ncdf4::ncvar_def("pt1_y", "degree_north", facesdim, longname = "y-coord of pt 1 of face", prec='float')
    var.pt2y=ncdf4::ncvar_def("pt2_y", "degree_north", facesdim, longname = "y-coord of pt 1 of face", prec='float')
    
    nc_transp=ncdf4::nc_create(new.trans.file,list(var.time,var.face, var.lev, var.destb,var.sourceb, var.pt1x, var.pt2x, var.pt1y, var.pt2y,var.trans))
    
    #assign global attributes to file
    ncdf4::ncatt_put(nc_transp,0,"title","Transport file, NEUS")
    ncdf4::ncatt_put(nc_transp,0,"geometry","neus_tmerc_RM.bgm")
    ncdf4::ncatt_put(nc_transp,0,"parameters","")
    
    #assign attributes to variables
    ncdf4::ncatt_put(nc_transp,var.time,"dt",86400,prec="double")
    
    #call old values
    transport = ncdf4::ncvar_get(transport.nc,'transport')
    
    dims = dim(transport)
    
    #assign variables to file
    ncdf4::ncvar_put(nc_transp,var.lev,1:4)
    ncdf4::ncvar_put(nc_transp,var.face,0:150)
    ncdf4::ncvar_put(nc_transp,var.destb,ncdf4::ncvar_get(transport.nc,'dest_boxid'))
    ncdf4::ncvar_put(nc_transp,var.pt1x,ncdf4::ncvar_get(transport.nc,'pt1_x'))
    ncdf4::ncvar_put(nc_transp,var.pt1y,ncdf4::ncvar_get(transport.nc,'pt1_y'))
    ncdf4::ncvar_put(nc_transp,var.pt2x,ncdf4::ncvar_get(transport.nc,'pt2_x'))
    ncdf4::ncvar_put(nc_transp,var.pt2y,ncdf4::ncvar_get(transport.nc,'pt2_y'))
    ncdf4::ncvar_put(nc_transp,var.sourceb,ncdf4::ncvar_get(transport.nc,'source_boxid'))
    ncdf4::nc_close(transport.nc)
    #If leap year, append values to add extra day
    if(new.year %% 4 == 0){

      dims[3] = 366
      new.array = array(NA,dim = dims)
      last.day = transport[,,365]
      new.array[,,1:365] = transport[,,1:365]
      new.array[,,366] = last.day
      
      var.trans = ncdf4::ncvar_def('transport','m3/s',list(leveldim,facesdim,timedim),0,prec='float')
      ncdf4::ncvar_put(nc_transp,var.trans,new.array,count = c(4,151,366))
      
    } else {
      
      dims[3] = 365
      new.array = transport[,,1:365]
      
      var.trans = ncdf4::ncvar_def('transport','m3/s',list(leveldim,facesdim,timedim),0,prec='float')
      ncdf4::ncvar_put(nc_transp,var.trans,new.array,count = c(4,151,365))
    }
    ncdf4::ncvar_put(nc_transp,var.time,time.vals)
    ncdf4::ncatt_put(nc_transp,'time','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
    
    ncdf4::nc_close(nc_transp)
  }

  if(!is.na(statevar.file)){
    # leveldim = ncdf4::ncdim_def('level','',1:5,create_dimvar = F)
    
    var.time=ncdf4::ncvar_def("time","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
    var.box=ncdf4::ncvar_def("boxes", "", boxesdim, longname="Box IDs", prec='integer')
    var.lev=ncdf4::ncvar_def("level","",leveldim,longname="layer index; 1=near surface; positice=down" ,prec="integer")

    var.def.ls = list(var.time,var.box,var.lev)
    
    #Modify and append statevar file
    statevar.nc = ncdf4::nc_open(statevar.file,write = F)
    var.names = names(statevar.nc$var)
    var.units = c('psu','deg C','m3/s')
    
    #If leap year, append values to add extra day
    new.var.dat.ls = list()
    for(v in 1:length(var.names)){
      var.dat = ncdf4::ncvar_get(statevar.nc,var.names[v])
      var.def.ls[[v+3]] = var.vertflux=ncdf4::ncvar_def(var.names[v],var.units[v],list(leveldim, boxesdim, timedim),-999,prec="float")
      dims = dim(var.dat)
      if(new.year %% 4 == 0){
        
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
    ncdf4::nc_close(statevar.nc)
    
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
  
  #Phyto Statevariables
  if(!is.na(anyvar.file)){
    # leveldim = ncdf4::ncdim_def('z','',1:5,create_dimvar = F)
    # boxesdim = ncdf4::ncdim_def('b','',1:30,create_dimvar = F)
    # timedim = ncdf4::ncdim_def('t','',1:length(time.vals),unlim = T,create_dimvar = F)
    # var.time = ncdf4::ncvar_def('t',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
    # 
    # #Create dim vars
    # var.time=ncdf4::ncvar_def("t","seconds since 1964-01-01 00:00:00 +10",timedim,prec="double")
    # var.box=ncdf4::ncvar_def("b", "", boxesdim, longname="Box IDs", prec='integer')
    # var.lev=ncdf4::ncvar_def("z","",leveldim,longname="layer index; 1=near surface; positice=down" ,prec="integer")
    
    # var.def.ls = list(var.time,var.box,var.lev)
    
    #Modify and append statevar file
    anyvar.nc = ncdf4::nc_open(anyvar.file,write = F)
    
    var.names = names(anyvar.nc$var)
    var.units = sapply(var.names,function(x) return(ncdf4::ncatt_get(anyvar.nc,x,'units')$value))
    var.longname = sapply(var.names,function(x) return(ncdf4::ncatt_get(anyvar.nc,x,'long_name')$value))
    
    #If leap year, append values to add extra day
    new.var.dat.ls = list()
    for(v in 1:length(var.names)){
      var.dat = ncdf4::ncvar_get(anyvar.nc,var.names[v])
      # var.def.ls[[v+3]] = ncdf4::ncvar_def(var.names[v],var.units[v],list(leveldim, boxesdim, timedim),-999,longname=var.longname[v],prec="float")
      
      dims = dim(var.dat)
      dims[3] = length(time.vals)
      if(mid.layer == 'zero'){
        dum.array = array(0,dim = dims)  
      }else {
        dum.array = array(NA,dim = dims)
      }
      
      if(bot.layer == 'dynamic'){
        dum.array[5,,] = NA  
      }else if(bot.layer == 'zero'){
        dum.array[5,,] = 0
      }
      
      if(new.year %% 4 == 0){
        new.var.dat = dum.array
        last.var = var.dat[,,365]
        new.var.dat[,,1:365] = var.dat[,,1:365]
        new.var.dat[,,366] = last.var
      } else {
       new.var.dat = var.dat[,,1:365]
      }
      new.var.dat.ls[[v]] = new.var.dat
    }
    ncdf4::nc_close(anyvar.nc)
    
    #Build new NC File
    #call new netCDF file
    nc.file = RNetCDF::create.nc(new.anyvar.file)
    
    RNetCDF::dim.def.nc(nc.file, "t", unlim=TRUE)
    RNetCDF::dim.def.nc(nc.file, "b", 30)
    RNetCDF::dim.def.nc(nc.file, "z", 5)
    
    RNetCDF::var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
    for(v in 1:length(var.names)){
      #Define Variables
      RNetCDF::var.def.nc(nc.file, var.names[v], 'NC_DOUBLE', c('z','b','t'))
      #Assign Fill Value
      RNetCDF::att.put.nc(nc.file, var.names[v], '_FillValue', "NC_DOUBLE", -999)
      #Assign 
      RNetCDF::att.put.nc(nc.file, var.names[v], 'missing_value', 'NC_DOUBLE',-999)
      #Assign valid_min
      RNetCDF::att.put.nc(nc.file, var.names[v], 'valid_min', 'NC_DOUBLE', -999)
      #Assing valid_max
      RNetCDF::att.put.nc(nc.file, var.names[v], 'valid_max', 'NC_DOUBLE', 99999)
      #Assign units
      RNetCDF::att.put.nc(nc.file, var.names[v], 'units','NC_CHAR', var.units[v])  
      #Assign long_name
      RNetCDF::att.put.nc(nc.file,var.names[v],'long_name','NC_CHAR',var.longname[v])
      
      #Put variable values
      RNetCDF::var.put.nc(nc.file,var.names[v],new.var.dat.ls[[v]])
    }
    
    RNetCDF::att.put.nc(nc.file, "t", "units", "NC_CHAR", 'seconds since 1964-01-01 00:00:00 UTC')
    RNetCDF::att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'NEUS_Atlantis_Obs_Hindcast')
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
    
    RNetCDF::var.put.nc(nc.file, "t", as.numeric(time.vals))
    
    
    RNetCDF::close.nc(nc.file)
    
    
    # nc_varfile = ncdf4::nc_create(new.anyvar.file,var.def.ls)
    # 
    # #assign global attributes to file
    # ncdf4::ncatt_put(nc_varfile,0,"title","Box averaged properties file, NEUS")
    # ncdf4::ncatt_put(nc_varfile,0,"geometry","neus_tmerc_RM.bgm")
    # ncdf4::ncatt_put(nc_varfile,0,"parameters","")
    # 
    # #assign attributes to variables
    # ncdf4::ncatt_put(nc_varfile,var.time,"dt",86400,prec="double")
    # 
    # for(v in 1:length(var.names)){
    #   ncdf4::ncvar_put(nc_varfile,var.def.ls[[v+3]],new.var.dat.ls[[v]], count=c(5,30,length(time.vals)),verbose = F)
    # 
    #   ncdf4::ncatt_put(nc_varfile,var.names[v],'_FillValue',-999,prec='double',)
    #   ncdf4::ncatt_put(nc_varfile,var.names[v],'missing_value',-999,prec='double')
    #   ncdf4::ncatt_put(nc_varfile,var.names[v],'valid_min',-999,prec='double')
    #   ncdf4::ncatt_put(nc_varfile,var.names[v],'valid_max',99999,prec='double')
    # }
    # ncdf4::ncvar_put(nc_varfile,var.time,time.vals,verbose = F)
    # ncdf4::ncvar_put(nc_varfile,var.lev,1:5)
    # ncdf4::ncvar_put(nc_varfile,var.box,0:29)
    # 
    # ncdf4::ncatt_put(nc_varfile,'t','units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
    # 
    # ncdf4::nc_close(nc_varfile)
  }

  #Run Hydroconstruct with proper start year
  if(do.hydroconstruct){

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

