#Duplicates data from one annual forcing file onto another year and updates timestamps

# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/bias_corrected_tempsalt/'
# reference.file = 'roms_tempsalt_force_1981_debias.nc'
# start.year = 1964
# new.year = 1964
# var.names = c('temperature','salinity')
# is.boxvar = T
# t.dim.name = 't'


duplicate_force_year = function(force.dir, reference.file, start.year, new.year, is.boxvar=T, t.dim.name = 't'){

  #copy file with new name
  ref.year = as.numeric(sort(gsub(".*(\\d{4}).+","\\1",reference.file)))
  base.name = strsplit(reference.file,split = ref.year)[[1]]
  new.name = paste(base.name, collapse = as.character(new.year))
  file.copy(paste0(force.dir,reference.file),paste0(force.dir,new.name),overwrite = T)
  
  #Open new file and adjust each variable
  force.nc = ncdf4::nc_open(paste0(force.dir,new.name),write = T)
  var.names = names(force.nc$var)
  var.units = var.longname = character()
  for(i in 1:length(var.names)){
    var.units[i] = ncdf4::ncatt_get(force.nc,var.names[i],'units')$value
    var.longname[i]= ncdf4::ncatt_get(force.nc,var.names[i],'long_name')$value
    }
  
  t1 = seq.Date(as.Date(paste0(new.year,'-01-01')),as.Date(paste0(new.year,'-12-31')),by = '+1 day')
  ndays = length(t1)
  time.vals =difftime(as.POSIXct(t1,tz = 'UTC'),as.POSIXct(paste0(start.year,'-01-01 00:00:00'),tz = 'UTC'),units = 'secs')
  timedim = ncdf4::ncdim_def(t.dim.name,'',1:ndays,unlim = T,create_dimvar = F)
  var.time = ncdf4::ncvar_def(t.dim.name,paste0('seconds since ',start.year,'-01-01 00:00:00 +10'),timedim,prec='double')
  
  leveldim = ncdf4::ncdim_def('level','',1:4,create_dimvar = F)
  boxesdim = ncdf4::ncdim_def('boxes','',1:30,create_dimvar = F)
  ncdf4::ncatt_put(force.nc,t.dim.name,'units',paste0('seconds since ',start.year,'-01-01 00:00:00 +10'))
  
  ncdf4::ncvar_put(force.nc,var.time,time.vals)
  
  if(!is.boxvar){
    
    #If leap year, append values to add extra day
    if(new.year %% 4 == 0){
      transport = ncdf4::ncvar_get(force.nc,'transport')
      nface = dim(transport)[2]
      facesdim = ncdf4::ncdim_def('faces','',1:nface,create_dimvar = F)
      dims = dim(transport)
      dims[3] = 366
      new.array = array(NA,dim = dims)
      last.day = transport[,,365]
      new.array[,,1:365] = transport[,,1:365]
      new.array[,,366] = last.day
      
      var.trans = ncdf4::ncvar_def('transport','m3/s',list(leveldim,facesdim,timedim),0,prec='float')
      ncdf4::ncvar_put(force.nc,var.trans,new.array,count = c(4,151,366))
    }
  } else {
    
    if(new.year %% 4 == 0){
      
      for( v in 1:length(var.names)){
        
        var.data = ncdf4::ncvar_get(force.nc,var.names[v])  
        dims = dim(var.data)
        dims[3] = 366
        new.var.data = array(NA,dim = dims)
        last.day = var.data[,,365]
        new.var.data[,,1:365] = var.data[,,1:365]
        new.var.data[,,366] = last.day
        var.def = ncdf4::ncvar_def(var.names[v],var.units[v],list(leveldim,boxesdim,timedim),-999,longname = var.longname[v],prec = 'float')
        ncdf4::ncvar_put(force.nc,var.def,new.var.data,count = c(4,30,366))
      }
    }
    
  }
  
  ncdf4::nc_close(force.nc)
  
}
