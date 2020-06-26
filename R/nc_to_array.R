#Function converts a spatial netCDF file with (lat/lon) coordinates into a flat array with atlantis depths 
nc_to_array = function(nc.dir,nc.file,var.name,lat.range,lon.range){
  lat.min = lat.range[1]
  lat.max = lat.range[2]
  lon.min = lon.range[1]
  lon.max = lon.range[2]
  
  data.nc = nc_open(paste0(nc.dir,nc.file))
  nc.split = strsplit(nc.file,'/')[[1]]
  
  # nc.dump = ncdump::NetCDF(nc.file)
  varnames = names(data.nc$var)
  var.dat = ncvar_get(data.nc, var.name)
  lat.bnds = ncvar_get(data.nc,'lat_bnds')
  lon.bnds = ncvar_get(data.nc,'lon_bnds')
  depth.bnds = ncvar_get(data.nc,'depth_bnds')
  neus.depths =apply(depth.bnds,2,function(z){
    if(z[1]<=50){
      return(1)
    }else if(z[1]<=120){
      return(2)
    }else if(z[1]<=300){
      return(3)
    }else if(z[1]<=500){
      return(4)
    }else {
      return(NA)
    }
  })
  
  #Crop to specified bounds
  lat.crop = which(lat.bnds[1,]>=lat.min & lat.bnds[2,]<=lat.max)
  lon.crop = which(lon.bnds[1,]>=lon.min & lon.bnds[2,]<=lon.max)
  depth.crop = which(!is.na(neus.depths))
  
  var.crop = var.dat[lon.crop,lat.crop,depth.crop]
  new.lat.bnds = lat.bnds[,lat.crop]
  new.lon.bnds = lon.bnds[,lon.crop]
  new.depth.bnds = depth.bnds[,depth.crop]
  new.neus.depths = neus.depths[depth.crop]
  
  var.neus.lev = array(NA,dim = c(length(lon.crop),length(lat.crop),4))
  for(i in 1:4){var.neus.lev[,,i] = apply(var.crop[,,which(new.neus.depths == i)], c(1,2),mean,na.rm=T)}
  # dim(var.neus.lev)
  
  time.names = filename2date(nc.file)
  
  new.lat.bnds = rbind(new.lat.bnds,colMeans(new.lat.bnds))
  new.lon.bnds = rbind(new.lon.bnds,colMeans(new.lon.bnds))
  out.list = list(data = var.neus.lev,
                  filename = nc.file,
                  varname = var.name,
                  full.name = paste0(nc.dir,nc.file),
                  lat_bnds = new.lat.bnds,
                  lon_bnds = new.lon.bnds,
                  # depth_bnds = new.depth.bnds,
                  # neus_depths = 1:4,
                  neus_depths = neus.depths,
                  year.range = time.names$year.range,
                  month = time.names$month.name)
  
}