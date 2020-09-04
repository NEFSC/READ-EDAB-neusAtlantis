#' @description Takes .csv output from satellite primary production model DOY climatology (1997-2019), converts it to biomass units (mgN/m-3)
#' and transforms it to atlantis-structured array. Then array is converted to netCDF output with appropriate 
#'  attritbutes.
#'  
#'  @in.dir string. Input file directory
#'  @in.file string. Input file name
#'  @out.dir string. Output directory
#'  @out.name string. Output file name
#'  @stat.var string. Name of summary statitic column used for aggregation
#'  @bio.vars Character vector of primary producer variable names
#'  @atl.groups character vector of atlantis phytoplankton group names
#'  @atl.varname Character vector length atl.groups with full Atlantis name (for netCDF variables). Should match initial conditions file
#'  @atl.longname Character vector length atl.groups with full descriptive name for each atlantis variable.
#'  @phtyo.fract data frame containing the fraction of each bio.var assigned to each atl.group (e.g. 0.5 of MICRO to PL)
#'  @chl.conv numeric vector same length as bio.vars that has the Cholorphyll to biomass conversion factor
#'  
#'Author: J. Caracappa
#'

# in.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/'
# in.file = 'DOY-OCCCI-ATLANTIS_NEUS-VER_1.csv'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/'
# out.file = 'Phyto_Climatology'
# stat.var = 'MED'
# bio.vars = c('MICRO','NANO','PICO')
# atl.groups = c('PL','DF','PS')
# atl.varname = c('Diatom_N','DinoFlag_N','PicoPhytopl_N')
# atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen')
# phyto.fract = data.frame(PL = c(0.5,0,0),DF = c(0.5,0.5,0), PS = c(0,0.5,1))
# chl.conv = rep(7,3)


make_SatPhyto_climatology = function(in.dir,
                                     in.file,
                                     out.dir,
                                     out.file,
                                     stat.var,
                                     bio.vars,
                                     atl.groups,
                                     atl.varname,
                                     atl.longname,
                                     phyto.fract,
                                     chl.conv){
  `%>%` = dplyr::`%>%`
  
  #Read in data from CSV
  data = read.csv(paste0(in.dir,in.file),header = T,as.is = T)
  #Remove unused columns
  data = data %>% dplyr::select(PROD,PERIOD,UNITS,SUBAREA,N_SUBAREA,all_of(stat.var)) %>% dplyr::rename( 'STAT' =all_of(stat.var))
  
  #Format DOY and DATE columns
  data$DOY = unname(sapply(data$PERIOD,function(x){return(strsplit(x,'_')[[1]][2])}))
  ref.year = 1998
  ref.year.dates = seq.Date(as.Date(paste0(ref.year,'-01-01')),as.Date(paste0(ref.year,'-12-31')),by = 1)
  data$DATE = ref.year.dates[as.numeric(data$DOY)]
  
  #Format Data into array
  prod.ls = list()
  boxes = 0:29
  
  v=1
  for(v in 1:length(bio.vars)){
    
    var.array = array(NA,dim = c(5,30,length(ref.year.dates)))
    
    #Loop over boxes and put int array (by day)
    for(b in 1:length(boxes)){
      var.box = data %>% 
        dplyr::filter(PROD == bio.vars[v] & SUBAREA == boxes[b]) %>%
        dplyr::arrange(DATE) 
      var.array[1,b,] = var.box$STAT
    }
    
    #Convert Chl to Nitrogen
    var.array = var.array / chl.conv[v]
    
    #Put into Prod.ls
    prod.ls[[v]] = var.array
  }
  
  #Assign Producer groups to Atlantis groups using phyto.fract
  atl.var.ls = list()
  
  for(i in 1:length(atl.groups)){
    
    atl.array = array(0,dim = c(5,30,length(ref.year.dates)))
    for(j in 1:length(bio.vars)){
      atl.array = atl.array+ (prod.ls[[j]]*phyto.fract[j,i])
    }
    atl.var.ls[[i]] = atl.array
  }
  
  #Quick test. All equal: MICRO, 2*PL, 2*DF - NANO
  # plot(prod.ls[[1]][1,1,],type='l')
  # lines(atl.var.ls[[1]][1,1,]*2,col = 'red')
  # lines(atl.var.ls[[2]][1,1,]*2-prod.ls[[2]][1,1,], col = 'green')
  
  #Format as netCDF
  
  #Dimension values
  levels = 1:5
  t_tot = as.numeric(difftime(as.POSIXct(ref.year.dates,tz='UTC'),as.POSIXct('1964-01-01 00:00:00',tz='UTC'),units = 'secs'))
  #Test t_tot dates
  # as.POSIXct(t_tot,origin = '1964-01-01 00:00:00',tz = 'UTC')
  
  #call new netCDF file
  filename = paste0(out.dir,out.file,'.nc')
  
  nc.file = RNetCDF::create.nc(filename)
  
  RNetCDF::dim.def.nc(nc.file, "time", unlim=TRUE)
  RNetCDF::dim.def.nc(nc.file, "b", 30)
  RNetCDF::dim.def.nc(nc.file, "z", 5)
  
  RNetCDF::var.def.nc(nc.file, "time", "NC_DOUBLE", "time")
  for(v in 1:length(atl.var.ls)){
    var.name = atl.varname[v]
    #Define Variables
    RNetCDF::var.def.nc(nc.file, atl.varname[v], 'NC_DOUBLE', c('z','b','time'))
    #Assign Fill Value
    RNetCDF::att.put.nc(nc.file, atl.varname[v], '_FillValue', "NC_DOUBLE", 0)
    #Assign 
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'missing_value', 'NC_DOUBLE',0)
    #Assign valid_min
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'valid_min', 'NC_DOUBLE', 0)
    #Assing valid_max
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'valid_max', 'NC_DOUBLE', 99999)
    #Assign units
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'units','NC_CHAR', 'mg N m-3')
    #Assign long_name
    RNetCDF::att.put.nc(nc.file,atl.varname[v],'long_name','NC_CHAR',atl.longname[v])
    
    #Put variable values
    RNetCDF::var.put.nc(nc.file,atl.varname[v],atl.var.ls[[v]])
  }
  
  RNetCDF::att.put.nc(nc.file, "time", "units", "NC_CHAR", 'seconds since 1964-01-01 00:00:00 UTC')
  RNetCDF::att.put.nc(nc.file, "time", "dt", "NC_DOUBLE", 86400)
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'NEUS_Atlantis_Obs_Hindcast')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  RNetCDF::var.put.nc(nc.file, "time", t_tot)
  
  
  RNetCDF::close.nc(nc.file)
  
}