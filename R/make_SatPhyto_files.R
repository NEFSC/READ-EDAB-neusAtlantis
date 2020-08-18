#' @description Takes annual .csv output from satellite primary production model output, converts it to biomass units (mgN/m-3)
#' and transforms it to atlantis-structured array. Then array is converted to netCDF output with appropriate 
#'  attritbutes.
#'  
#'  @in.dir string. Input file directory
#'  @in.file string. Input file name
#'  @out.dir string. Output directory
#'  @out.name string. Output file name
#'  @stat.var string. Name of summary statitic column used for aggregation
#'  @bio.vars Character vector of primary producer variable names
#'  @atl.varname Character vector length atl.groups with full Atlantis name (for netCDF variables). Should match initial conditions file
#'  @atl.longname Character vector length atl.groups with full descriptive name for each atlantis variable.
#'  @atl.units character vector of units from initial conditions file
#'  @phtyo.fract matrix (box x time) that contains the Diatom:Dinoflagellate ratio for the microphytoplankton
#'  @chl.conv numeric vector same length as bio.vars that has the Cholorphyll to biomass conversion factor
#'  
#'Author: J. Caracappa
#'
# 
# in.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
# in.file = 'D8-OCCCI-ATLANTIS_2000.csv'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/'
# out.file = 'Phyto_2000'
# stat.var = 'MED'
# bio.vars = c('MICRO','NANO','PICO')
# atl.varname = c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
# atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicon')
# # phyto.fract = data.frame(PL = c(0.5,0,0),DF = c(0.5,0.5,0), PS = c(0,0.5,1))
# phyto.fract = matrix(0.75,nrow = 30, ncol = 366)
# chl.conv = rep(7,3)


make_SatPhyto_files = function(in.dir,
                                     in.file,
                                     out.dir,
                                     out.file,
                                     stat.var,
                                     bio.vars,
                                     atl.varname,
                                     atl.longname,
                                     atl.units,
                                     phyto.fract,
                                     chl.conv){
  
  
  source(here::here('R','fill_satphyto_gaps.R'))
  
  `%>%` = dplyr::`%>%`
  
  #Read in data from CSV
  data = read.csv(paste0(in.dir,in.file),header = T,as.is = T)
  data$mid = as.Date(data$mid)
  
  #Remove unused columns
  data = data %>% 
    dplyr::select(PROD,PERIOD,mid,mid.year,UNITS,SUBAREA,N_SUBAREA,all_of(stat.var)) %>% 
    dplyr::rename( 'STAT' =all_of(stat.var))
  
  #Format all dates
  ref.year = unique(data$mid.year)
  ref.year.dates = seq.Date(as.Date(paste0(ref.year,'-01-01')),as.Date(paste0(ref.year,'-12-31')),by = 1)
  
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
        dplyr::arrange(mid)
      date.match = which(ref.year.dates %in% var.box$mid)
      var.array[1,b,date.match] = var.box$STAT
    }
    
    #Convert Chl to Nitrogen
    var.array = var.array * chl.conv[v]
    
    #Put into Prod.ls
    prod.ls[[v]] = var.array
  }
  
  #Assign Producer groups to Atlantis groups using phyto.fract
  atl.var.ls = lapply(1:4,function(x) array(NA,dim = c(5,30,length(ref.year.dates))))
  names(atl.var.ls) = atl.varname
  
  #Extract appropriate fractions and assign Atlantis variables
  for(d in 1:length(ref.year.dates)){
    #Diatom calculation
    atl.var.ls[[1]][1,,d] = prod.ls[[1]][1,,d]*phyto.fract[,d]
    
    #Dinoflagellate Calculation
    atl.var.ls[[2]][1,,d] = prod.ls[[1]][1,,d]*(1-phyto.fract[,d])
    
    #Small Phytoplankton Calculation
    atl.var.ls[[3]][1,,d] = prod.ls[[2]][1,,d]+prod.ls[[3]][1,,d]

  }
  
  for(i in 1:length(atl.var.ls)){
    if(names(atl.var.ls)[i] == 'Diatom_S'){
      var.name = 'Diatom_N'
    }else{
      var.name = atl.varname[i]
    }
    atl.var.ls[[i]] = fill_satphyto_gaps(input.mat = atl.var.ls[[i]],
                       var.name = var.name,
                       doy.file = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/Phyto_Climatology.nc',
                       max.interp = 8,
                       write.gaps = T,
                       gaps.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Diagnostics/Gap_Analysis/',
                       ref.year = ref.year,
                       ref.year.dates = ref.year.dates
                       )
  }
  
  
  #Diatom_S Using Si:N = 1.1
  # From Brzezinski 1985
  atl.var.ls[[4]] = atl.var.ls[[1]] * 1.1
  
  # save(atl.var.ls,file= paste0(out.dir,'var_test.R'))
  #Format as netCDF
  
  #Dimension values
  levels = 1:5
  t_tot = as.numeric(difftime(as.POSIXct(ref.year.dates,tz='UTC'),as.POSIXct('1964-01-01 00:00:00',tz='UTC'),units = 'secs'))
  #Test t_tot dates
  # as.POSIXct(t_tot,origin = '1964-01-01 00:00:00',tz = 'UTC')
  
  #call new netCDF file
  filename = paste0(out.dir,out.file,'.nc')
  
  nc.file = RNetCDF::create.nc(filename)
  
  RNetCDF::dim.def.nc(nc.file, "t", unlim=TRUE)
  RNetCDF::dim.def.nc(nc.file, "b", 30)
  RNetCDF::dim.def.nc(nc.file, "z", 5)
  
  RNetCDF::var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
  for(v in 1:length(atl.var.ls)){
    var.name = atl.varname[v]
    #Define Variables
    RNetCDF::var.def.nc(nc.file, atl.varname[v], 'NC_DOUBLE', c('z','b','t'))
    #Assign Fill Value
    RNetCDF::att.put.nc(nc.file, atl.varname[v], '_FillValue', "NC_DOUBLE", 0)
    #Assign 
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'missing_value', 'NC_DOUBLE',0)
    #Assign valid_min
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'valid_min', 'NC_DOUBLE', 0)
    #Assing valid_max
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'valid_max', 'NC_DOUBLE', 99999)
    #Assign units
    RNetCDF::att.put.nc(nc.file, atl.varname[v], 'units','NC_CHAR', atl.units[v])  
    #Assign long_name
    RNetCDF::att.put.nc(nc.file,atl.varname[v],'long_name','NC_CHAR',atl.longname[v])
    
    #Put variable values
    RNetCDF::var.put.nc(nc.file,atl.varname[v],atl.var.ls[[v]])
  }
  
  RNetCDF::att.put.nc(nc.file, "t", "units", "NC_CHAR", 'seconds since 1964-01-01 00:00:00 UTC')
  RNetCDF::att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'NEUS_Atlantis_Obs_Hindcast')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
  RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  RNetCDF::var.put.nc(nc.file, "t", t_tot)
  
  
  RNetCDF::close.nc(nc.file)
  
}
