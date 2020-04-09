#'@title creates ltl forcing files from ROMS_COBALT data
#'
#'Creates ltl forcing files similarly structured to temp/salt
#'files made by hydroconstruct. Assumes structure 
#'of files made by ROMS_to_hydroconstruct script. 
#'Works on one year at a time.
#'
#'Author: Hem Nalini Morzaria Luna, modified by J.Caracappa
#'
# 
roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/statevars/'
# roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
roms.file = paste0(roms.dir,'roms_output_statevars_tohydro_1965.nc')

make.tempsalt.force = function(roms.dir,roms.file){

  `%>%` = dplyr::`%>%`
  
  source(here::here('R','make_hydrofile_functions.R'))
  source(here::here('R','ROMS_aggregated_convert_longform.R'))
  
  bgm.polygons = 0:29
  
  file.year = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",roms.file)))

  # general
  fill.value <- 0
  this.geometry <- "neus_tmerc_RM2.bgm"
  this.title <- "ROMS"
  
  #options depth dimension
  depth.bins <- 1:5
  d.units <- "depth layers"
  
  #options time dimension
  timestep <- 24 # 12 hour timesteps
  t.units <- "seconds since 1964-01-01 00:00:00 +10"
  time.unit.length <- 1 # years
  time.length <- 365
  seconds.timestep <- 60*60*24
  time.vector <- 1:time.length
  
  #options polygon dimension
  pol.units <- "spatial polygons"
  pol.bins <- 1:30
  
  # time.array <- make_time_array(time.vector)
  
  
  pol.array <- pol.bins %>% 
    as.array
  
  depth.array <- depth.bins %>% 
    as.array

  ltl.nc = ncdf4::nc_open(roms.file)
  time.steps = ltl.nc$dim$time$vals
  dat.ls =  roms2long(roms.file,is.hflux = F)
  
  avg.data = dat.ls[[1]]
  for(i in 2:length(dat.ls)){
    avg.data = cbind(avg.data,dat.ls[[i]][,4])
    colnames(avg.data)[i+3] = colnames(dat.ls[[i]])[4]
  }
  avg.data$month = as.numeric(format(avg.data$time,format= '%m'))
  avg.data$year = as.numeric(format(avg.data$time,format = '%Y'))
  # avg.data$timesteps = as.numeric(avg.data$time - as.POSIXct('1964-01-01 00:00:00',tz= 'UTC'))
  avg.data$timesteps = difftime(avg.data$time,as.Date('1964-01-01 00:00:00 UTC'),units = 'sec')
  avg.data = avg.data %>% 
    dplyr::select(year,month,time,timesteps,box,level,salinity,temperature) %>% 
    dplyr::arrange(year,month,time,box,level)
  
  avg.data = na.omit(avg.data)
  
  time.array = sort(unique(avg.data$timesteps))
  
  time.steps <- avg.data %>% 
    dplyr::distinct(timesteps) %>% 
    dplyr::pull(timesteps)
  
  these.years <- avg.data %>% 
    dplyr::distinct(year) %>% 
    dplyr::pull(year)
  

  get_array <- function(eachvariable) {
    
    print(eachvariable)
    
    variable.list <- list()
    
    for(eachindex in 1:length(time.steps))  {
      
      print(eachindex)
      this.index.data <- avg.data %>% 
        dplyr::filter(timesteps==time.steps[eachindex])
      
      pol.list <- list()
      
      for(eachpolygon in bgm.polygons) {
        
        this.value <- this.index.data %>% 
          dplyr::filter(box == eachpolygon) %>% 
          dplyr::arrange(desc(level)) %>% 
          dplyr::select(all_of(eachvariable)) %>% 
          dplyr::pull(1)
        
        if(isEmpty(this.value)){
          
          length(this.value) <- 5
          
        } else if(!isEmpty(this.value)){
          
          length(this.value) <- 5
          
          # this.value[5] <- this.value[1]
          
        }
        
        pol.array <- this.value %>% 
          as.array()
        
        pol.list[[eachpolygon+1]] <- pol.array
        
      }
      
      this.index <- do.call("cbind", pol.list) %>% as.array()
      
      variable.list[[eachindex]] <- this.index
      
    }
    
    return(variable.list)
    
  }
  
  salt.list.array = get_array('salinity')
  temp.list.array = get_array('temperature')
  
  salt.result.array = array(dim = c(5,30,length(salt.list.array)))
  temp.result.array = array(dim = c(5,30,length(temp.list.array)))
                                              
  for(eachindex in 1:length(salt.list.array)){
    
    salt.result.array[,,eachindex] = do.call('cbind', salt.list.array[eachindex])
    temp.result.array[,,eachindex] = do.call('cbind', temp.list.array[eachindex])

  }
  
  
  make_hydro <- function(varname,var.array, nc.name, t.units, seconds.timestep, this.title, this.geometry, time.array, cdf.name) {
    
    nc.file <- RNetCDF::create.nc(nc.name)
    
    RNetCDF::dim.def.nc(nc.file, "t", unlim=TRUE)
    RNetCDF::dim.def.nc(nc.file, "b", 30)
    RNetCDF::dim.def.nc(nc.file, "z", 5)
    
    RNetCDF::var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
    RNetCDF::var.def.nc(nc.file, varname, 'NC_DOUBLE', c('z','b','t'))

    RNetCDF::att.put.nc(nc.file, varname,'_FillValue','NC_DOUBLE',0)

    RNetCDF::att.put.nc(nc.file, "t", "units", "NC_CHAR", t.units)
    RNetCDF::att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", seconds.timestep)
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", this.title)
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", this.geometry)
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
    
    RNetCDF::var.put.nc(nc.file, "t", time.array)
    RNetCDF::var.put.nc(nc.file,varname,var.array)

    RNetCDF::close.nc(nc.file)
    
    system(paste("ncdump ",nc.name," > ", cdf.name,sep=""), wait = TRUE)
    
    
  }
  
  make_hydro(varname = 'salinity',var.array = salt.result.array,nc.name=paste0(roms.dir,"salt_force_",file.year,".nc"), t.units, seconds.timestep, this.title, this.geometry, time.array, cdf.name = paste0(roms.dir,"salt_force_",file.year,".nc"))
  make_hydro(varname = 'temperature',var.array = temp.result.array,nc.name=paste0(roms.dir,"temp_force_",file.year,".nc"), t.units, seconds.timestep, this.title, this.geometry, time.array, cdf.name = paste0(roms.dir,"temp_force_",file.year,".nc"))
  
}

# make.ltl.force(
#   roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/',
#   # roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
#   roms.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/roms_output_ltl_statevars_tohydro_1964.nc'
#   
# )



