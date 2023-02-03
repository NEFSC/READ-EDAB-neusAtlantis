#'@title creates nutrient forcing files from ROMS_COBALT data
#'
#'Creates ltl forcing files similarly structured to temp/salt
#'files made by hydroconstruct. Assumes structure 
#'of files made by ROMS_to_hydroconstruct script. 
#'Works on one year at a time.
#'
#'Author: Hem Nalini Morzaria Luna, modified by J.Caracappa
#'
# 
# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/nut_statevars/'
# # # roms.files <- list.files(path=roms.dir, pattern="^roms_output_nut_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
# roms.file = paste0(roms.dir,'roms_output_nut_statevars_tohydro_1964.nc')

make_force_nutrients = function(roms.dir,roms.file){

  source(here::here('R','alternate_force_functions.R'))
  source(here::here('R','flatten_ROMS.R'))
  
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
  avg.data = avg.data %>% select(year,month,time,timesteps,box,level,nh4,no3,o2,sio4) %>% arrange(year,month,time,box,level)
  
  avg.data = na.omit(avg.data)
  
  time.array = sort(unique(avg.data$timesteps))
  
  time.steps <- avg.data %>% 
    distinct(timesteps) %>% 
    pull(timesteps)
  
  these.years <- avg.data %>% 
    distinct(year) %>% 
    pull(year)
  

  get_array <- function(eachvariable) {
    
    print(eachvariable)
    
    variable.list <- list()
    
    for(eachindex in 1:length(time.steps))  {
      
      print(eachindex)
      this.index.data <- avg.data %>% 
        filter(timesteps==time.steps[eachindex])
      
      pol.list <- list()
      
      for(eachpolygon in bgm.polygons) {
        
        this.value <- this.index.data %>% 
          filter(box == eachpolygon) %>% 
          arrange(desc(level)) %>% 
          select(all_of(eachvariable)) %>% 
          pull(1)
        
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
  
  nh4.list.array = get_array('nh4')
  no3.list.array = get_array('no3')
  o2.list.array = get_array('o2')
  sio4.list.array = get_array('sio4')
  
  nh4.result.array <- array(dim = c(5,30,length(nh4.list.array)))
  no3.result.array <- array(dim = c(5,30,length(no3.list.array)))
  o2.result.array <- array(dim = c(5,30,length(o2.list.array)))
  sio4.result.array <- array(dim = c(5,30,length(sio4.list.array)))
  
  for(eachindex in 1:length(nh4.list.array)){
    
    nh4.result.array[,,eachindex] <- do.call("cbind", nh4.list.array[eachindex])
    no3.result.array[,,eachindex] <- do.call("cbind", no3.list.array[eachindex])
    o2.result.array[,,eachindex] <- do.call("cbind", o2.list.array[eachindex])
    sio4.result.array[,,eachindex] <- do.call("cbind", sio4.list.array[eachindex])
  }
  
  
  make_hydro <- function(nc.name, t.units, seconds.timestep, this.title, this.geometry, time.array, cdf.name) {
    
    nc.file <- create.nc(nc.name)
    
    dim.def.nc(nc.file, "t", unlim=TRUE)
    dim.def.nc(nc.file, "b", 30)
    dim.def.nc(nc.file, "z", 5)
    
    var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
    var.def.nc(nc.file, 'NH3', 'NC_DOUBLE', c('z','b','t'))
    var.def.nc(nc.file,'NO3', 'NC_DOUBLE', c('z','b','t'))
    var.def.nc(nc.file,'Oxygen', 'NC_DOUBLE', c('z','b','t'))
    var.def.nc(nc.file,'Si', 'NC_DOUBLE', c('z','b','t'))
    
    #_FillValue
    att.put.nc(nc.file, 'NH3', '_FillValue', "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'NO3', '_FillValue', "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'Oxygen', '_FillValue', "NC_DOUBLE",8000)
    att.put.nc(nc.file, 'Si', '_FillValue', "NC_DOUBLE",0)
    
    #missing_value
    att.put.nc(nc.file, 'NH3', 'missing_value', "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'NO3', 'missing_value', "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'Oxygen', 'missing_value', "NC_DOUBLE",8000)
    att.put.nc(nc.file, 'Si', 'missing_value', "NC_DOUBLE",0)

    #valid_min
    att.put.nc(nc.file, 'NH3', "valid_min", "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'NO3', "valid_min", "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'Oxygen', "valid_min", "NC_DOUBLE", 0)
    att.put.nc(nc.file, 'Si', "valid_min", "NC_DOUBLE", 0)

    #valid_max
    att.put.nc(nc.file,'NH3', "valid_max", "NC_DOUBLE", 99999)
    att.put.nc(nc.file,'NO3', "valid_max", "NC_DOUBLE", 99999)
    att.put.nc(nc.file,'Oxygen', "valid_max", "NC_DOUBLE", 99999)
    att.put.nc(nc.file,'Si', "valid_max", "NC_DOUBLE", 99999)

    #units
    att.put.nc(nc.file,'NH3','units','NC_CHAR','mg N m-3')
    att.put.nc(nc.file,'NO3','units','NC_CHAR','mg N m-3')
    att.put.nc(nc.file,'Oxygen','units','NC_CHAR','mg O2 m-3')
    att.put.nc(nc.file,'Si','units','NC_CHAR','mg Si m-3')

    att.put.nc(nc.file, "t", "units", "NC_CHAR", t.units)
    att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", seconds.timestep)
    att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", this.title)
    att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", this.geometry)
    att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
    
    var.put.nc(nc.file, "t", time.array)
    var.put.nc(nc.file,'NH3',nh4.result.array)
    var.put.nc(nc.file,'NO3',no3.result.array)
    var.put.nc(nc.file,'Oxygen',o2.result.array)
    var.put.nc(nc.file,'Si',sio4.result.array)

    close.nc(nc.file)
    
    system(paste("ncdump ",nc.name," > ", cdf.name,sep=""), wait = TRUE)
    
    
  }
  make_hydro(nc.name=paste0(roms.dir,"roms_nut_force_",file.year,".nc"), t.units, seconds.timestep, this.title, this.geometry, time.array, cdf.name = paste0(roms.dir,"test_roms_",file.year,".cdf"))
  
  
}

# make.ltl.force(
#   roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/',
#   # roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
#   roms.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/roms_output_ltl_statevars_tohydro_1964.nc'
#   
# )



