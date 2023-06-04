#'@title creates ltl forcing files from ROMS_COBALT data
#'
#'Creates ltl forcing files similarly structured to temp/salt
#'files made by hydroconstruct. Assumes structure 
#'of files made by ROMS_to_hydroconstruct script. 
#'Works on one year at a time.
#'
#'Author: Hem Nalini Morzaria Luna, modified by J.Caracappa
#'


# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/phys_statevars/'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/'
# roms.file = paste0(roms.dir,'roms_cobalt_v10_statevars_1981_neus_atl.nc')
# force.vars = c('temperature','salinity')
# var.units = c('deg C','psu')

make_force_statevar = function(roms.dir,roms.file,out.dir,force.vars,final.vars,var.units,fill.val,miss.val,valid.min,valid.max,long.names,out.prefix,dupe.bottom){

  library(dplyr)
  library(RNetCDF)
  source(here::here('R','alternate_force_functions.R'))
  source(here::here('R','ROMS_COBALT','flatten_ROMS.R'))
  
  bgm.polygons = 0:29
  
  file.year = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",roms.file)))
  if(is.na(file.year)){
    file.year = ''
  }
  
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

  roms.nc = ncdf4::nc_open(roms.file)
  time.steps = roms.nc$dim$time$vals
  dat.ls =  flatten_ROMS(roms.file,is.hflux = F)
  
  avg.data = dat.ls[[1]]
  for(i in 2:length(dat.ls)){
    avg.data = cbind(avg.data,dat.ls[[i]][,4])
    colnames(avg.data)[i+3] = colnames(dat.ls[[i]])[4]
  }
  avg.data$month = as.numeric(format(avg.data$time,format= '%m'))
  avg.data$year = as.numeric(format(avg.data$time,format = '%Y'))
  # avg.data$timesteps = as.numeric(avg.data$time - as.POSIXct('1964-01-01 00:00:00',tz= 'UTC'))
  avg.data$timesteps = difftime(avg.data$time,as.Date('1964-01-01 00:00:00 UTC'),units = 'sec')
  avg.data = avg.data %>% select(year,month,time,timesteps,box,level,force.vars) %>% arrange(year,month,time,box,level)
  
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
          
          #Below copies bottom layer into sediment layer
          if(dupe.bottom){
            this.value[5] <- this.value[1]  
          }
          
          
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
  
  var.list = list()
  var.result.list = list()
  
  for(v in 1:length(force.vars)){
    var.list[[v]] = get_array(force.vars[v])
    var.result.list[[v]] = array(dim=c(5,30,length(var.list[[v]])))
    for(eachindex in 1:length(var.list[[v]])){
      var.result.list[[v]][,,eachindex] = do.call('cbind',var.list[[v]][eachindex])
    }
    
  }
                                        
  # ndi.list.array <- get_array("ndi")
  # ndi.result.array <- array(dim=c(5,30,length(ndi.list.array)))
  # for(eachindex in 1:length(ndi.list.array)){
  #   ndi.result.array[,,eachindex] <- do.call("cbind", ndi.list.array[eachindex])
  # }
  
  
  make_hydro <- function(nc.name, t.units, seconds.timestep, this.title, this.geometry, time.array, cdf.name,force.vars, final.vars,var.units) {
    
    nc.file <- create.nc(nc.name)
    
    dim.def.nc(nc.file, "t", unlim=TRUE)
    dim.def.nc(nc.file, "b", 30)
    dim.def.nc(nc.file, "z", 5)
    
    var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
    for(v in 1:length(force.vars)){
      var.name = force.vars[v]
      #Define Variables
      var.def.nc(nc.file, final.vars[v], 'NC_DOUBLE', c('z','b','t'))
      #Assign Fill Value
      att.put.nc(nc.file, final.vars[v], '_FillValue', "NC_DOUBLE", fill.val[v])
      #Assign 
      att.put.nc(nc.file, final.vars[v], 'missing_value', 'NC_DOUBLE',miss.val[v])
      #Assign valid_min
      att.put.nc(nc.file, final.vars[v], 'valid_min', 'NC_DOUBLE', valid.min[v])
      #Assing valid_max
      att.put.nc(nc.file, final.vars[v], 'valid_max', 'NC_DOUBLE', valid.max[v])
      #Assign units
      att.put.nc(nc.file, final.vars[v], 'units','NC_CHAR', var.units[v])
      #Assign long_name
      att.put.nc(nc.file,final.vars[v],'long_name','NC_CHAR',long.names[v])
      
      #Put variable values
      var.put.nc(nc.file,final.vars[v],var.result.list[[v]])
    }
    
    att.put.nc(nc.file, "t", "units", "NC_CHAR", t.units)
    att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", seconds.timestep)
    att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", this.title)
    att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", this.geometry)
    att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
    
    var.put.nc(nc.file, "t", time.array)
    

    close.nc(nc.file)
    
    # system(paste("ncdump ",nc.name," > ", cdf.name,sep=""), wait = TRUE)
    
    
  }
  make_hydro(nc.name=paste0(out.dir,out.prefix,file.year,".nc"),
             t.units, seconds.timestep,
             this.title, this.geometry,
             time.array,
             cdf.name = paste0(out.dir,"test_roms_",file.year,".cdf"),
             force.vars = force.vars,
             final.vars = final.vars,
             var.units = var.units)
  
  
}

# make.ltl.force(
#   roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/',
#   # roms.files <- list.files(path=roms.dir, pattern="^roms_output_ltl_statevars_tohydro_.*\\.nc$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
#   roms.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/roms_output_ltl_statevars_tohydro_1964.nc'
#   
# )



