#' @title  Make initial condition hydrofiles
#' @description Uses ROMS model output files and creates nc files for temperature and salinity
#' @author Hem Nalini Morzaria Luna @email hmorzarialuna@gmail.com, modified by J.Caracappa
#' @date January 2020


isEmpty <- function(x) {
  return(identical(x, numeric(0)))
}

#create array for time

time.list <- function(eachtimestep){
  
  this.step <- eachtimestep * seconds.timestep
  return(this.step)
  
}

make_time_array <- function(time.vector) {
  
  time.array <- lapply(time.vector,time.list) %>% 
  unlist %>% 
  as.array
  
  return(time.array)
  
}




#clean input files and convert to dataframe

clean_datafiles <- function(this.file){
  
  this.month <- this.file %>% 
    unlist %>% 
    str_split("/") %>% 
    unlist %>% 
    str_split("-") %>% 
    .[[3]] %>% 
    .[2]
  
  this.year <- this.file %>% 
    unlist %>% 
    str_split("/") %>% 
    unlist %>% 
    str_split("-") %>% 
    .[[3]] %>% 
    .[1]
  
  this.data <- read_delim(this.file,delim="    ") %>% 
    setNames(c("time_step","polygon","depth_layer","vertical_velocity","temperature","salinity")) %>% 
    mutate(month = as.numeric(this.month),
           year = as.numeric(this.year)) 
  
  return(this.data)
}





get_array <- function(eachvariable) {
  
  print(eachvariable)
  
  variable.list <- list()
  
  for(eachindex in index.list)  {
    
    print(eachindex)
    this.index.data <- avg.data.index %>% 
      filter(index==eachindex)
    
    pol.list <- list()
    
    for(eachpolygon in bgm.polygons) {
      
      this.value <- this.index.data %>% 
        filter(polygon == eachpolygon) %>% 
        arrange(desc(depth_layer)) %>% 
        select(all_of(eachvariable)) %>% 
        pull(1)
      
      if(isEmpty(this.value)){
        
        length(this.value) <- 7
        
      } else if(!isEmpty(this.value)){
        
        length(this.value) <- 7
        
        this.value[[7]] <- this.value[[1]]
        
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




# create and write the netCDF file -- ncdf4 version
# define dimensions

make_hydro <- function(eachvariable, nc.name, t.units, seconds.timestep, this.title, this.geometry, time.array, this.array, cdf.name) {
  
  nc.file <- create.nc(nc.name)
  
  dim.def.nc(nc.file, "t", unlim=TRUE)
  dim.def.nc(nc.file, "b", 89)
  dim.def.nc(nc.file, "z", 7)
  
  var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
  var.def.nc(nc.file, eachvariable, "NC_DOUBLE", c("z","b","t"))
  
  att.put.nc(nc.file, eachvariable, "_FillValue", "NC_DOUBLE", 0)
  att.put.nc(nc.file, "t", "units", "NC_CHAR", t.units)
  att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", seconds.timestep)
  att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", this.title)
  att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", this.geometry)
  att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc.file, "t", time.array)
  var.put.nc(nc.file, eachvariable, this.array)
  close.nc(nc.file)
  
  system(paste("ncdump ",nc.name," > ", cdf.name,sep=""), wait = TRUE)
  
  
}
