### Convenience script to help produce dummy SDM netCDF forcing files for Atlantis ###
### Owen R. Liu; April 2023 ###

### OUTLINE ###

### Input file should be a .csv with species code (Atlantis 3-letter code), stage (0 or 1 indicating juvenile or adult, respectively),
### year, season (1 to however many distributions per year are designated in the groups.csv, usually 1, 2, 4, or 12)b
### Atlantis polygon/box #, and proportion. Proportion should be equal to the 
### PROPORTION OF THE SPECIES DISTRIBUTION IN BOX XX IN YEAR YY
### If box-specific values are given in other units (e.g., biomass, density, probability of occurrence), this 
### script will convert them to proportions by dividing each value by the sum across boxes (per year/season).
### Then, the script linearly interpolates to create a full distribution time series from the earliest to 
### the latest year given in the csv. Finally, the script packs the output into an Atlantis-readble netCDF.


###

# library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ncdf4)
library(RNetCDF)
library(lubridate)

#Concert dist CSV from wide to long
orig.file = '/net/work3/EDAB/atlantis/Shared_Data/Spatial_Distributions/cod_dist_test_wide.csv'
orig.dist = read.csv(orig.file,header = T) %>%
  select(code:'box.29')%>%
  tidyr::gather('box2','proportion',-code,-season,-year,-stage)%>%
  tidyr::separate(box2,c('dum','box'))%>%
  select(-dum)
  
write.csv(orig.dist,'/net/work3/EDAB/atlantis/Shared_Data/Spatial_Distributions/cod_dist_test_long.csv',row.names = F)

# PUT PATH TO INPUT CSV HERE
fp_csv <- '/net/work3/EDAB/atlantis/Shared_Data/Spatial_Distributions/cod_dist_test_long.csv'

# Function to organize SDM input

calc_full_sdm <- function(fp){
  
  # Import csv. Expected format is 4 columns :code (XXX Atlantis species code), year, box#, proportion.
  dat <- read_csv(fp)
  
  # Fill years and make sure everything is converted to proportions that add to 1
  out <- dat %>% 
    # fill in zeroes for NAs if there are any in the "proportion" column
    mutate(proportion=replace_na(proportion,0)) %>% 
    
    # expand years to full time series
    complete(code,box,stage,season,year=full_seq(year,1)) %>% 
    
    # linearly interpolate between years, from given values
    group_by(code,stage,season,box) %>% 
    arrange(box,year) %>% 
    mutate(intval=approx(year,proportion,year)$y) %>% 
    
    # normalize proportions to make sure that they add to 1 each year
    ungroup() %>% 
    group_by(code,stage,season,year) %>%
    mutate(normval=intval/sum(intval)) %>% 
    
    # clean up output
    ungroup() %>% 
    dplyr::select(code,stage,year,season,box,normval)
  
  return(out)

}

### Function to take an SDM dataframe for one species/stage from the above function (or similar dataframe) and 
### produce an Atlantis-readable SDM forcing file.

make_single_ncdf <- function(sdm_df,spinup_yrs,save.dir="",this_geom=""){
  
  # get unique species code and stage
  varname <- unique(sdm_df$code_stage)
  nbox=length(unique(sdm_df$box))
  # file name and descriptive title for the ncdf
  nc_name <- paste0(save.dir,varname,'.nc')
  this_title <- paste("Interpolated SDM for",unique(sdm_df$code),"stage",unique(sdm_df$stage))
  
  # Defining the time-range for which we will formulate the distributions
  # use lubridate for precise dates and times
  minyr <- min(sdm_df$year)
  maxyr <- max(sdm_df$year)+spinup_yrs  
  yrs <- minyr:maxyr
  
  # key matching number of seasons to correct months (e.g., 2 seasons = Jan and Jul, 4 seasons= Jan, Apr, Jul,Oct, etc.)
  season_mths <- tibble(season=unique(sdm_df$season)) %>% 
    mutate(month=seq(1,12,by=12/length(unique(season))))

  # Organize time steps for the ncdf
  time_seq <- crossing(year=yrs,season=unique(sdm_df$season),day="01") %>% 
    left_join(season_mths,by="season") %>% 
    unite(date,year,month,day,sep="-",remove=F) %>% 
    # convert dates to a real date object
    mutate(date=ymd(date)) %>% 
    # convert to seconds since first date
    mutate(seconds=date[1]%--%date %>% seconds() %>% as.numeric()) %>% 
    dplyr::select(date,year,season,seconds)
  
  t_units <- paste('seconds since',time_seq$date[1],"00:00:00")
  
  # make data array from sdm dataframe
  # first year of distribution data (for looping for XX spinup years)
  firstyr <- sdm_df %>%
    dplyr::select(box,year,season,normval) %>% 
    mutate(year=as.integer(year)) %>% 
    filter(year==minyr)
  
  # manually copy first year for `spinup_yrs` years
  loopyrs <- purrr::map_df((minyr+1):(minyr+spinup_yrs-1),function(yr)firstyr %>% mutate(year=yr))
  
  # all data except the looped years
  tsyrs <- sdm_df %>% 
    dplyr::select(box,year,season,normval) %>% 
    mutate(year=as.integer(year)+spinup_yrs) %>% 
    filter(year>minyr)
  
  # attach first year, any spin up years, then the actual SDM timeseries
  ts_full <- firstyr %>%
    bind_rows(loopyrs) %>% 
    bind_rows(tsyrs) %>% 
    arrange(year,season,box,normval)
  
  # Create the box by time array to pack into the netCDF
  sppdf <- time_seq %>% 
    mutate(box=0) %>% 
    complete(nesting(year,season,date,seconds),box=unique(sdm_df$box)) %>% 
    left_join(ts_full,by = c("year","season", "box"))
  
  data_array <- array(sppdf$normval,dim=c(length(unique(sppdf$box)),length(unique(sppdf$seconds))))
  
  # construct the nc file
  nc_file <- create.nc(nc_name)
  
  # if(stage=="juvenile") {varname <- paste0(sppname,"_stage_0")} else{varname<-paste0(sppname,"_stage_1")}
  
  dim.def.nc(nc_file, "t", unlim=TRUE)
  dim.def.nc(nc_file, "b", nbox) # manual
  
  var.def.nc(nc_file, "t", "NC_DOUBLE", "t")
  var.def.nc(nc_file,"t1","NC_INT","t")
  var.def.nc(nc_file, varname, "NC_DOUBLE", c("b","t"))
  
  att.put.nc(nc_file, varname, "_FillValue", "NC_DOUBLE", 0)
  att.put.nc(nc_file, "t", "units", "NC_CHAR", t_units)
  att.put.nc(nc_file, "t", "dt", "NC_DOUBLE", 86400)
  att.put.nc(nc_file,"t1","long_name","NC_CHAR","t1")
  att.put.nc(nc_file, "NC_GLOBAL", "title", "NC_CHAR", this_title)
  att.put.nc(nc_file, "NC_GLOBAL", "geometry", "NC_CHAR", this_geom)
  att.put.nc(nc_file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc_file, "t", time_seq$seconds)
  var.put.nc(nc_file,"t1",1:nrow(time_seq))
  var.put.nc(nc_file, varname, data_array)
  close.nc(nc_file)
  
  message(paste(nc_name,"created."))
}

### ONE-FUNCTION NCDF CREATOR
### PARAMETERS
### fp: file path for input .csv. Expected format is 6 columns: code (XXX Atlantis species code),stage (0 or 1), season, year, box#, proportion.
### spinup_yrs: number of extra looped years to append on the front end of the time series
### save.dir: save directory for saving output ncdfs; Default is the current working directory
### this_geom: optional character string; name of Atlantis bgm box geometry file, just to keep track in ncdf metadata

### Option to have a 'spin-up' period, where the first year's distribution is repeated for xx spin-up years

make_sdm_ncdfs <- function(fp,save.dir="",spinup_yrs=0,this_geom=""){
  
  # use the functions defined above to create the sdm dataframe
  combined_sdm_df <- calc_full_sdm(fp)
  
  # split input dataframe by species and stage
  sdm_ls <- combined_sdm_df %>% 
    mutate(code_stage=paste0(code,"_stage_",stage)) %>% 
    group_split(code_stage)
  
  # apply to all list members, producing one SDM for each code/stage combination in the original df
  purrr::walk(sdm_ls,make_single_ncdf,save.dir=save.dir,this_geom=this_geom,spinup_yrs=spinup_yrs)
}


## TEST these functions ##
x <- calc_full_sdm(fp_csv)
glimpse(x)

# write a sample set of .ncdfs based on our input .csv, with 30 spinup years
make_sdm_ncdfs(fp=fp_csv,save.dir = here::here('Setup_Files','eccwo test'),
               spinup_yrs = 50,
               this_geom = here::here('currentVersion','neus_tmerc_RM2.bgm'))

# check
test = nc_open(here::here('Setup_Files','eccwo testCOD_stage_1.nc'))
tidync::tidync(here::here('Setup_Files','eccwo testCOD_stage_1.nc'))

names(test$var)

cod1 = ncvar_get(test,'COD_stage_1')
View(cod1)
### END ###