#' Function to pull specific variable data from entire satellite OCCCI dataset and format as one long dataframe
#' 
#' @satphyto.dir string. Directory containing annual OCCCI output
#' @years numeric vector of all years to be concatenated
#' @varname string. Variable name to be pulled
#' @file.prefix string. Prefix for OCCCI output
#' @out.dir string. Directory for output file
#' 
#'

# satphyto.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
# years = 1997:2019
# file.prefix = 'D8-OCCCI-ATLANTIS_'
# varname = 'PPD'
# out.dir = satphyto.dir

get_SatPhyto_var_allyears = function(satphyto.dir,
                                     years,
                                     file.prefix,
                                     varname,
                                     out.dir){
  
  #Script to pull and summarize euphotic depth from OCCCI data
  library(dplyr)
  library(ggplot2)
  
  source(here::here('R','fill_satphyto_gaps.R'))
  
  var.year.ls = list()
  
  #Loop over years, fill gaps, and output to list
  for(y in 1:length(years)){
    year.dates = seq.Date(as.Date(paste0(years[y],'-01-01')),as.Date(paste0(years[y],'-12-31')),by = 1)
    date.index = data.frame(mid = rep(year.dates,each = 30),doy = rep(1:length(year.dates),each = 30), SUBAREA = rep(0:29,length(year.dates)))
    # date.index = data.frame(mid = year.dates,doy = 1:length(year.dates))
    
    dat = read.csv(paste0(satphyto.dir,file.prefix,years[y],'.csv'),header = T,as.is = T) %>% 
      filter(PROD == varname) %>%
      select(mid,mid.year,SUBAREA,PROD,MED) %>%
      mutate(mid = as.Date(mid)) %>%
      right_join(date.index)
    colnames(dat) = c('date','ref.year','box','variable','values','doy')
    
    var.year.ls[[y]] = dat
    # dat2 = reshape2::dcast(dat, mid~SUBAREA,value.var = 'MED')
  }
  
  var.year = bind_rows(var.year.ls)
  
  
  var.year = fill_satphyto_gaps(input.mat = var.year,
                                var.name = varname,
                                doy.file = NA,
                                max.interp = 100,
                                write.gaps = F)
  
  var.year = var.year %>%
    filter(date >= as.Date('1997-09-07') & date <= as.Date('2019-12-27'))
  
  save(var.year, file = paste0(out.dir,varname,'_allyears.R'))
  
}
