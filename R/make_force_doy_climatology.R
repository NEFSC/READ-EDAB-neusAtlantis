# Script to create a DOY climatology for each forced variable in Atlantis
# These will be based on the combined longform data table made by "make_longform_allvars_ts.R"

library(ggplot2)
library(dplyr)
library(ncdf4)
library(RNetCDF)

#Load full dataset
load('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/All_Forcing_Variables_Longform.R')

#filter based on true start date of variables (PL,DF,PS,and DL on 1998-01-01, temp,salt,vflux on 1993-01-01)
force.vars.df$date = as.Date(force.vars.df$date)
force.vars.df = force.vars.df %>%
  filter( (variable %in% c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S','Lab_Det_N') & date >= as.Date('1998-01-01')) | (variable %in% c('temperature','salinity','verticalflux') & date >= as.Date('1993-01-01')))

#test min and max dates by variable
# force.vars.df %>%
#   group_by(variable) %>%
#   summarize(min.date = min(date),
#             max.date = max(date))

#determine DOY 
force.vars.df$jul = format(force.vars.df$date, format = '%j')
force.vars.df$doy = as.numeric(force.vars.df$jul) 

#Aggregate by DOY
force.vars.doy = force.vars.df %>%
  group_by(variable,units,box,level,doy) %>%
  summarize(values = mean(values,na.rm=T))
rm(force.vars.df)

#Add variable group column (which goes into separate netCDF files)
force.vars.doy$var.group = sapply(force.vars.doy$variable,function(x){
  if(x %in% c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')){
    return('LTL')
  }else if(x %in% c('Lab_Det_N')){
    return('Det')
  }else if(x %in% c('temperature','salinity','verticalflux')){
    return('Physics')
  }
})

#Test by plotting
# var.names = unique(force.vars.doy$variable)
# for(i in 1:length(var.names)){
#   test.df = force.vars.doy %>% filter(variable == var.names[i] & box == 1 & level == 1 ) 
#   plot(values~doy,test.df,type='l',main = var.names[i])}
# 
# x = force.vars.df %>% filter(variable == 'PicoPhytopl_N' & box == 1 & level ==1 & date > as.Date('1999-01-01'))
# plot(values~date,x,type='l')

#Split data into list by variable group
force.vars.ls = lapply(unique(force.vars.doy$var.group),function(x) return(filter(force.vars.doy, var.group == x)))
rm(force.vars.doy)

#Loop through force.vars.ls and create an netCDF file for each
boxes = 0:29

for(g in 1:length(force.vars.ls)){
  
  dat = force.vars.ls[[g]]
  var.names = unique(dat$variable)
  
  for(v in 1:length(var.names)){
    
    #set up data array
    dat.var = filter(dat,variable == var.names[v])
    var.dat.array = array(NA,dim = c(5,30,366))
    
    #loop through boxes then levels (different vars have different number of levels)
    for(b in 1:length(boxes)){
      dat.var.box = filter(dat.var,box == boxes[b])
      test = reshape2::dcast(dat.var.box,level~doy,value.var = 'values')[,-1]
      levels = unique(dat.var.box$level)
      # for(l in 1:length(levels)){
      #   dat.var.box.lev = filter(dat.var.box, level == levels[l])
      #   var.dat.array[l,b,] = dat.var.box.lev$values
      # }
    }
    
    
    
    
    
    
    
    # for(b in 1:length(boxes)){
    #   var.dat.box = filter(dat.var,box == boxes[b])
    #   test  = reshape2::dcast(var.dat.box,level~doy,value.var = 'values')[,-1]
    # }
    
    
    
  }
  
}