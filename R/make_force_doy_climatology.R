# Script to create a DOY climatology for each forced variable in Atlantis
# These will be based on the combined longform data table made by "make_longform_allvars_ts.R"

library(ggplot2)
library(dplyr)
library(ncdf4)
library(RNetCDF)

#Load full dataset
data.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/'
load(paste0(data.dir,'All_Forcing_Variables_Longform.R'))

#filter based on true start date of variables (PL,DF,PS,and DL on 1998-01-01, temp,salt,vflux on 1993-01-01)
force.vars.df$date = as.Date(force.vars.df$date)
force.vars.df = force.vars.df %>%
  filter( (variable %in% c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S','Lab_Det_N') & date >= as.Date('1998-01-01')) | (variable %in% c('temperature','salinity','verticalflux') & date >= as.Date('1993-01-01')))

#Define netCDF params
fill.val.ls = list( rep(-999,4),
                    -999,
                    c(15,0,-999))
valid.min.ls = list( rep(-999,4),
                     -999,
                     c(-2,0,-999))
valid.max.ls = list( rep(99999,4),
                     99999,
                     c(999,999,999))
longname.ls = list(c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S'),
                   'Lab_Det_N',
                   c('salinity','temperature','verticalflux')
                   )


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
# plot(values~doy,test.df,type='l',main = var.names[i])
#   }

# x = force.vars.df %>% filter(variable == 'PicoPhytopl_N' & box == 1 & level ==1 & date > as.Date('1999-01-01'))
# plot(values~date,x,type='l')

#Split data into list by variable group
force.vars.ls = lapply(unique(force.vars.doy$var.group),function(x) return(filter(force.vars.doy, var.group == x)))
rm(force.vars.doy)

#Loop through force.vars.ls and create an netCDF file for each
boxes = 0:29

#Define "dummy year" so that there are dates assigned (must be leap year due to 366 length DOY)
ref.year = 1964
date.seq = seq.Date(as.Date(paste0(ref.year,'-01-01')),as.Date(paste0(ref.year,'-12-31')),by = 'day')
time.vals = as.numeric(difftime(date.seq,as.POSIXct('1964-01-01 00:00:00',tz = 'UTC'),tz = 'UTC',units = 'secs'))

for(g in 1:length(force.vars.ls)){
  
  dat = force.vars.ls[[g]]
  var.names = unique(dat$variable)
  
  var.array.ls = list()
  var.units = character()
  for(v in 1:length(var.names)){
    
    #set up data array
    dat.var = filter(dat,variable == var.names[v])
    var.units[v] = dat.var$units[1]
    var.dat.array = array(NA,dim = c(5,30,366))
    
    #loop through boxes then levels (different vars have different number of levels)
    for(b in 1:length(boxes)){
      dat.var.box = filter(dat.var,box == boxes[b])
      test = reshape2::dcast(dat.var.box,level~doy,value.var = 'values')[,-1]
      nlev = length(unique(dat.var.box$level))
      for(l in 1:nlev){
        var.dat.array[l,b,] = as.numeric(test[l,])
      }
    }
    
    var.array.ls[[v]] = var.dat.array
  }
  
  #Write as netCDF (based on make_statevar_alternate)
  nc.name = paste0(data.dir,unique(dat$var.group),'_DOY_Climatology.nc')
  nc.file <- create.nc(nc.name)
  
  dim.def.nc(nc.file, "t", unlim=TRUE)
  dim.def.nc(nc.file, "b", 30)
  dim.def.nc(nc.file, "z", 5)
  
  var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
  
  for(v in 1:length(var.names)){
    var.name = var.names[v]
    #Define Variables
    var.def.nc(nc.file, var.names[v], 'NC_DOUBLE', c('z','b','t'))
    #Assign Fill Value
    att.put.nc(nc.file, var.names[v], '_FillValue', "NC_DOUBLE", fill.val.ls[[g]][v])
    #Assign 
    att.put.nc(nc.file, var.names[v], 'missing_value', 'NC_DOUBLE', fill.val.ls[[g]][v])
    #Assign valid_min
    att.put.nc(nc.file, var.names[v], 'valid_min', 'NC_DOUBLE', valid.min.ls[[g]][v])
    #Assing valid_max
    att.put.nc(nc.file, var.names[v], 'valid_max', 'NC_DOUBLE', valid.max.ls[[g]][v])
    #Assign units
    att.put.nc(nc.file, var.names[v], 'units','NC_CHAR', var.units[v])
    #Assign long_name
    att.put.nc(nc.file,var.names[v],'long_name','NC_CHAR',longname.ls[[g]][v])
    
    #Put variable values
    var.put.nc(nc.file,var.names[v],var.array.ls[[v]])
  }
  
  att.put.nc(nc.file, "t", "units", "NC_CHAR", "seconds since 1964-01-01 00:00:00 +10")
  att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
  att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'Observations_Hindcast')
  att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR",  "neus_tmerc_RM2.bgm")
  att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc.file, "t", time.vals)
  
  
  close.nc(nc.file)
  
}