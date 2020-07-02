#Alter atlantis temperature 
library(ncdf4)
source(here::here('R','edit_force_var.R'))

years = 1964:2014
force.dir = 'C:/users/joseph.caracappa/Documents/GitHub/neus-atlantis2/currentVersion/tsfiles/Annual_Files/'
force.files = paste0('roms_tempsalt_force_',years,'.nc')
new.files.p2 = paste0('roms_tempsalt_force_hightemp_',years,'.nc')
new.files.m2 = paste0('roms_tempsalt_force_lowtemp_',years,'.nc')
new.files.lower.p2 = paste0('roms_tempsalt_force_lower_hightemp_',years,'.nc')
new.files.lower.m2 = paste0('roms_tempsalt_force_lower_lowtemp_',years,'.nc')

#Get layer mapping index (NA for layers <50m and 1 for >50m)
master.nc = nc_open(paste0(force.dir,force.files[1]))
master.temp = (ncvar_get(master.nc,'temperature')[,,1]*0)+1
box.deepest = apply(master.temp,2,function(x) {
  x2 = x[-5]
  cumm.x = cumsum(x2)
  if(all(is.na(x2))){
    return(rep(0,5))
  }else{
    deepest = which.max(cumm.x) 
    new.x = c(rep(0,4),1)
    new.x[deepest] =1
    return(new.x)
  }
})

yr=1
for(yr in 1:length(years)){
  
  # ndays = length(seq.Date(as.Date(paste0(years[yr],'-01-01')),as.Date(paste0(years[yr],'-12-31')),by = 'day'))
  force.nc = nc_open(paste0(force.dir,force.files[yr]))
  ndays = length(force.nc$di$t$vals)
  nc_close(force.nc)
  # edit.val.p2 = array(2,dim=c(5,30,ndays))
  # edit.val.m2 = array(-2,dim=c(5,30,ndays))
  
  #Shift temp for just layers >50m
  
  edit.val.lower.p2 = edit.val.lower.m2 = array(NA,dim = c(5,30,ndays))
  for(i in 1:ndays){
      edit.val.lower.p2[,,i] = box.deepest *2
      edit.val.lower.m2[,,i] = box.deepest * -2
  }
  
  #lower plus 2 run
  edit_force_var(force.dir = force.dir,
                 force.file = force.files[yr],
                 var.name = 'temperature',
                 edit.array = edit.val.lower.p2,
                 edit.method = 'add',
                 new.file = new.files.lower.p2[yr],
                 overwrite = F
  )
  
  #lower minus 2 run
  edit_force_var(force.dir = force.dir,
                 force.file = force.files[yr],
                 var.name = 'temperature',
                 edit.array = edit.val.lower.m2,
                 edit.method = 'add',
                 new.file = new.files.lower.m2[yr],
                 overwrite = F
  )
  
  #plus 2 run
  # edit_force_var(force.dir = force.dir,
  #                force.file = force.files[yr],
  #                var.name = 'temperature',
  #                edit.array = edit.val.p2,
  #                edit.method = 'add',
  #                new.file = new.files.p2[yr],
  #                overwrite = F
  #                )
  
  #minus 2 run
  # edit_force_var(force.dir = force.dir,
  #                force.file = force.files[yr],
  #                var.name = 'temperature',
  #                edit.array = edit.val.m2,
  #                edit.method = 'add',
  #                new.file = new.files.m2[yr],
  #                overwrite = F
  # )
  
}

#Make paths
temp.lines.p2 = paste0('Temperature',(1:length(years))-1,'.name \\tsfiles\\Annual_Files\\roms_tempsalt_force_hightemp_',years,'.nc')
temp.lines.m2 = paste0('Temperature',(1:length(years))-1,'.name \\tsfiles\\Annual_Files\\roms_tempsalt_force_lowtemp_',years,'.nc')
temp.lines.lower.p2 = paste0('Temperature',(1:length(years))-1,'.name \\tsfiles\\Annual_Files\\roms_tempsalt_force_lower_hightemp_',years,'.nc')
temp.lines.lower.m2 = paste0('Temperature',(1:length(years))-1,'.name \\tsfiles\\Annual_Files\\roms_tempsalt_force_lower_lowtemp_',years,'.nc')


write.csv(data.frame(high_temp = temp.lines.p2,low_temp = temp.lines.m2,lower_high_temp = temp.lines.lower.p2, lower_low_temp = temp.lines.lower.m2),file = paste0(force.dir,'temp_sensitivity_paths.csv'))
          