# Combine all yearly .R output files (ROMS aggregated) into one file.

transport.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/transport/'
phys.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/phys_statevars/'
nut.dir ='C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/nut_statevars/'
ltl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'

out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/combined_years/'

# years = 1981:2014

file.dir = transport.dir
var.name = 'transport'
file.pattern = '*.nc'

combine_years = function(file.dir,file.pattern,var.name,out.dir){
  
  file.names = list.files(file.dir,file.pattern)  
  data.ls = list()
  time.ls = list()
  for(f in 1:length(file.names)){
    
    file.nc = ncdf4::nc_open(paste0(file.dir,file.names[f]))
    data.ls[[f]] = ncdf4::ncvar_get(file.nc,var.name)
    time.ls[[f]] = file.nc$dim$time$vals
    
    # load(paste0(file.dir,file.names[f]))
    # data = get(var.name);rm(list = var.name)
    # data.ls[[f]] = data
    ncdf4::nc_close(file.nc)
  }
  full.data = abind::abind(data.ls,along =3)
  full.time = unlist(time.ls)
  
  save(full.data,full.time, file = paste0(out.dir,var.name,'_allyears.R'))
} 

#Transport
combine_years(transport.dir,file.pattern = '*.nc','transport',out.dir)

#Physics Statevariables
combine_years(phys.dir,file.pattern = '*.nc','temperature',out.dir)
combine_years(phys.dir,file.pattern = '*.nc','salinity',out.dir)
combine_years(phys.dir,file.pattern = '*.nc','verticalflux',out.dir)

#LTL statevariables
combine_years(ltl.dir,file.pattern = '*.nc','ndi',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','nlg',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','nlgz',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','nmdz',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','nsm',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','nsmz',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','silg',out.dir)
combine_years(ltl.dir,file.pattern = '*.nc','nbact',out.dir)

#Nutrient statevariables
combine_years(nut.dir,file.pattern = '*.nc','no3',out.dir)
combine_years(nut.dir,file.pattern = '*.nc','nh4',out.dir)
combine_years(nut.dir,file.pattern = '*.nc','o2',out.dir)
combine_years(nut.dir,file.pattern = '*.nc','sio4',out.dir)
