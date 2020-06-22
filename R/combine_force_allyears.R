### For Forcing Files

# transport.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/transport/'
# phys.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars_alternate/'
# nut.dir ='C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/nut_statevars/'
# ltl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/ltl_statevars/'
# force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'
force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/bias_corrected_tempsalt/'

# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/combined_years/'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/combined_years_repo/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/combined_debias_temp/'

# file.dir = phys.dir
# var.name = 'Temp'
# file.pattern = '*.nc'

combine_years = function(file.dir,file.pattern,var.name,out.dir){
  
  file.names = list.files(file.dir,file.pattern)  
  data.ls = list()
  time.ls = list()
  for(f in 1:length(file.names)){
    
    file.nc = ncdf4::nc_open(paste0(file.dir,file.names[f]))
    data.ls[[f]] = ncdf4::ncvar_get(file.nc,var.name)
    time.ls[[f]] = file.nc$dim$t$vals
    
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
# combine_years(transport.dir,file.pattern = '*.nc','hlfux',out.dir)

# #Physics Statevariables
# combine_years(phys.dir,file.pattern = '*.nc','Temp',out.dir)
# combine_years(phys.dir,file.pattern = '*.nc','salt',out.dir)
# # combine_years(phys.dir,file.pattern = '*.nc','verticalflux',out.dir)
# 
# #LTL statevariables
# combine_years(ltl.dir,file.pattern = '*.nc','ndi',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','Diatom_N',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','Carniv_Zoo_N',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','Zoo_N',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','PicoPhytopl_N',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','MicroZoo_N',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','Diatom_S',out.dir)
# combine_years(ltl.dir,file.pattern = '*.nc','Pelag_Bact_N',out.dir)
# 
# #Nutrient statevariables
# combine_years(nut.dir,file.pattern = '*.nc','NH3',out.dir)
# combine_years(nut.dir,file.pattern = '*.nc','NO3',out.dir)
# combine_years(nut.dir,file.pattern = '*.nc','Oxygen',out.dir)
# combine_years(nut.dir,file.pattern = '*.nc','Si',out.dir)

#Debias Temperature
# combine_years(force.dir,file.pattern = 'roms_tempsalt*','temperature',out.dir )

#Physics Statevariables
combine_years(force.dir,file.pattern = 'roms_tempsalt*','temperature',out.dir)
combine_years(force.dir,file.pattern = 'roms_tempsalt*','salinity',out.dir)
# combine_years(phys.dir,file.pattern = 'roms_tempsalt*','verticalflux',out.dir)

#LTL statevariables
# combine_years(force.dir,file.pattern = 'roms_ltl*','ndi',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','Diatom_N',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','Carniv_Zoo_N',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','Zoo_N',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','PicoPhytopl_N',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','MicroZoo_N',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','Diatom_S',out.dir)
combine_years(force.dir,file.pattern = 'roms_ltl*','Pelag_Bact_N',out.dir)

#Nutrient statevariables
combine_years(force.dir,file.pattern = 'roms_nut*','NH3',out.dir)
combine_years(force.dir,file.pattern = 'roms_nut*','NO3',out.dir)
combine_years(force.dir,file.pattern = 'roms_nut*','Oxygen',out.dir)
combine_years(force.dir,file.pattern = 'roms_nut*','Si',out.dir)