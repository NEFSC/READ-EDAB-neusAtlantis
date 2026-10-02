#Script to fix the ltl variable units
library(RNetCDF)
roms.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/CurrentVersion/tsfiles/Annual_Files/'

all.files =  list.files(roms.dir,pattern = '^GLORYS_tempsalt_force_.*\\.nc$',full.names = T)
# ltl.files = ltl.files[17]
for(i in 1:length(ltl.files)){
# for(i in 17){

  nc.file = open.nc(all.files[i],write = T)
  
  #_FillValue
  att.put.nc(nc.file, 'temperature', "_FillValue", "NC_DOUBLE", -999)
  att.put.nc(nc.file, 'salinity', "_FillValue", "NC_DOUBLE", -999)
  # att.put.nc(nc.file, 'ndi', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Diatom_N', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Carniv_Zoo_N', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Zoo_N', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'PicoPhytopl_N', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'MicroZoo_N', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Diatom_S', "_FillValue", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Pelag_Bact_N', "_FillValue", "NC_DOUBLE", 0.2)
  # 
  # #missing_value
  att.put.nc(nc.file, 'temperature', "missing_value", "NC_DOUBLE", -999)
  att.put.nc(nc.file, 'salinity', "missing_value", "NC_DOUBLE", -999)
  # att.put.nc(nc.file, 'ndi', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Diatom_N', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Carniv_Zoo_N', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Zoo_N', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'PicoPhytopl_N', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'MicroZoo_N', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Diatom_S', "missing_value", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Pelag_Bact_N', "missing_value", "NC_DOUBLE", 0.2)

  #valid_min
  att.put.nc(nc.file, 'temperature', "valid_min", "NC_DOUBLE", 0)
  att.put.nc(nc.file, 'salinity', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'ndi', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Diatom_N', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Carniv_Zoo_N', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Zoo_N', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'PicoPhytopl_N', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'MicroZoo_N', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Diatom_S', "valid_min", "NC_DOUBLE", 0)
  # att.put.nc(nc.file, 'Pelag_Bact_N', "valid_min", "NC_DOUBLE", 0)

  #valid_max
  att.put.nc(nc.file, 'temperature', "valid_max", "NC_DOUBLE", 999)
  att.put.nc(nc.file, 'salinity', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'ndi', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'Diatom_N', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'Carniv_Zoo_N', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'Zoo_N', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'PicoPhytopl_N', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'MicroZoo_N', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'Diatom_S', "valid_max", "NC_DOUBLE", 999)
  # att.put.nc(nc.file, 'Pelag_Bact_N', "valid_max", "NC_DOUBLE", 999)

  #units
  # att.put.nc(nc.file, 'ndi', "units", "NC_CHAR", "mg N m-3")
  # att.put.nc(nc.file, 'Diatom_N', "units", "NC_CHAR", "mg N m-3")
  # att.put.nc(nc.file, 'Carniv_Zoo_N', "units", "NC_CHAR", "mg N m-3")
  # att.put.nc(nc.file, 'Zoo_N', "units", "NC_CHAR", "mg N m-3")
  # att.put.nc(nc.file, 'PicoPhytopl_N', "units", "NC_CHAR", "mg N m-3")
  # att.put.nc(nc.file, 'MicroZoo_N', "units", "NC_CHAR", "mg N m-3")
  # att.put.nc(nc.file, 'Diatom_S', "units", "NC_CHAR", "mg Si m-3")
  # att.put.nc(nc.file, 'Pelag_Bact_N', "units", "NC_CHAR", "mg N m-3")


  close.nc(nc.file)
  print(i)
}
# 
# 
# nut.files =  list.files(roms.dir,pattern = '^roms_nut_force.*\\.nc$',full.names = T)
# # ltl.files = ltl.files[17]
# for(i in 1:length(nut.files)){
# # for(i in 17){
#   
#   nc.file = open.nc(nut.files[i],write = T)
# 
#   
#   #valid_max
#   att.put.nc(nc.file, 'NH3', "valid_max", "NC_DOUBLE", 99999)
#   att.put.nc(nc.file, 'NO3', "valid_max", "NC_DOUBLE", 99999)
#   att.put.nc(nc.file, 'Oxygen', "valid_max", "NC_DOUBLE", 99999)
#   att.put.nc(nc.file, 'Si', "valid_max", "NC_DOUBLE", 99999)
# 
#   
#   close.nc(nc.file)
#   print(i)
# }
