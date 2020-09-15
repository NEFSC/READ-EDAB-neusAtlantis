### For Forcing Files

transport.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/transport/'
phys.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phys_statevars_alternate/'
force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'

out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/'

# file.dir = phys.dir
# var.name = 'temperature'
# file.pattern = '^temp_.*\\.nc$'

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
combine_years(transport.dir,file.pattern = '^GLORYS_Atlantis.*\\.nc','transport',out.dir)

# #Physics Statevariables
combine_years(phys.dir,file.pattern = '^GLORYS_tempsalt_force_.*\\.nc','temperature',out.dir)
combine_years(phys.dir,file.pattern = '^GLORYS_tempsalt_force_.*\\.nc','salinity',out.dir)
combine_years('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/statevars/',file.pattern = '^ECCO_vflux_Atlantis_.*\\.nc','verticalflux',out.dir)

phyto.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_dynamic_lower/'
dl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/labile_detritus/'

#New Satellite Phytoplankton
combine_years(file.dir = phyto.dir,
              file.pattern = 'Phyto_Forcing_*',
              var.name = 'Diatom_N',
              out.dir = out.dir)

combine_years(file.dir = phyto.dir,
              file.pattern = 'Phyto_Forcing_*',
              var.name = 'DinoFlag_N',
              out.dir = out.dir)

combine_years(file.dir = phyto.dir,
              file.pattern = 'Phyto_Forcing_*',
              var.name = 'PicoPhytopl_N',
              out.dir = out.dir)

combine_years(file.dir = phyto.dir,
              file.pattern = 'Phyto_Forcing_*',
              var.name = 'Diatom_S',
              out.dir = out.dir)

combine_years(file.dir = dl.dir,
              file.pattern = 'Satphyto_Forcing_DL_*',
              var.name = 'Lab_Det_N',
              out.dir = out.dir)

