#Script that calls on all scripts needed to generate new phytoplankton forcing data
#A) make satphyto_files 1998-2019
#B) make forcing files w/ make_force_statevar 1998-2019
#C) create spinup forcing 1964-1997

#Read in climatology function and statevar forcing function
source(here::here('R','Satellite_Phytoplankton','make_SatPhyto_files_byClass.R'))
source(here::here('R','Physical_Forcing','make_force_statevar.R'))
source(here::here('R','Physical_Forcing','make_force_spinup.R'))

#set.directories
satphyto.dir ='C:/Users/joseph.caracappa/Documents/Satellite_Phyto/'
rawdata.dir = paste0(satphyto.dir,'Data/v6/')
satphyto.atl.dir = paste0(satphyto.dir,'Atlantis_Format/v6/')
satphyto.force.dir = paste0(satphyto.dir,'Forcing_dynamic_lower/v6/')

#Generatates the correct diatom proportion of micro plankton
source(here::here('R','Satellite_Phytoplankton','make_satphyto_hirata_PLDF.R'))
make_satphyto_hirata_PLDF(FDiatom.file  =  'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_FDIATOM-HIRATA.CSV',
                          Micro.file =     'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_MICRO-TURNER.CSV',
                          Chl.file =     'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-CHLOR_A-CCI.CSV',
                          out.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/v6/',
                          figure.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/v6/') 
       
#Combines all forcing variables into a single data object for climatological calculations
source(here::here('R','Forcing_General','combine_force_allyears.R'))
combine_force_allyears(transport.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/transport/',
                       phys.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phys_statevars_alternate/',
                       force.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/',
                       out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/v6/')

#Creates a longform dataframe for each forcing variable
source(here::here('R','Forcing_General','make_longform_allvars_ts.R'))
make_longform_allvars_ts(data.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/v6/')

#Make a DOY climatology for all forcing variables on atlantis format
source(here::here('R','Forcing_General','make_force_DOY_climatology.R'))
make_force_DOY_climatology(data.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/v6/')

#Make Climatology Forcing files
source(here::here('R','Satellite_Phytoplankton','make_SatPhyto_climatology_byClass.R'))
source(here::here('R','Physical_Forcing','make_force_statevar.R'))

#set.directories
rawdata.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/'
processed.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/v6/'
force.dir = 'c:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Files/v6/'
atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen')

#make climatology
make_SatPhyto_climatology_byClass(in.dir = rawdata.dir,
                                  micro.file = 'DOY-OCCCI-ATLANTIS_NEUS-PSC_MICRO-TURNER.CSV',
                                  nanopico.file = 'DOY-OCCCI-ATLANTIS_NEUS-PSC_NANOPICO-TURNER.CSV',
                                  out.dir = processed.dir,
                                  out.file = 'Phyto_Climatology',
                                  stat.var = 'MED',
                                  bio.vars =  c('PSC_MICRO','PSC_NANOPICO'),
                                  atl.groups = c('PL','DF','PS'),
                                  atl.varname = atl.varname,
                                  atl.longname = atl.longname,
                                  hirata.doy = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/v6/diatom_proportion_DOY_dataframe.rds', 
                                  chl.conv = rep(7,3)
)

#make forcing
satphyto.files = list.files(processed.dir,'*Climatology.nc',full.names = T)
for(i in 1:length(satphyto.files)){
  make_force_statevar(roms.dir = processed.dir,
                      roms.file = satphyto.files[i],
                      out.dir = force.dir,
                      force.vars = atl.varname,
                      var.units = rep('mg N m-3',3),
                      final.vars = atl.varname,
                      fill.val = rep(0,3),
                      long.names = atl.longname,
                      miss.val = rep(0,3),
                      valid.min = rep(0,3),
                      valid.max = rep(99999,3),
                      dupe.bottom = F,
                      out.prefix = 'SatPhyto_Climatology')
  print(i)
}


# make satphyto files Atlantis-format -------------------------------------

atl.varname =  c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicate')

years = 1998:2021
# hirata.file = 'D8-OCCCI-ATLANTIS_NEUS-PSC_FDIATOM-HIRATA.CSV'

# Make Forcing using Hirata Diatom Proportion
# diatom.pct = read.csv(paste0(rawdata.dir,'/',hirata.file),as.is = T)%>% 
#   select(PERIOD,SUBAREA,MED)%>%
#   tidyr::separate(PERIOD,c('DURATION','DATE.START','DATE.END'),sep = '_')%>%
#   mutate(DATE = as.Date(DATE.END,'%Y%m%d'),
#          DOY = format(DATE,'%j'))%>%
#   group_by(DOY,SUBAREA)%>%
#   summarise(MED = mean(MED,na.rm=T))

diatom.pct = readRDS(paste0(rawdata.dir,'/diatom_proportion_DOY_dataframe.rds'))

diatom.pct.mat = diatom.pct %>%
  tidyr::spread(DOY,MED)%>%
  ungroup()%>%
  select(-SUBAREA)%>%
  as.matrix()
  
phyto.fract.ls = list()
for(f in 1:length(years)){
  if(years[f] %% 4 == 0){
    phyto.fract.ls[[f]] = matrix(NA,30,366)
    phyto.fract.ls[[f]][,1:365] = diatom.pct.mat
    phyto.fract.ls[[f]][,366] = diatom.pct.mat[,365]
  }else{
    phyto.fract.ls[[f]] = diatom.pct.mat[,1:365]
  }
}



make_SatPhyto_files_byClass(in.dir = rawdata.dir,
                    micro.file = 'D8-OCCCI-ATLANTIS_NEUS-PSC_MICRO-TURNER.CSV',
                    nanopico.file = 'D8-OCCCI-ATLANTIS_NEUS-PSC_NANOPICO-TURNER.CSV',
                    out.dir = satphyto.force.dir,
                    out.prefix =  'Phyto_Forcing_',
                    stat.var = 'MED',
                    bio.vars = c('MICRO','NANO','PICO'),
                    atl.varname = atl.varname,
                    atl.longname = atl.longname,
                    atl.units = c(rep('mg N m-3',3),'mg Si m-3'),
                    dynamic.mid = T,
                    dynamic.bot = T,
                    phyto.fract.ls = phyto.fract.ls,
                    chl.conv = rep(7,3),
                    years = years
)


# C) Make Spinup ----------------------------------------------------------


#copy to obs hindcast directory
obs.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/phyto_statevars_v6_DOY_spinup/'
from.files = paste0(satphyto.force.dir,'Phyto_Forcing_',1998:2021,'.nc')
file.copy(from.files,obs.dir,overwrite = T)

#Make spinup files
years = 1964:1997
for(i in 1:length(years)){
  make_force_spinup(
    do.hydroconstruct = F,
    out.dir = obs.dir,
    trans.prefix = NA,
    statevar.prefix = NA,
    anyvar.prefix = 'Phyto_Forcing_',
    transport.file = NA,
    statevar.file = NA,
    # anyvar.file = paste0(obs.dir,'Phyto_Forcing_1998.nc'),
    anyvar.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/v6/LTL_DOY_Climatology.nc',
    anyvar.out = obs.dir,
    force.dir = obs.dir,
    start.year = 1964,
    new.year = years[i],
    mid.layer = 'dynamic',
    bot.layer = 'dynamic',
    param.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/obs_hindcast_hydroconstruct_template.prm',
    bat.temp = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/hydroconstruct_run_template.bat'
    )
}

#Copy files into GitHub directory
from.files = paste0(obs.dir,'Phyto_Forcing_',1964:2021,'.nc')
git.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/'
file.copy(from.files,git.dir,overwrite = T)


