years = 2021:2100
id = 56+(1:length(years))
x = data.frame(flow = paste0('hd',id,'.name /tsfiles/Annual_Files/flow_',years,'.nc'),
               temp = paste0('Temperature',id,'.name /tsfiles/Annual_Files/CM2_6_tempsalt_force_',years,'.nc'),
               salt = paste0('Salinity',id,'.name /tsfiles/Annual_Files/CM2_6_tempsalt_force_',years,'.nc'))
write.csv(x,file = 'C:/Users/joseph.caracappa/Documents/param1.csv')

years = 2018:2100
id = 53+(1:length(years))
x = data.frame(diatom = paste0('Diatom_N_File',id,'.name /tsfiles/Annual_Files/Phyto_Forcing_',years,'.nc'),
               diatom2 = paste0('Diatom_N_File',id,'.use_resets 0'),
               picophyto = paste0('PicoPhytopl_N_File',id,'.name /tsfiles/Annual_Files/Phyto_Forcing_',years,'.nc'),
               picophyto2 = paste0('PicoPhytopl_N_File',id,'.use_resets 0'),
               diatomS = paste0('Diatom_S_File',id,'.name /tsfiles/Annual_Files/Phyto_Forcing_',years,'.nc'),
               diatomS2 = paste0('Diatom_S_File',id,'.use_resets 0'),
               dinoflag = paste0('DinoFlag_N_File',id,'.name /tsfiles/Annual_Files/Phyto_Forcing_',years,'.nc'),
               dinoflag2 = paste0('DinoFlag_N_File',id,'.use_resets 0'),
               DL = paste0('Lab_Det_N_File',id,'.name /tsfiles/Annual_Files/Satphyto_Forcing_DL_',years,'.nc'),
               DL2 = paste0('Lab_Det_N_File',id,'.use_resets 0')
               )
write.csv(x,file = 'C:/Users/joseph.caracappa/Documents/param2.csv')