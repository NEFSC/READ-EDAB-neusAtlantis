  library(dplyr)
  library(data.table)
  library(stringr)
  # NOTE: THIS FILE NOT TO BE POSTED ON GITHUB DUE TO POTENTIAL CONFIDENTIALITY CONCERNS
  # ask Sean for it
  
  load("/home/rgamble/Desktop/Atlantis-Catch/comland_meatwt_deflated_stat_areas.RData")
  # from https://github.com/NOAA-EDAB/Atlantis-Catch-Files/blob/master/Atlantis_1_5_groups_svspp_nespp3.csv
  
  spcodes <- readr::read_csv("/home/rgamble/atlantis/Atlantis_1_5_groups_svspp_nespp3.csv")
  spcodes <- filter(spcodes,!is.na(NESPP3))
  
  # Conversion factor to appropriate Atlantis units
  CONVFACTOR = 40 / 71.902080
  
  # time series in case we want to plot them
  
  # Get summary for each year for each species
  comlandts <- group_by(comland,YEAR, NESPP3)
  comlandts_sumary <- summarise(comlandts, WGT = (sum(SPPLIVMT) * CONVFACTOR)) 
  
  # Aggregate species as needed into Atlantis codes
  comlandts_atlantis <- inner_join(comlandts_sumary,spcodes, by="NESPP3")
  comlandts_atlantis_grouped <- group_by(comlandts_atlantis,YEAR, Code)
  comlandts_atlantis_summary <- summarise(comlandts_atlantis_grouped, grpWGT = (sum(WGT))) 
  
  # Set up hindcast catch data
  hindcast_catch <- filter(comlandts_atlantis_summary,YEAR >= 1964)
  timesteps <- 54 * 365
    
  # Set up catch tibble for time series file
  header <- c("MAK","HER","WHK","BLF","WPF","SUF","WIF","WTF","FOU","HAL","PLA","FLA","BFT","TUN","BIL","MPF","BUT","BPF","ANC","GOO","MEN","FDE","COD","SHK","OHK","POL","RHK","BSB","SCU","TYL","RED","OPT","SAL","DRM","STB","TAU","WOL","SDF","FDF","HAD","YTF","DOG","SMO","SSH","DSH","BLS","POR","PSH","WSK","LSK","SK","SB","PIN","REP","RWH","BWH","SWH","TWH","INV","LSQ","ISQ","SCA","QHG","CLA","BFF","BG","LOB","RCB","BMS","NSH","OSH","ZL","BD","MA","MB","SG","BC","ZG","PL","DF","MB","MMM","ZS","PB","BB","BO","DL","DR","DC")
  catch <- tibble(time = 0:timesteps)
  catch[,header]=0
  
  year <- 0
  for (sp in 1:91) {
    timestep <- 1
    spp_ts <- filter(hindcast_catch,Code == header[sp])
    if (nrow(spp_ts) > 0) {
        for (i in 1:54) {
          year <- 1963 + i
          if (length(spp_ts$grpWGT[which(spp_ts$YEAR==year)] > 0)) {
            for (day in 1:365) {
              n = sp + 1
              catch[timestep,n] <- spp_ts$grpWGT[which(spp_ts$YEAR == year)]
              timestep = timestep + 1
            }
          }
        }
    }    
  }
  
  write.table(catch,"/home/rgamble/Desktop/Atlantis-Catch/catch_ts_all_new.txt",col.names = F, row.names = F, sep = " ")
      