# caclulate the diagnostic criteria by guild from the baseline run over several key time periods

#Define param files and atlantis outputs

base.run.dir = base.run.dir = 'Z:/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/'
biomind <- paste0(base.run.dir,'neus_outputBiomIndx.txt')
catchfile <- paste0(base.run.dir,'neus_outputCatch.txt')
fgs <- paste0(here::here(),"/currentVersion/neus_groups.csv")
guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
fgs.guild.file = here::here('diagnostics','functional_group_guild.csv')

#Get reference biomass
realBiomass <- readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units) |> 
  dplyr::mutate(variable = 'biomass')

#aggreate original by guild assignments

biomass = read.table(biomind, header=TRUE) |> 
  dplyr::select(!contains("Rel"))

catch = read.table(catchfile, header=TRUE) |> 
  select(!contains("TsAct"))

#Define time assignments (years 35-55, 57-77)
time.end = 28104
time.slices = data.frame(
  yr.start = c(36,57),
  yr.end = c(56,77)
) |> 
  mutate(day.start = yr.start*365,
         day.end = yr.end*365,
         nyr = yr.end-yr.start)



#Function to create guild-versions of output and param files for each time slice to "trick" the atlantisdiagnostic function
get_diag_guild = function(day.start, day.end){
  
  nyr = ceiling((day.end - day.start)/365)
  
  #define temp files
  bio.temp.name = here::here('diagnostics','guild_biomass_temp.txt')
  bio.reasonable.temp.name = here::here('diagnostics','guild_biomass_reasonable_temp.txt')
  catch.temp.name = here::here('diagnostics','guild_catch_temp.txt')
  
  #group by guild and write with same format as biomind
  new.biomass = biomass |> 
    dplyr::filter(Time <= day.end) |> 
    dplyr::rename(time = 'Time')
  
  new.biomass.reasonable =  biomass |> 
    dplyr::filter(Time <= day.end) |> 
    dplyr::rename(time = 'Time')
  
  
  write.table(new.biomass, bio.temp.name,row.names = F)
  write.table(new.biomass.reasonable, bio.reasonable.temp.name,row.names = F)
  
  #group by guild and write the same format as catch
  new.catch = catch |> 
    dplyr::filter(Time <= day.end) 

  write.table(new.catch, catch.temp.name,row.names =F)
  
  #Run atlantisdiagnostics
  stabilty.guild = atlantisdiagnostics::diag_stability(fgs = fgs,
                                                       biomind = bio.temp.name,
                                                       speciesCodes = NULL,
                                                       nYr = nyr,
                                                       relChangeThreshold = 0.05) |> 
    dplyr::rename(Code = 'code',
                  Stable = 'pass') |> 
    dplyr::select(Code, relChange, Stable)
  
  reasonable.guild = atlantisdiagnostics::diag_reasonability(fgs = fgs,
                                                             startYr = floor(day.start/365),
                                                             nYrs = nyr,
                                                             biomind = bio.reasonable.temp.name,                                                   speciesCodes= NULL,
                                                             realBiomass = realBiomass
  ) |> 
    dplyr::select(code, minBiomass, maxBiomass, pass) |> 
    dplyr::rename(Code = 'code',
                  Reasonable = 'pass')
  
  persist.guild = atlantisdiagnostics::diag_persistence(fgs = fgs.guild.file,
                                                        biomind = bio.temp.name
  ) |> 
    dplyr::select(code, pass) |>
    dplyr::rename(Code = 'code',
                  Persist = 'pass')
  
  mean.bio = new.biomass |> 
    dplyr::filter(time >= day.start & time <=day.end) |> 
    tidyr::pivot_longer(cols = -time, names_to = 'Code', values_to = 'Biomass') |> 
    dplyr::group_by(Code) |> 
    dplyr::summarise(mean.bio = mean(Biomass,na.rm=T))
  
  mean.catch = new.catch |> 
    dplyr::filter(Time >= day.start & Time <=day.end) |> 
    tidyr::pivot_longer(cols = -Time, names_to = 'Code', values_to = 'Catch') |> 
    dplyr::group_by(Code) |> 
    dplyr::summarise(mean.catch = mean(Catch,na.rm=T))
  
  data.out = mean.bio |> 
    dplyr::left_join(mean.catch) |> 
    dplyr::left_join(stabilty.guild) |> 
    dplyr::left_join(reasonable.guild) |> 
    dplyr::left_join(persist.guild) |> 
    dplyr::mutate(day.start = day.start,
                  day.end = day.end,
                  mean.catch = ifelse(is.na(mean.catch),0,mean.catch),
                  f.rate = mean.catch/mean.bio,
                  year.start = round(day.start/365,0),
                  year.end = round(day.end/365,0)
    ) |> 
    dplyr::select(Code, day.start, year.start, day.end, year.end, Persist, Stable, Reasonable,dplyr::everything())
  
  return(data.out)
}

#Run iterations
data.ls = lapply(1:nrow(time.slices), function(i){
  get_diag_guild(day.start = time.slices$day.start[i],
                 day.end = time.slices$day.end[i])
})

#export
run.diag.stats = dplyr::bind_rows(data.ls)
write.csv(run.diag.stats,paste0(base.run.dir,'Post_Processed/Data/guild_diagnostic_stats.csv'),row.names = F)




