#Function to combine fishing sensitivity output

make_fishing_sensitivity_combined_output = function(batch.dir,
                                                    batch.prefix,
                                                    out.dir,
                                                    run.index.file
                                                    ){
  scenario.combs = readRDS(run.index.file)
  
  bio.ls = catch.ls = bio.age.ls = list()
  i=1
  for(i in 1:nrow(scenario.combs)){
    
    tryCatch(
      {
        run.name = paste0(batch.prefix,'_',scenario.combs$guild.names[i],'_',scenario.combs$fishing.levels.text[i])  
        
        bio.ls[[i]] = read.table(paste0(batch.dir,run.name,'/neus_outputBiomIndx.txt'),as.is =T,header = T)%>%
          select(Time:DC)%>%
          tidyr::gather(Code,Biomass,-Time)%>%
          mutate(run.name = run.name,
                 guild.name = scenario.combs$guild.names[i],
                 fishing.scalar = scenario.combs$fishing.levels.scalar[i])%>%
          select(run.name,guild.name,fishing.scalar,Code,Time,Biomass)
        
        catch.ls[[i]] = read.table(paste0(batch.dir,run.name,'/neus_outputCatch.txt'),as.is =T,header = T)%>%
          select(Time:ZG)%>%
          tidyr::gather(Code,Catch,-Time)%>%
          mutate(run.name = run.name,
                 guild.name = scenario.combs$guild.names[i],
                 fishing.scalar = scenario.combs$fishing.levels.scalar[i])%>%
          select(run.name,guild.name,fishing.scalar,Code,Time,Catch)
        
        bio.age.ls[[i]] = read.table(paste0(batch.dir,run.name,'/neus_outputAgeBiomIndx.txt'),as.is = T,header =T)%>%
          tidyr::gather(ID,Biomass,-Time)%>%
          tidyr::separate(ID,c('Code','agecl'))%>%
          mutate(run.name = run.name,
                 guild.name = scenario.combs$guild.names[i],
                 fishing.scalar = scenario.combs$fishing.levels.scalar[i])
      }, error = function(e){}
    )
    
    print(i)
  }
  
  bio.ls = bind_rows(bio.ls)
  catch.ls = bind_rows(catch.ls)
  bio.age.ls = bind_rows(bio.age.ls)
  
  saveRDS(bio.ls,paste0(out.dir,'biomass_all.rds'))
  saveRDS(bio.age.ls,paste0(out.dir,'biomass_age_all.rds'))
  saveRDS(catch.ls,paste0(out.dir,'catch_all.rds'))
  
}

# make_fishing_sensitivity_combined_output(batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
#                                          batch.prefix = 'fishing_sensitivity_extended_constant_2',
#                                          out.dir = here::here('data','fishing_sensitivity_extended_constant_2',''), #output directory
#                                          run.index.file = paste0(out.dir,'run_index.rds'))
