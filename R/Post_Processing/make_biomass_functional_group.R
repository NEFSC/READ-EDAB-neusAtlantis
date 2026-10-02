#Script to aggregate biomass summaries over functional groups

make_biomass_functional_group = function(run.name, data.dir,agg.level){
  ##Functional group index
  
  new.fgs = read.csv(here::here('Diagnostics','functional_groups_match.csv')) %>%
    select(Code,AtlantisGroup,FuncGroup)
  
  ##Biomass by age by EPU
  bio.age.epu = read.csv(paste0(data.dir,'/',run.name,'_epu_age_biomass.csv'))
  
  if(agg.level == 'month'){
    bio.epu.newfgs = bio.age.epu %>%
      left_join(new.fgs,by = c('spp' = 'Code')) %>%
      group_by(year,month,epu,FuncGroup) %>%
      summarise(time = max(time),value = sum(value,na.rm=T))
  }else if(agg.level == 'year'){
    bio.epu.newfgs = bio.age.epu %>%
      left_join(new.fgs,by = c('spp' = 'Code')) %>%
      group_by(year,epu,FuncGroup) %>%
      summarise(time = max(time),value = sum(value,na.rm=T))
  }else{
    bio.epu.newfgs = bio.age.epu %>%
      left_join(new.fgs,by = c('spp' = 'Code')) %>%
      group_by(time,epu,FuncGroup) %>%
      summarise(value = sum(value,na.rm=T))
  }
  
  write.csv(bio.epu.newfgs, file = paste0(data.dir,'/',run.name,'_epu_newfgs_biomass.csv'),row.names = F)
  
  standardized.file = paste0(data.dir,'/',run.name,'_epu_age_biomass_concentration.csv')
  if(file.exists(standardized.file)){
    
    bio.age.epu = read.csv(standardized.file)
    
    if(agg.level == 'month'){
      bio.epu.newfgs = bio.age.epu %>%
        left_join(new.fgs,by = c('spp' = 'Code')) %>%
        group_by(year,month,epu,FuncGroup) %>%
        summarise(time = max(time),value = sum(value.wgt,na.rm=T))
    }else if(agg.level == 'year'){
      bio.epu.newfgs = bio.age.epu %>%
        left_join(new.fgs,by = c('spp' = 'Code')) %>%
        group_by(year,epu,FuncGroup) %>%
        summarise(time = max(time),value = sum(value.wgt,na.rm=T))
    }else{
      bio.epu.newfgs = bio.age.epu %>%
        left_join(new.fgs,by = c('spp' = 'Code')) %>%
        group_by(time,epu,FuncGroup) %>%
        summarise(value = sum(value.wgt,na.rm=T))
    }
    
    write.csv(bio.epu.newfgs, file = paste0(data.dir,'/',run.name,'_epu_newfgs_biomass_standardized.csv'),row.names = F)
  }
  # bio.epu.vertpos = bio.age.epu %>%
  #   left_join(new.fgs,by = c('spp' = 'Code')) %>%
  #   group_by(time,epu,VertPosition) %>%
  #   summarise(value = sum(value,na.rm=T))
  # 
  # write.csv(bio.epu.vertpos, file = paste0(data.dir,'/',run.name,'_epu_vert_pos_biomass.csv'),row.names = F)
  # 
}
