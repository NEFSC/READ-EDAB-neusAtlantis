#Function to extract box level biomass and aggregate over EPUS

make_biomass_EPU  = function(run.dir,run.name,epu.file,groups.file,bgm.file,out.dir,out.name){
  
  `%>%` = dplyr::`%>%`
  
  #read output
  run.nc = ncdf4::nc_open(paste0(run.dir,'neus_output.nc'))
  
  #read in box to epu index
  box2epu = read.csv(epu.file,stringsAsFactors = F,na.strings = 'NA')
  
  #read in box biomass data
  # bio.box = read.table(paste0(run.dir,'neus_outputBoxBiomass.txt'),header = T,as.is = T)
  # bio = read.table(paste0(run.dir,'neus_outputBiomIndx.txt'),header = T,as.is = T)
  # 
  #read in groups.csv
  fgs = read.csv(groups.file,as.is =T)
  
  #get box volume
  box.vol = ncdf4::ncvar_get(run.nc,'volume')[1:4,,1]
  time = run.nc$dim$t$vals/86400
  
  #get box area
  bgm = rbgm::bgmfile(bgm.file)
  box.area = bgm$boxes$area
  
  #Epibenthic groups
  epi.groups = c('SED_EP_FF','SED_EP_OTHER','MOB_EP_OTHER','PHYTOBEN','MICROPHYTBENTHOS','SEAGRASS')
  
  #Loop over groups in neus_output.nc to get total biomass
  box.age.ls = list()
  for(i in 1:nrow(fgs)){
    
    if(fgs$IsTurnedOn[i] == 0){
      box.age.ls[[i]] = NULL
      next()
    }
    #If age10
    if(fgs$NumCohorts[i]==10){
      
      spp.age.ls = list()
      
      for(j in 1:10){
        SN = ncdf4::ncvar_get(run.nc,paste0(fgs$Name[i],j,'_StructN'))[1:4,,]
        RN = ncdf4::ncvar_get(run.nc,paste0(fgs$Name[i],j,'_ResN'))[1:4,,]
        nums = ncdf4::ncvar_get(run.nc,paste0(fgs$Name[i],j,'_Nums'))[1:4,,]
        
        bio.array =nums*(SN + RN)*5.7*2E-8
        bio.box.wide = apply(bio.array,c(3,2),sum) %>% as.data.frame() 
        colnames(bio.box.wide) = as.character(0:29)
        bio.box.wide$time = time
        
        bio.box.df = reshape2::melt(bio.box.wide,id.vars = 'time',variable.name = 'box',factorsAsStrings = T)
        bio.box.df$spp = fgs$Code[i]
        bio.box.df$age = j
        
        spp.age.ls[[j]] = bio.box.df
      }
      
      box.age.ls[[i]] = dplyr::bind_rows(spp.age.ls)
      
    #If age2  
    }else if(fgs$NumCohorts[i]==2){
      
      spp.age.ls = list()
      
      for(j in 1:2){
        N = ncdf4::ncvar_get(run.nc,paste0(fgs$Name[i],'_N',j))[1:4,,]
        
        for(k in 1:dim(N)[3]){
          N[,,k] = N[,,k] * box.vol
        }
        N = N * 5.7 *2E-8
        
        bio.box.wide = apply(N,c(3,2),sum) %>% as.data.frame()
        colnames(bio.box.wide) = as.character(0:29)
        bio.box.wide$time = time
        
        bio.box.df = reshape2::melt(bio.box.wide,id.vars = 'time',variable.name = 'box')
        bio.box.df$spp = fgs$Code[i]
        bio.box.df$age = j
        
        spp.age.ls[[j]] = bio.box.df
      }
      
      box.age.ls[[i]] = dplyr::bind_rows(spp.age.ls)
    
    #If age1
    }else if(fgs$NumCohorts[i] == 1){
      
      if(fgs$GroupType[i] %in% epi.groups){
        N = ncdf4::ncvar_get(run.nc,paste0(fgs$Name[i],'_N'))
        bio.box.wide = t(N * box.area * 5.7 * 2E-8) %>% as.data.frame()
        colnames(bio.box.wide) = as.character(0:29)
        bio.box.wide$time = time
        
        bio.box.df = reshape2::melt(bio.box.wide, id.vars = 'time', variable.name = 'box')
        bio.box.df$spp = fgs$Code[i]
        bio.box.df$age = 1
        
        box.age.ls[[i]] = bio.box.df
      }else{
          
        N = ncdf4::ncvar_get(run.nc,paste0(fgs$Name[i],'_N'))[1:4,,]
        for(k in 1:dim(N)[3]){
          N[,,k] = N[,,k] * box.vol
        }
        N = N * 5.7 *20*1E-9
        
        bio.box.wide = apply(N,c(3,2),sum) %>% as.data.frame()
        colnames(bio.box.wide) = as.character(0:29)
        bio.box.wide$time = time
        
        bio.box.df = reshape2::melt(bio.box.wide,id.vars = 'time',variable.name = 'box')
        bio.box.df$spp = fgs$Code[i]
        bio.box.df$age = 1
        
        box.age.ls[[i]] = bio.box.df

        }

    }else{
      next()
    }
    print(paste0(i,'-',fgs$Code[i]))
    
    if(all(is.na(box.age.ls[[i]]$spp ))){
      print('HELP')
    }
  }
  box.age.biomass = dplyr::bind_rows(box.age.ls)
  
  x = box.age.biomass %>%
    filter(spp == 'ZM'& box %in% c(8,12:15))
  ggplot(x, aes(x= time, y = value))+
    geom_line()+
    facet_wrap(~box)
  
  write.csv(box.age.biomass,paste0(out.dir,'/',out.name,'_box_age_biomass.csv'),row.names = F)
  
  #aggregate over EPU
  bio.box.epu = box.age.biomass %>%
    dplyr::mutate(box = as.numeric(as.character(box))) %>%
    dplyr::left_join(box2epu) %>%
    dplyr::group_by(time, epu, spp) %>%
    dplyr::summarize(value = sum(value,na.rm=T)) %>%
    dplyr::filter(!is.na(epu))
  
  write.csv(bio.box.epu,paste0(out.dir,'/',out.name,'_epu_biomass.csv'),row.names = F)
  
  #Aggregate box age epu
  box.age.epu = box.age.biomass %>%
    dplyr::mutate(box = as.numeric(as.character(box))) %>%
    dplyr::left_join(box2epu) %>% 
    dplyr::filter(!is.na(epu)) %>%
    dplyr::group_by(time,epu,spp,age) %>%
    dplyr::summarize(value = sum(value,na.rm=T))

  write.csv(box.age.epu,paste0(out.dir,'/',out.name,'_epu_age_biomass.csv'),row.names = F)
}

# run.name = 'BalanceHerStart'
# run.dir = paste0(getwd(),'/Atlantis_Runs/',run.name,'/')
# epu.file = here::here('Manuscript','Data','box2epu.csv')
# groups.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_groups.csv'
# bgm.file = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm'
# out.dir = here::here('Manuscript','Data')
# out.name = run.name


