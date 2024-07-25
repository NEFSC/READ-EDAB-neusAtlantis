#Script to create a center of mass plot across multiple runs to ease spatial distribution comparisons
library(ggplot2)
library(dplyr)
library(atlantisprocessing)

#Define run set
experiment.id = 'ddepend_1'
setup.df = read.csv(here::here('Setup_Files','cloud_v6681_ddepend_1_setup.csv'),as.is=T)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

run.names = paste0(experiment.id,'_',sort(unique(setup.df$Run.ID)))
run.dirs = paste0(here::here('Atlantis_Runs',experiment.id),'/',experiment.id,'_',setup.df$Run.ID)

ref.years = c(45,55)*365

#Setup BGM
box.stats = rbgm::bgmfile(here::here('Geometry','neus_ll_WGS84.bgm'))$boxes
boxes.pts = atlantistools::convert_bgm(here::here('Geometry','neus_tmerc_RM2.bgm')) %>% 
  dplyr::mutate(polygon = as.factor(polygon))


do.process = T
use.txt.output =T
experiment.com.ls = list()
i=1
for(i in 1:nrow(setup.df)){
  
  run.name = paste0(experiment.id,'_',setup.df$Run.ID[i])
  if(use.txt.output == T){
    biomass.box = read.table(here::here('Atlantis_Runs',experiment.id,run.name,'neus_outputBoxBiomass.txt'),header =T)%>%
      filter(Time>=ref.years[1] & Time <=ref.years[2])%>%
      tidyr::gather('Code','Biomass',-Time,-Box)%>%
      group_by(Code,Box)%>%
      summarise(Biomass = mean(Biomass,na.rm=T))
  }else{
    if(do.process){
      atl.dir = here::here('Atlantis_Runs',experiment.id,run.name,'')
      param.dir = here::here('currentVersion','/')
      run.prefix = 'neus_output'
      param.ls = get_atl_paramfiles(param.dir,atl.dir,run.prefix = run.prefix,include_catch = T)
      
      process_atl_output(
        param.dir = param.dir,
        atl.dir= atl.dir,
        out.dir = file.path(atl.dir, "/Post_Processed/Data/"),
        run.prefix = run.prefix,
        param.ls = param.ls,
        agg.scale= 'year',
        large.file = F,
        system = 'linux',
        process.all = F,
        plot.all = F,
        plot.benthic = F,
        plot.overall.biomass = F,
        plot.biomass.timeseries = F,
        plot.length.age = F,
        plot.biomass.box = T,
        plot.c.mum = F,
        plot.sn.rn = F,
        plot.recruits = F,
        plot.numbers.timeseries = F,
        plot.physics = F,
        plot.growth.cons = F,
        plot.cohort = F,
        plot.diet = F,
        plot.consumption = F,
        plot.spatial.biomass = F,
        plot.spatial.biomass.seasonal = F,
        plot.catch = F,
        plot.mortality = F,
        plot.weight = F,
        plot.spatial.overlap = F
      )
    }
    
    biomass.box = readRDS(paste0(atl.dir,'Post_Processed/Data/biomass_box.rds'))%>%
      left_join(select(fgs,Code,LongName),by = c('species' = 'LongName'))%>%
      filter(time >= ref.years[1]/365 & time <= ref.years[2]/365)%>%
      group_by(Code,polygon)%>%
      summarise(Biomass = mean(atoutput,na.rm=T))%>%
      rename(Box = 'polygon')
  }
   run.com =  data.frame(
     run.ID = setup.df$Run.ID[i],
     Code = fgs$Code,
     com.x = NA,
     com.y = NA  
   )
   j=1
   for(j in 1:nrow(fgs)){
     if(use.txt.output==T){
       biomass.box.spp = biomass.box %>%
         filter(Code == fgs$Code[j])  
     }else{
       biomass.box.spp = biomass.box %>%
         filter(Code == fgs$Code[j]) 
       
       which.box.missing = box.stats$.bx0[!(box.stats$.bx0 %in% biomass.box.spp$Box)]
       
       biomass.box.missing = data.frame(
         Code = fgs$Code[j],
         Box = which.box.missing,
         Biomass = 0)
         
         biomass.box.spp = biomass.box.spp %>%
           bind_rows(biomass.box.missing)%>%
           arrange(Box)
     }
     
     run.com$com.x[j] = sum(biomass.box.spp$Biomass*box.stats$insideX,na.rm=T)/sum(biomass.box.spp$Biomass,na.rm=T)
     run.com$com.y[j] = sum(biomass.box.spp$Biomass*box.stats$insideY,na.rm=T)/sum(biomass.box.spp$Biomass,na.rm=T)  
     
   }
  
   experiment.com.ls[[i]] = run.com
}

experiment.com = bind_rows(experiment.com.ls)
saveRDS(experiment.com,here::here('Atlantis_Runs',experiment.id,paste0(experiment.id,'_com.rds')))

spp.names = arrange(fgs,LongName)
s=1
pdf(here::here('Figures',paste0(experiment.id,'_center_of_mass.pdf')))
for(s in 1:nrow(spp.names)){
  experiment.com.spp = experiment.com %>%
    filter(Code == spp.names$Code[s])
  
  p = ggplot(data = boxes, aes(x = long, y = lat, group = polygon))+
    geom_polygon(fill = 'white',color = 'black')+
    annotate('text',x=experiment.com.spp$com.x, y = experiment.com.spp$com.y, label = experiment.com.spp$run.ID, color = 'black')+
    ggtitle(spp.names$LongName[s])
  
  gridExtra::grid.arrange(p)
}
dev.off()

