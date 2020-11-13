#Script to plot 2 or more species biomass relative to initial conditions

plot_species_comp_rel = function(atl.dir,spp.name, bio.log = F, bio.rel = T){
  
  library(ggplot2)
  library(dplyr)
  
  bio.dat = read.table(paste0(atl.dir,'neus_outputBiomIndx.txt'),header= T, stringsAsFactors = F)
  spp.ls = list()

  
  for(spp in 1:length(spp.name)){
    bio.spp = bio.dat %>% select(Time,all_of(spp.name[spp])) 
    bio = bio.spp[,2]
    if(bio.log){
      bio = log(bio,10)
    }
    if(bio.rel){
      bio = bio/bio[1]
    }
    
    spp.ls[[spp]] = data.frame(Time = as.numeric(bio.spp$Time)/365,
                               Group = as.character(spp.name[spp]),
                               Biomass = bio,
                               stringsAsFactors = F)
  }
  
  plot.df = bind_rows(spp.ls)
  
  ggplot(plot.df,aes(x= Time, y = Biomass, col = Group))+
    geom_line(size = 1.2)+
    theme_bw()
}

plot_species_comp_rel(atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ReducePred13/',
                      spp.name = c('WOL'),
                      bio.log =T,
                      bio.rel = T)
  