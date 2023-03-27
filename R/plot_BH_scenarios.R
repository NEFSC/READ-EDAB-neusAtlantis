library(dplyr)
library(ggplot2)

batch.prefix = 'BH_convert_1'

run.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/')

run.index = readRDS(here::here('data',paste0(batch.prefix,'_index.rds')))

groups = sort(unique(run.index$Code))

i=1
pdf(here::here('Figures',batch.prefix,'biomass_spp_1.pdf'))
for(i in 1:length(groups)){
  
  dir.names = filter(run.index,Code == groups[i])$name
  if(length(dir.names) == 0){next()}
  
  group.bio.ls = list()
  for(j in 1:length(dir.names)){
    bio.file = paste0(run.dir,dir.names[j],'/neus_outputBiomIndx.txt')
    if(file.exists(bio.file)&file.size(bio.file)>0){
      group.bio.ls[[j]] = read.table(bio.file,header = T) %>%
        select(Time,all_of(groups[i]))%>%
        mutate(group = dir.names[j])%>%
        rename('biomass' = groups[i])
    }else{
      next()
    }

  }
  group.bio = bind_rows(group.bio.ls)
  
  if(nrow(group.bio == 0)){
    p = ggplot(data = group.bio, aes(x = Time, y = biomass/biomass[1],color = group))+
      geom_line()+
      theme_bw()+
      ggtitle(groups[i])+
      theme(plot.title = element_text(hjust = 0.5))
    gridExtra::grid.arrange(p)
  }else{
    next()
  }
}
dev.off()