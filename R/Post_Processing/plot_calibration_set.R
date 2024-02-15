#' Script to create plot comparisons between individual runs of the same Run.Group
#' Also produces mean biomass of target species
library(dplyr)
library(ggplot2)
#Specify setup.csv used to generate calibration run set
experiment.id = 'cloud_jcc_6681_1'
setup.df = read.csv(here::here('Setup_Files','cloud_jcc_6681_1.csv'),as.is=T)
setup.df$file.ID = 1:nrow(setup.df)
# experiment.dir = here::here('Atlantis_Runs',experiment.id,'')
experiment.dir = paste0('/contrib/Joseph.Caracappa/neus-atlantis/Atlantis_Runs/',experiment.id,'/')
# figure.dir = here::here('Figures',experiment.id,'')
figure.dir = paste0('/contrib/Joseph.Caracappa/neus-atlantis/Figures/',experiment.id,'/')

if(!dir.exists(figure.dir)){dir.create(figure.dir)}



#Read in groups file
# fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>%
fgs = read.csv('/contrib/Joseph.Caracappa/neus-atlantis/currentVersion/neus_groups.csv',as.is = T)%>%
  filter(IsTurnedOn == T)

plot.colors = c(RColorBrewer::brewer.pal(12,'Paired'),
                RColorBrewer::brewer.pal(8,'Dark2'),
                RColorBrewer::brewer.pal(12,'Set3'))

run.groups = sort(unique(setup.df$Run.Group))
i=6

for(i in 1:length(run.groups)){
  
  setup.group = setup.df %>%
    filter(Run.Group == run.groups[i])
  j=1
  bio.run.group = list()
  for(j in 1:nrow(setup.group)){
    biom.file = paste0(experiment.dir,experiment.id,'_',setup.group$Run.ID[j],'/neus_outputBiomIndx.txt')
    if(!file.exists(biom.file)){next()}
    bio.run.group[[j]] = data.table::fread(paste0(experiment.dir,experiment.id,'_',setup.group$Run.ID[j],'/neus_outputBiomIndx.txt'))%>%
      select(Time,all_of(fgs$Code))%>%
      tidyr::gather(Code,Biomass,-Time)%>%
      mutate(Run.ID = setup.group$Run.ID[j],
             file.ID = setup.group$file.ID[j])
  }
  bio.run.group = bind_rows(bio.run.group)
  
  spp.names = sort(unique(fgs$LongName))
  s=1

  pdf(paste0(figure.dir,experiment.id,'_',run.groups[i],'.pdf'))
  for(s in 1:length(spp.names)){
    
    spp.code = fgs$Code[which(fgs$LongName == spp.names[s])]
    bio.spp = bio.run.group %>%
      filter(Code == spp.code)
    
    
    p =ggplot(bio.spp, aes(x=Time,y = Biomass, color = factor(Run.ID)))+
      geom_line()+
      scale_color_manual(name = paste0('Run ID (',run.groups[i],')'),
                         values = plot.colors[1:length(unique(setup.group$Run.ID))])+
      ggtitle(spp.names[s])+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom')
    gridExtra::grid.arrange(p)
  }
  dev.off()
}
