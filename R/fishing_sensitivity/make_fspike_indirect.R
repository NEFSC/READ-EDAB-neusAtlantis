library(dplyr)
library(ggplot2)

data.files = list.files('C:/Users/joseph.caracappa/Documents/Data/fspike1/',full.names = T)

fig.dir = 'C:/Users/joseph.caracappa/Documents/Manuscripts/NEUSv2_fishing_sensitivity/Figures/'

i=19
for(i in 1:length(data.files)){

  dat = readRDS(data.files[i])
  
  group.names = sort(unique(dat$LongName))
  
  target.spp = dat$target.species[1]
  
  j=1
  p.ls = list()
  for(j in 1:length(group.names)){
    
    dat.spp = dat %>% 
      filter(dat$LongName == group.names[j])
    
    p.ls[[j]] = ggplot(dat.spp, aes(x = Time, y = Value, color = factor(scalar)))+
      geom_line()+
      ggtitle(group.names[j])+
      theme_bw()+
      theme(legend.position = 'bottom')
  }
 
  pdf(paste0(fig.dir,target.spp,'_indirect.pdf'),width = 12, height = 8)
  for(k in 1:length(p.ls)){gridExtra::grid.arrange(p.ls[[k]]) }
  dev.off()
}