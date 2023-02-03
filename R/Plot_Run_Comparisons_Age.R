#Script to analyze zooplankton testing output

###############################
####___Change These Only___####
###############################

#Set Project Name
#Set where figures are to be written
fig.dir = here::here('Figures/')
#Define run name
run.name = 'GOO_etc_13'
#Define run set directory
run.dir = here::here('Atlantis_Runs',run.name,'')
#Define run setup file (long version with actual scalars)
run.index = read.csv(here::here('Setup_Files/GOO_etc_13_long.csv'),as.is = T)
#Define Parameter Files Director
param.dir = here::here('currentVersion','')
#Define groups you want plotted
# group.names = unique(run.index$group)
x = read.csv(paste0(param.dir,'neus_groups.csv'),header = T,as.is = T) %>% filter(IsTurnedOn == T)
group.names = sort(x$Code)

run.dirs = 

##Which plots do you want it to write

#Raw total biomass
plot.raw = F
#Relative biomass
plot.rel = T
#Log Scaled biomass
plot.log = F

# plot.age = 0
###############################
###############################

library(dplyr)
library(ggplot2)

#Define Directories
folders = unique(paste0(run.dir,run.index$dir.name))

#Loop through folders and create a dataframe of desired group biomass using dir.name as index
out.ls = list()
fgs = read.csv(paste0(param.dir,'neus_groups.csv'),header = T) %>% filter(IsTurnedOn == T)

i=1
for(i in 1:length(folders)){
  file.name = paste0(folders[i],'/neus_outputBiomIndx.txt')
  if(file.exists(file.name)){
    dat = read.table(paste0(folders[i],'/neus_outputAgeBiomIndx.txt'),header =T, as.is = T)
    dat2 = dat %>% select(Time,starts_with(group.names))
    dat.long = reshape2::melt(dat2,id.vars = 'Time')
    # group.age = paste0(group.names,'.',plot.age)
    dat.long.age = dat.long %>%
      # filter(variable %in% group.age) %>%
      tidyr::separate(variable,c('group','age'))%>%
      mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
             year = as.numeric(format(date,format = '%Y')))%>%
      group_by(group,age,year)%>%
      summarise(value = mean(value,na.rm=T))%>%
      ungroup()%>%
      mutate(dir.name = unique(run.index$dir.name)[i],
             ID = i-1)
    out.ls[[i]] = dat.long.age
  }else{
    next()
  }

}
data.combined = bind_rows(out.ls)

#Merge with run.index to get parameters
data.combined = left_join(data.combined, run.index )

# data.combined %>%filter(group == 'DOG')%>%select(Time,value,ID)%>%tidyr::spread(ID,value)
#plot each of the interest groups
plot.cols = c(RColorBrewer::brewer.pal(12,'Paired'),RColorBrewer::brewer.pal(8,'Dark2'),RColorBrewer::brewer.pal(8,'Accent'))[1:nrow(run.index)]

if(plot.log){
  pdf(paste0(fig.dir,run.name,'_Log_Biomass_Age.pdf'),onefile = T, width =10, height = 8)
  for(spp in 1:length(group.names)){
    dat = filter(data.combined, variable == group.names[spp])
    
    plot.out = ggplot(dat, aes(x=year, y = log(value,10), color = as.factor(ID)))+
      facet_wrap(~age, scale = 'free_y',nrow = 3)+
      scale_color_manual(values = plot.cols)+
      ylab('log(Biomass) (mT)')+
      labs(color = 'Init Scalar')+
      theme_bw()+
      geom_line()+
      ggtitle(group.names[spp])+
      guides(color = guide_legend(nrow = 2, byrow = T))+
      theme(legend.position = 'bottom',
            legend.box = 'horizontal',
            plot.title = element_text(hjust= 0.5))
    gridExtra::grid.arrange(plot.out)
  }
  dev.off()
}

if(plot.rel){
  pdf(paste0(fig.dir,run.name,'_Relative_Biomass_Age.pdf'),onefile = T, width =10, height = 8)
  for(spp in 1:length(group.names)){
    dat = filter(data.combined, group == group.names[spp])
    plot.out = ggplot(dat,aes(x = year, y= value/value[1],color = as.factor(ID)))+
      facet_wrap(~age, scale = 'free_y',nrow = 3)+
      ylab('Relative Biomass')+
      labs(color = 'Init Scalar')+
      theme_bw()+
      geom_line()+
      scale_color_manual(values = plot.cols)+
      ggtitle(group.names[spp])+
      guides(color = guide_legend(nrow = 2, byrow = T))+
      theme(legend.position = 'bottom',
            legend.box = 'horizontal',
            plot.title = element_text(hjust= 0.5))
    gridExtra::grid.arrange(plot.out)
  }
  dev.off()
}

if(plot.raw){
  pdf(paste0(fig.dir,run.name,'_Raw_Biomass_Age.pdf'),onefile = T, width =10, height = 8)
  for(spp in 1:length(group.names)){
    dat = filter(data.combined, variable == group.names[spp])
    plot.out = ggplot(dat,aes(x = year, y= value,color = as.factor(ID)))+
      facet_wrap(~age, scale = 'free_y',nrow = 3)+
      ylab('Relative Biomass')+
      labs(color = 'Init Scalar')+
      theme_bw()+
      geom_line()+
      scale_color_manual(values = plot.cols)+
      ggtitle(group.names[spp])+
      guides(color = guide_legend(nrow = 2, byrow = T))+
      theme(legend.position = 'bottom',
            legend.box = 'horizontal',
            plot.title = element_text(hjust= 0.5))
    gridExtra::grid.arrange(plot.out)
  }
  dev.off()
}
