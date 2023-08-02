#Script to analyze zooplankton testing output
library(dplyr)
library(ggplot2)
###############################
####___Change These Only___####
###############################

#Set where figures are to be written
fig.dir = here::here('Figures','')
fig.name = 'v6645_Update'
#Define groups you want plotted
# group.names = unique(run.index$group)
x = read.csv(here::here('currentVersion','neus_groups.csv'),header = T,as.is = T) %>% filter(IsTurnedOn == T)
group.names = sort(x$Code)
group.longnames = x$LongName

v6536_nfnm = here::here('Atlantis_Runs/v6536_10yr_nfnm/')
v6645_nfnm = here::here('Atlantis_Runs/v6645_10yr_nfnm/')
v6580_nfnm = here::here('Atlantis_Runs/v6580_10yr_nfnm/')
v6536_nfnmnp = here::here('Atlantis_Runs/v6536_10yr_nfnmnp/')
v6645_nfnmnp = here::here('Atlantis_Runs/v6645_10yr_nfnmnp/')
v6580_nfnmnp = here::here('Atlantis_Runs/v6580_10yr_nfnmnp/')

run.dirs = c(v6536_nfnm,v6536_nfnmnp, v6645_nfnm, v6645_nfnmnp)
dir.names = c('v6536_nfnm','v6536_nfnmnp','v6645_nfnm','v6645_nfnmnp')

##Which plots do you want it to write
#Raw total biomass
plot.raw = T
#Relative biomass
plot.rel = F
#Log Scaled biomass
plot.log = F

# plot.age = 0
###############################
###############################

#Define Directories
#Loop through folders and create a dataframe of desired group biomass using dir.name as index
out.ls = list()
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),header = T) %>% filter(IsTurnedOn == T)

i=1
for(i in 1:length(run.dirs)){
  file.name = paste0(run.dirs[i],'/neus_outputBiomIndx.txt')
  if(file.exists(file.name)){
    dat = read.table(paste0(run.dirs[i],'/neus_outputAgeBiomIndx.txt'),header =T, as.is = T)
    dat2 = dat %>% select(Time,starts_with(group.names))
    dat.long = reshape2::melt(dat2,id.vars = 'Time')
    group.age = paste0(group.names,'.',plot.age)
    dat.long.age = dat.long %>%
      # filter(variable %in% group.age) %>%
      tidyr::separate(variable,c('group','age'))%>%
      mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
             year = as.numeric(format(date,format = '%Y')))%>%
      ungroup()%>%
      mutate(dir.name = dir.names[i])
    out.ls[[i]] = dat.long.age
  }else{
    next()
  }

}
data.combined = bind_rows(out.ls) %>%
  left_join(fgs,by = c('group' = 'Code'))%>%
  tidyr::unite('plot.name',group,LongName, sep = ' : ', remove = F)
  

#Merge with run.index to get parameters
# data.combined = left_join(data.combined, run.index )

# data.combined %>%filter(group == 'DOG')%>%select(Time,value,ID)%>%tidyr::spread(ID,value)
#plot each of the interest groups
plot.cols = c(RColorBrewer::brewer.pal(12,'Paired'),RColorBrewer::brewer.pal(8,'Dark2'),RColorBrewer::brewer.pal(8,'Accent'))[1:length(run.dirs)]

if(plot.log){
  pdf(paste0(fig.dir,fig.name,'_Log_Biomass_Age.pdf'),onefile = T, width =10, height = 8)
  for(spp in 1:length(group.names)){
    dat = filter(data.combined, variable == group.names[spp])
    
    plot.out = ggplot(dat, aes(x=year, y = log(value,10), color = as.factor(fig.name)))+
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
  pdf(paste0(fig.dir,fig.name,'_Relative_Biomass_Age.pdf'),onefile = T, width =10, height = 8)
  for(spp in 1:length(group.names)){
    dat = filter(data.combined, group == group.names[spp])
    plot.out = ggplot(dat,aes(x = year, y= value/value[1],color = dir.name))+
      geom_line()+
      facet_wrap(~age)+
      ylab('Relative Biomass')+
      labs(color = 'Init Scalar')+
      theme_bw()+
      scale_color_manual(values = plot.cols)+
      ggtitle(dat$plot.name[1])+
      guides(color = guide_legend(nrow = 2, byrow = T))+
      theme(legend.position = 'bottom',
            legend.box = 'horizontal',
            plot.title = element_text(hjust= 0.5))
    gridExtra::grid.arrange(plot.out)
  }
  dev.off()
}

if(plot.raw){
  pdf(paste0(fig.dir,fig.name,'_Raw_Biomass_Age.pdf'),onefile = T, width =10, height = 8)
  for(spp in 1:length(group.names)){
    dat = filter(data.combined, group == group.names[spp])
    plot.out = ggplot(dat,aes(x = Time, y= value,color = dir.name))+
      geom_line()+
      facet_wrap(~age,scales = 'free_y')+
      ylab('Relative Biomass')+
      labs(color = 'Init Scalar')+
      theme_bw()+
      scale_color_manual(values = plot.cols)+
      ggtitle(dat$plot.name[1])+
      guides(color = guide_legend(nrow = 2, byrow = T))+
      theme(legend.position = 'bottom',
            legend.box = 'horizontal',
            plot.title = element_text(hjust= 0.5))
    gridExtra::grid.arrange(plot.out)
  }
  dev.off()
}
