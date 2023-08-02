#Script to plot NEUS output in terms of true Functional Group Types
library(dplyr)
library(ggplot2)

#Get functional group match
soe.fgs = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code,FuncGroup,LongName)

#Get run info
run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/PL_DF_SlowSink_4/'
fig.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/PL_DF_SlowSink_4/Post_Processed/'

plot.cols = rep(RColorBrewer::brewer.pal(12,'Paired'),3)

biomind.raw = read.table(paste0(run.dir,'neus_outputBiomIndx.txt'),header =T)%>%
  select(Time:DC)%>%
  reshape2::melt(id.var = 'Time')%>%
  mutate(date = as.POSIXct(Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC'),
         year = as.numeric(format(date,format = '%Y')),
         value = as.numeric(value))%>%
  rename(Code = 'variable')%>%
  group_by(year,Code)%>%
  summarise(value = mean(value,na.rm=T))%>%
  left_join(soe.fgs)

biomind= biomind.raw %>%
  group_by(year,FuncGroup)%>%
  summarise(value = sum(value,na.rm=T))
  
ggplot(data = biomind,aes(x=year,y = value,fill = FuncGroup))+
  geom_bar(position = 'stack',stat = 'identity')+
  scale_fill_manual(values = plot.cols)+
  ggtitle('Functional Group Composition')+
  ylab('Biomass (mT)')+
  xlab('')+
  theme_bw()+
  ggsave(paste0(fig.dir,'Functional_Group_Composition.png'),width =12 ,height = 8, units = 'in',dpi = 300)

biomind.nospinup = biomind %>%
  filter(year >= 1998)

ggplot(data = biomind.nospinup,aes(x=year,y = value,fill = FuncGroup))+
  geom_bar(position = 'stack',stat = 'identity')+
  scale_fill_manual(values = plot.cols)+
  ggtitle('Functional Group Composition')+
  ylab('Biomass (mT)')+
  xlab('')+
  theme_bw()+
  ggsave(paste0(fig.dir,'Functional_Group_Composition_NoSpinup.png'),width =12 ,height = 8, units = 'in',dpi = 300)

biomind.totals = biomind %>%
  group_by(year)%>%
  summarise(total = sum(value,na.rm=T))

biomind.prop = biomind %>%
  left_join(biomind.totals)%>%
  mutate(value.prop = value/total)

biomind.prop.nospinup = biomind.prop %>%
  filter(year >= 1998)

ggplot(data = biomind.prop,aes(x=year,y = value.prop,fill = FuncGroup))+
  geom_bar(position = 'stack',stat = 'identity')+
  scale_fill_manual(values = plot.cols)+
  ggtitle('Functional Group Composition')+
  ylab('Biomass (mT)')+
  xlab('')+
  theme_bw()+
  ggsave(paste0(fig.dir,'Functional_Group_Composition_Proportion.png'),width =12 ,height = 8, units = 'in',dpi = 300)

ggplot(data = biomind.prop.nospinup,aes(x=year,y = value.prop,fill = FuncGroup))+
  geom_bar(position = 'stack',stat = 'identity')+
  scale_fill_manual(values = plot.cols)+
  ggtitle('Functional Group Composition')+
  ylab('Biomass (mT)')+
  xlab('')+
  theme_bw()+
  ggsave(paste0(fig.dir,'Functional_Group_Composition_Proportion_NoSpinup.png'),width =12 ,height = 8, units = 'in',dpi = 300)

#Do props for each group
new.fgs = unique(soe.fgs$FuncGroup)
groups = unique(soe.fgs$Code)

pdf(paste0(fig.dir,'Functional_Group_Composition_Proportion_NoSpinup_DetailedView_All.pdf'),onefile = T,width = 12, height = 8)
for(i in 1:length(new.fgs)){
  
  bio.sub = biomind.raw %>%
    filter(FuncGroup == new.fgs[i])
    # filter(year >= 1998)
  
  bio.total = bio.sub %>%
    group_by(year) %>%
    summarise(total = sum(value,na.rm =T))
  
  bio.sub.prop = bio.sub %>%
    left_join(bio.total)%>%
    mutate(value.prop = value/total)
  
  p = ggplot(data = bio.sub.prop,aes(x=year,y = value.prop,fill = LongName))+
    geom_bar(position = 'stack',stat = 'identity')+
    scale_fill_manual(values = plot.cols)+
    ggtitle(paste0('Functional Group Composition: ',new.fgs[i]))+
    ylab('Biomass (mT)')+
    xlab('')+
    theme_bw()
  
  gridExtra::grid.arrange(p)
}
dev.off()
  