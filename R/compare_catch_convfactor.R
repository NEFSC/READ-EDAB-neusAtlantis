#Compare Catch from Atlantis Runs with Old/New Conversion Factor and Original Comlands Data
#Comland in tons/year
#Atlantis takes mgN/s

library(dplyr)
library(ggplot2)

#Read in all comland data aggregated by species and year
load('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Catch_Comparison.RData')

#Atlantis output with old conversion factor (0.556)
atl.catch.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/New_Init_Scalar_5/neus_outputCatch.txt'

#Read in and format Catch.txt to tons/year
atl.catch.wide = read.table(atl.catch.file,header = T)
old.atl.catch.df = reshape2::melt(atl.catch.wide,id.var = 'Time',variable.name = 'Group',value.name = 'catch') %>%
  mutate(Date = as.Date('1964-01-01')+Time,
         Year = format(Date,format = '%Y')) %>%
  group_by(Year,Group) %>%
  summarize(Catch = sum(catch,na.rm=T)) %>%
  arrange(Group,Year) %>%
  mutate(source = 'old_atl') %>%
  ungroup()

#Atlantis output with new conversion factor (0.278)
atl.catch.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Test_CatchTS/neus_outputCatch.txt'

#Read in and format Catch.txt to tons/year
atl.catch.wide = read.table(atl.catch.file,header = T)
new.atl.catch.df = reshape2::melt(atl.catch.wide,id.var = 'Time',variable.name = 'Group',value.name = 'catch') %>%
  mutate(Date = as.Date('1964-01-01')+Time,
         Year = format(Date,format = '%Y')) %>%
  group_by(Year,Group) %>%
  summarize(Catch = sum(catch,na.rm=T)) %>%
  arrange(Group,Year) %>%
  mutate(source = 'new_atl') %>%
  ungroup()


#Combine with old comland data
catch.all = bind_rows(new.atl.catch.df,old.atl.catch.df,old.comland.df)
groups = unique(catch.all$Group)


#Plot Results
pdf('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/Catch_Conversion_Factor_Comparison.pdf',width = 16, height = 6, onefile = T)
for(i in 1:length(groups)){
  
  dat = filter(catch.all,Group == groups[i])
  
  g= ggplot(data = dat,aes(x = as.numeric(Year),y = Catch, col = source))+
    geom_line(size = 0.5)+
    scale_color_manual(values = RColorBrewer::brewer.pal(3,'Set1'),name =NULL)+
    ylab('Catch ( MT/yr )')+
    xlab('')+
    ggtitle(groups[i])+
    theme_bw()+
    theme(
      legend.position= 'bottom',
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank()
    )
  
  gridExtra::grid.arrange(g)
}
dev.off()
