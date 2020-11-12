# Compares the landings data from 
# A) Current Atlantis total_catch.ts input converted to tons/year
# B) Current Atlantis catch output
# C) Updated comlandr data
# D) Old comlandr pull
# E) StockSmart pull
# F) Join and plot by species

library(dplyr)
library(ggplot2)

# A) Current Atlantis Input (total_catch.ts) ------------------------------

catch.ts.file = here::here('currentVersion','CatchFiles','total_catch.ts')

#get .ts colnames and first data line
catch.ts.lines = readLines(catch.ts.file)
catch.ts.grep.names = grep('COLUMN.*\\.name',catch.ts.lines,value = T)
catch.ts.colnames = sapply(catch.ts.grep.names, function(x) return(strsplit(x,'\t| ')[[1]][3]),USE.NAMES = F)
catch.ts.skip = last(grep('#',catch.ts.lines))

#read in data.frame
catch.ts.wide = read.table(catch.ts.file,skip = catch.ts.skip)
colnames(catch.ts.wide) = catch.ts.colnames

#Reformat and aggregate catch.ts.long to annual sums
#Conversion from mgN/s to tonnes/day = 9.85E-3
catch.ts.raw = reshape2::melt(catch.ts.wide,id.var = 'Time',variable.name = 'Group',value.name = 'catch')
catch.ts.df = catch.ts.raw %>%
  mutate(Date = as.Date('1964-01-01')+ Time,
         Year = format(Date,format = '%Y')) %>%
  group_by(Year, Group) %>%
  summarize(Catch = sum(catch*9.849E-3,na.rm=T)) %>%
  arrange(Group,Year) %>%
  mutate(source = 'catch.ts') %>%
  ungroup()



# B) Current Atlantis Catch Output ----------------------------------------

atl.catch.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_10202020/neus_outputCatch.txt'

#Read in and format Catch.txt to tons/year
atl.catch.wide = read.table(atl.catch.file,header = T)
atl.catch.df = reshape2::melt(atl.catch.wide,id.var = 'Time',variable.name = 'Group',value.name = 'catch') %>%
  mutate(Date = as.Date('1964-01-01')+Time,
         Year = format(Date,format = '%Y')) %>%
  group_by(Year,Group) %>%
  summarize(Catch = sum(catch,na.rm=T)) %>%
  arrange(Group,Year) %>%
  mutate(source = 'atl.catch') %>%
  ungroup()


# C) New Comlandr Pull ----------------------------------------------------

comland.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/'
new.comland.file = paste0(comland.dir,'comland_meatwt_deflated_EPU.Rds')

#get NESPP3 to NEUS index
svspp.atl.index = read.csv(here::here('data-raw','Atlantis_1_5_groups_svspp_nespp3.csv'))

#Merge to get Atlantis codes, filter, aggregate by year
new.comland.df = readRDS(new.comland.file) %>%
  left_join(svspp.atl.index,by = 'NESPP3') %>%
  filter(!(is.na(Code))) %>%
  rename(Year = 'YEAR') %>%
  group_by(Code,Year) %>%
  summarize(Catch = sum(SPPLIVMT,na.rm=T)) %>%
  rename(Group = 'Code') %>%
  arrange(Group,Year) %>%
  mutate(source = 'new.comland',
         Year = as.character(Year)) %>%
  ungroup()


# D) Old Comlandr Pull ----------------------------------------------------

old.comland.file = paste0(comland.dir,'comland_meatwt_deflated_stat_areas.RData') 
load(old.comland.file)
old.comland.df = comland %>%
  left_join(svspp.atl.index,by = 'NESPP3') %>%
  filter(!(is.na(Code))) %>%
  rename(Year = 'YEAR') %>%
  group_by(Code,Year) %>%
  summarize(Catch = sum(SPPLIVMT,na.rm=T)) %>%
  rename(Group = 'Code') %>%
  arrange(Group,Year) %>%
  mutate(source = 'old.comland',
         Year = as.character(Year)) %>%
  ungroup()

# E) StockSmart Data ------------------------------------------------------
stocksmart.convert.tons = function(x){
  if(x == 'Thousands Metric Tons'){
    return(1000)
  }else if(x == 'Thousand Pounds'){
    return(.4536)
  }else if(x == 'Thousand Kilograms'){
    return(1E-3)
  }else{
    return(1)
  }
}

stocksmart.file = here::here('data-raw','stockData.Rds')
stocksmart.df = readRDS(stocksmart.file) %>%
  filter(Metric == 'Catch' & Units != 'Number') %>%
  group_by(Code,Year, Units) %>%
  summarize(Catch = sum(Value,na.rm=T)) %>%
  rename('Group' = Code) %>%
  arrange(Group,Year) %>%
  ungroup() %>%
  mutate(source = 'stocksmart',
         Year = as.character(Year))
  
#apply conversions for units to tons
stocksmart.df$Catch = stocksmart.df$Catch * sapply(stocksmart.df$Units,stocksmart.convert.tons,USE.NAMES = F)
stocksmart.df = dplyr::select(stocksmart.df,-Units)  

# F) Join and Plot Catch by species ---------------------------------------

#Combine datasets

save(catch.ts.raw,atl.catch.df,new.comland.df,old.comland.df,stocksmart.df,file  = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Catch_Comparison.RData')
catch.all = bind_rows(catch.ts.df,atl.catch.df,new.comland.df,old.comland.df,stocksmart.df)
# rm(list = ls()[!ls() %in% c('catch.ts.df','atl.catch.df','new.comland.df','old.comland.df','svspp.atl.index','stocksmart.df')])
# rm(list = ls()[!ls() %in% c('catch.all')])

groups = unique(catch.all$Group)
groups = groups[-grep('TsAct',groups)]

plot.cols = tibble(source = c('catch.ts','atl.catch','new.comland','old.comland','stocksmart'),
                       source.fullname = c('Catch_TS','Atl_Output','Comland_EPU','Comland_StatArea','StockSmart'),
                       color =RColorBrewer::brewer.pal(5,'Set2'))
catch.all  = catch.all %>%
  left_join(plot.cols)


pdf('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/Catch_Source_Comparisons.pdf',width = 16, height = 6, onefile = T)
for(i in 1:length(groups)){
  
  dat = filter(catch.all,Group == groups[i])
  
  g= ggplot(data = dat,aes(x = as.numeric(Year),y = Catch, col = source.fullname))+
    geom_line(size = 1.2)+
    scale_color_manual(values = plot.cols$color,name =NULL)+
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

