library(dplyr)
run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/FinalPersist_6/'

bio.file = 'neus_outputBiomIndx.txt'
fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)$Code

data = read.table(paste0(run.dir,bio.file),stringsAsFactors = F,header = T)

group.names = colnames(data)

rel.data = data %>% select(group.names[grepl('Rel*',group.names)])
group.names = group.names[2:90]

out.df = data.frame(group = group.names,
                    rel.min = apply(rel.data,2,min,na.rm=T),
                    rel.max = apply(rel.data,2,max,na.rm=T))
write.csv(out.df,'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/FinalPersist_4_relValue.csv',row.names = F)

out.extr = out.df %>% filter(rel.min<0.1 | rel.max > 10)

out.df %>%
  mutate(high = ifelse(rel.max>10,'X',''),
         low = ifelse(rel.min<0.1,'X',''))
