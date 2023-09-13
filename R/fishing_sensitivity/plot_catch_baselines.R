data.dir = paste0('/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/')

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)

fished.recovery = read.table(paste0(data.dir,'total_catch_project_mean_20yrs.ts')) 
colnames(fished.recovery) = c('Time',fgs$Code)

unfished.recovery = read.table(paste0(data.dir,'total_catch_fspike_UnfishedRecovery.ts'))
colnames(unfished.recovery) = c('Time',fgs$Code)

fished.recovery.long = fished.recovery %>%
  tidyr::gather('Code','Catch',-Time)%>%
  mutate(source = 'fished recovery')

unfished.recovery.long = unfished.recovery %>%
  tidyr::gather('Code','Catch',-Time)%>%
  mutate(source = 'unfished recovery')

catch.all = fished.recovery.long%>%
  bind_rows(unfished.recovery.long)

i= 1
for(i in 1:nrow(fgs)){

  catch.spp = catch.all %>%
    filter(Code == fgs$Code[i])
  
  ggplot(catch.spp,aes(x= Time, y = Catch, color = source))+
    geom_line()+
    theme_bw()+
    ggtitle(fgs$LongName[i])+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom')
}
