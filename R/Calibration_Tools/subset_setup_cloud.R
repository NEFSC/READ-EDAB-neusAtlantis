setup.df = read.csv(here::here('Setup_Files','cloud_new_age_1.csv'),as.is=T)%>%
  filter(Type %in% c('mum','C'))
setup.df$Run.ID = 1:nrow(setup.df)

write.csv(setup.df,here::here('Setup_Files','cloud_new_age_1_mumC.csv'),row.names = F)