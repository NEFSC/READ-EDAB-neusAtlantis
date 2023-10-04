data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fspike_combined/'
table.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/tables/'

guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
guild.colors = RColorBrewer::brewer.pal(11,'Paired')
names(guild.colors) = sort(unique(guild2spp$Guild))
guild.color.df = data.frame(Guild = sort(unique(guild2spp$Guild)),plot.color = guild.colors)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)%>% select(Code,LongName)

bio.run.stats = readRDS(paste0(data.dir,'recovery_stats_fspike_combined.rds'))%>%
  filter(scalar %in% c(2,5,10,25,50,100))%>%
  left_join(fgs)%>%
  mutate(db.tmin = signif(db.tmin,2),
         db.t5 = signif(db.t5,2),
         db.t10 = signif(db.t10,2),
         db.t20 = signif(db.t20,2))%>%
  mutate(is.zero = db.tmin == 0)%>%
  group_by(LongName,is.zero)%>%
  mutate(is.min = scalar == min(scalar))%>%
  ungroup()%>%
  mutate(remove  = is.zero == T & is.min == F)%>%
  filter(remove == F)%>%
  select(LongName,scalar,db.tmin,db.t5,db.t20)

write.csv(bio.run.stats, paste0(table.dir,'table_2_recovery_stats.csv'),row.names= F)


