#Script to run multiple batcher run comparisons
library(dplyr)
library(ggplot2)

source(here::here('R','plot_run_comparisons.R'))
source(here::here('R','plot_run_catch_comparisons.R'))

batch.prefix =  'fishing_sensitivity_extended_constant_2'
guild.names =  c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')
fishing.levels = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100')

batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/'

run.dirs = list.dirs(batch.dir,recursive = F)
run.names = list.dirs(batch.dir,full.names = F,recursive = F)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),stringsAsFactors = F) %>%
  filter(IsTurnedOn == T) %>%
  select(Code)
  

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T)%>%
  select(Code,Guild)%>%
  filter(Guild %in% guild.names & Code %in% fgs$Code)

#make output figure directory
figure.dir = here::here('Figures',batch.prefix,'')
dir.create(figure.dir)

for( i in 4:length(guild.names)){
  
  model.names = paste0(batch.prefix,'_',guild.names[i],'_',fishing.levels)
  model.dirs = paste0(batch.dir,model.names,'/')
  
  guild.match = spp2guild$Code[which(spp2guild$Guild == guild.names[i])]
  
  plot_run_comparisons(
    model.dirs = model.dirs,
    model.names = model.names,
    plot.raw = T,
    plot.diff = F,
    plot.out = paste(figure.dir,guild.names[i]),
    table.out = F,
    groups = NULL,
    remove.init = F
  )
  
  plot_run_catch_comparisons(
    model.dirs = model.dirs,
    model.names = model.names,
    plot.raw = T,
    plot.diff = F,
    plot.out = paste(figure.dir,guild.names[i]),
    table.out = F,
    groups = NULL,
    remove.init = F
  )
  
  plot_run_comparisons(
    model.dirs = model.dirs,
    model.names = model.names,
    plot.raw = T,
    plot.diff = F,
    plot.out = paste(figure.dir,guild.names[i],'_WITHIN_GUILD_'),
    table.out = F,
    groups = guild.match,
    remove.init = F
  )
  
  plot_run_catch_comparisons(
    model.dirs = model.dirs,
    model.names = model.names,
    plot.raw = T,
    plot.diff = F,
    plot.out = paste(figure.dir,guild.names[i],'_WITHIN_GUILD_'),
    table.out = F,
    groups = guild.match,
    remove.init = F
  )
  
  
}
