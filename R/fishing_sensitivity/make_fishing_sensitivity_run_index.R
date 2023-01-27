#Function to create table of fishing scenario run index
# out.file = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds')
# guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')
# fishing.levels.scalar = c(0,0.5,1.5,2.5,5,10,15,20,40,60,100)
# fishing.levels.text = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100')

make_fishing_sensitivity_run_index = function(out.name,guild.names,fishing.levels.scalar,fishing.levels.text){
  
  scenario.combs = expand.grid('guild.names' = guild.names, 'fishing.levels.scalar' = fishing.levels.scalar) %>%
    arrange(guild.names)%>%
    left_join(data.frame(fishing.levels.scalar = fishing.levels.scalar,fishing.levels.text = fishing.levels.text))
  
  saveRDS(scenario.combs,file = out.file)
}

# make_fishing_sensitivity_run_index(out.file = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
#                                    guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore'),
#                                    fishing.levels.scalar = c(0,0.5,1.5,2.5,5,10,15,20,40,60,100),
#                                    fishing.levels.text = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100'))