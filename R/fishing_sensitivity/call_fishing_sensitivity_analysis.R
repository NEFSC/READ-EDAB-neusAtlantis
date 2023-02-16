#Wrapper script to call functions related to fishing sensitivity analysis
library(dplyr)
library(ggplot2)
library(gridExtra)

#Scenario specifics

guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore')
fishing.levels.scalar = c(0,0.1,0.5,1.1,1.25,1.5,2,5,10,25,50,100)
fishing.levels.text = c('0','0_1','0_5','1_1','1_25','1_5','2','5','10','25','50','100')

batch.prefix = 'fishing_sensitivity_extended_constant_3'
out.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/post_processed/')
batch.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/run_data/')
data.dir = paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/post_processed/')
fig.dir =  paste0('/home/jcaracappa/atlantis/Shared_Data/',batch.prefix,'/figures/')


#Create run index
source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_run_index.R'))
make_fishing_sensitivity_run_index(out.file= paste0(out.dir,'run_index.rds'),
                                   guild.names = guild.names,
                                   fishing.levels.scalar = fishing.levels.scalar,
                                   fishing.levels.text = fishing.levels.text)

#Create scenario data object
source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_combined_output.R'))
make_fishing_sensitivity_combined_output(batch.dir = batch.dir,
                                         batch.prefix = batch.prefix,
                                         out.dir = out.dir,
                                         run.index.file = paste0(out.dir,'run_index.rds')
)

source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_diet_connections.R'))
make_fishing_sensitivity_diet_connections(batch.dir =batch.dir,
                                          batch.prefix = batch.prefix,
                                          out.dir = out.dir,
                                          run.index.file = paste0(out.dir,'run_index.rds'))

####Create baseline referenced data###
source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_post_processing.R'))

#With all groups
make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
                                                  filter.type = 'all',
                                                  fgs.file = here::here('currentVersion','neus_groups.csv'), 
                                                  guild.match = here::here('diagnostics','functional_groups_match.csv'), 
                                                  batch.dir = batch.dir,
                                                  batch.prefix = batch.prefix,
                                                  data.dir = data.dir,
                                                  base.biomass.file = here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputBiomIndx.txt'),
                                                  base.biomass.age.file =here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputAgeBiomIndx.txt'),
                                                  base.catch.file = here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputCatch.txt'),
                                                  run.index.file = paste0(out.dir,'run_index.rds'),
                                                  ref.years = 20
)

#With fished groups only
make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
                                                  filter.type = 'fished',
                                                  fgs.file = here::here('currentVersion','neus_groups.csv'), 
                                                  guild.match = here::here('diagnostics','functional_groups_match.csv'), 
                                                  batch.dir = batch.dir,
                                                  batch.prefix = batch.prefix,
                                                  data.dir = data.dir,
                                                  base.biomass.file = here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputBiomIndx.txt'),
                                                  base.biomass.age.file =here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputAgeBiomIndx.txt'),
                                                  base.catch.file = here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputCatch.txt'),
                                                  run.index.file = paste0(out.dir,'run_index.rds'),
                                                  ref.years = 20
)

#with fished groups w/ BH recruitment only
make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
                                                  filter.type = 'fished_BH',
                                                  fgs.file = here::here('currentVersion','neus_groups.csv'), 
                                                  guild.match = here::here('diagnostics','functional_groups_match.csv'), 
                                                  batch.dir = batch.dir,
                                                  batch.prefix = batch.prefix,
                                                  data.dir = data.dir,
                                                  base.biomass.file = here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputBiomIndx.txt'),
                                                  base.biomass.age.file =here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputAgeBiomIndx.txt'),
                                                  base.catch.file = here::here('Atlantis_Runs','Dev_02062023_extended','neus_outputCatch.txt'),
                                                  run.index.file = paste0(out.dir,'run_index.rds'),
                                                  ref.years = 20
)

#Raw Timeseries Plots of Biomass/Catch
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_timeseries.R'))
plot_fishing_sensitivity_timeseries(batch.prefix =  batch.prefix,
                                    run.index.file =paste0(out.dir,'run_index.rds'),
                                    data.dir =data.dir,
                                    fig.dir = paste0(fig.dir,'timeseries_comparisons/'),
                                    guild.match = here::here('diagnostics','functional_groups_match.csv'),
                                    fgs.file = here::here('currentVersion','neus_groups.csv'))


#Plot Scenario output relative to baseline
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_relative_baseline.R'))
plot_fishing_sensitivity_relative_baseline(fgs.file = here::here('currentVersion','neus_groups.csv'),
                                           data.dir =data.dir,
                                           fig.dir = paste0(fig.dir,'realtive_baseline/'),
                                           plot.species = T,
                                           plot.guild = T,
                                           plot.guild.match = T,
                                           plot.exploitation = T,
                                           ref.years = 20,
                                           filter.type = 'all',
                                           guild.match = here::here('diagnostics','functional_groups_match.csv')
)

#plot age structure relative to baseline

#As a line chart
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_relative_baseline_age.R'))
plot_fishing_sensitivity_relative_baseline_age(data.dir = data.dir,
                                               fig.dir = paste0(fig.dir,'relative_baseline/'),
                                               filter.type = 'all',
                                               ref.years = 20,
                                               guild.match = here::here('diagnostics','functional_groups_match.csv'),
                                               fgs.file = here::here('currentVersion','neus_groups.csv')
)
#As a bubble plot
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_age_distribution.R'))
plot_fishing_sensitivity_age_distribution(data.dir =data.dir,
                                          fig.dir = paste0(fig.dir,'age_distribution/'),
                                          filter.type = 'all',
                                          ref.years = 20,
                                          guild.match = here::here('diagnostics','functional_groups_match.csv'),
                                          fgs.file = here::here('currentVersion','neus_groups.csv')
)

#plot guild composition by speciess
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_guild_composition.R'))
plot_fishing_sensitivity_guild_composition(data.dir =data.dir,
                                           fig.dir = fig.dir,
                                           filter.type = 'fished',
                                           ref.years = 20,
                                           guild.match = here::here('diagnostics','functional_groups_match.csv'),
                                           fgs.file = here::here('currentVersion','neus_groups.csv'))


