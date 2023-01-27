#Wrapper script to call functions related to fishing sensitivity analysis
library(dplyr)
library(ggplot2)
library(gridExtra)
#Source functions
source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_run_index.R'))
source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_combined_output.R'))
source(here::here('R','fishing_sensitivity','make_fishing_sensitivity_post_processing.R'))
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_timeseries.R'))
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_relative_baseline.R'))
source(here::here('R','fishing_sensitivity','plot_fishing_sensitivity_relative_baseline_age.R'))
#Create run index
make_fishing_sensitivity_run_index(out.name = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
                                   guild.names = c('Apex_Predator','Benthivore','Benthos','Piscivore','Planktivore'),
                                   fishing.levels.scalar = c(0,0.5,1.5,2.5,5,10,15,20,40,60,100),
                                   fishing.levels.text = c('0','0_5','1_5','2_5','5','10','15','20','40','60','100'))

#Create scenario data object
make_fishing_sensitivity_combined_output(batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
                                         batch.prefix = 'fishing_sensitivity_extended_constant_2',
                                         out.dir = here::here('data','fishing_sensitivity_extended_constant_2',''), #output directory
                                         run.index.file = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds')
)

####Create baseline referenced data###

#With all groups
make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
                                                  filter.type = 'all',
                                                  fgs.file = here::here('currentVersion','neus_groups.csv'), 
                                                  guild.match = here::here('diagnostics','functional_groups_match.csv'), 
                                                  batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
                                                  batch.prefix = 'fishing_sensitivity_extended_constant_2',
                                                  data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
                                                  base.biomass.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputBiomIndx.txt'),
                                                  base.biomass.age.file =here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputAgeBiomIndx.txt'),
                                                  base.catch.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputCatch.txt'),
                                                  run.index.file =  here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
                                                  ref.years = 20
)

#With fished groups only
make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
                                                  filter.type = 'fished',
                                                  fgs.file = here::here('currentVersion','neus_groups.csv'), 
                                                  guild.match = here::here('diagnostics','functional_groups_match.csv'), 
                                                  batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
                                                  batch.prefix = 'fishing_sensitivity_extended_constant_2',
                                                  data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
                                                  base.biomass.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputBiomIndx.txt'),
                                                  base.biomass.age.file =here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputAgeBiomIndx.txt'),
                                                  base.catch.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputCatch.txt'),
                                                  run.index.file =  here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
                                                  ref.years = 20
)

#with fished groups w/ BH recruitment only
make_fishing_sensitivity_scenario_post_processing(base.bio.prm = here::here('currentVersion','at_biology.prm'),
                                                  filter.type = 'fished_BH',
                                                  fgs.file = here::here('currentVersion','neus_groups.csv'), 
                                                  guild.match = here::here('diagnostics','functional_groups_match.csv'), 
                                                  batch.dir = '/media/jcaracappa/06b7679b-9bac-4c53-9cf3-9abecb801e6d/home.orig/jcaracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fishing_sensitivity_extended_constant_2/',
                                                  batch.prefix = 'fishing_sensitivity_extended_constant_2',
                                                  data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
                                                  base.biomass.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputBiomIndx.txt'),
                                                  base.biomass.age.file =here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputAgeBiomIndx.txt'),
                                                  base.catch.file = here::here('Atlantis_Runs','Extended_Constant_Catch','neus_outputCatch.txt'),
                                                  run.index.file =  here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
                                                  ref.years = 20
)

#Raw Timeseries Plots of Biomass/Catch
plot_fishing_sensitivity_timeseries(batch.prefix =  'fishing_sensitivity_extended_constant_2',
                                    run.index.file = here::here('data','fishing_sensitivity_extended_constant_2','run_index.rds'),
                                    data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
                                    fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','timeseries_comparisons',''),
                                    guild.match = here::here('diagnostics','functional_groups_match.csv'),
                                    fgs.file = here::here('currentVersion','neus_groups.csv'))


#Plot Scenario output relative to baseline
plot_fishing_sensitivity_relative_baseline(fgs.file = here::here('currentVersion','neus_groups.csv'),
                                           data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
                                           fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','relative_baseline',''),
                                           plot.species = T,
                                           plot.guild = T,
                                           plot.guild.match = T,
                                           plot.exploitation = T,
                                           ref.years = 20,
                                           filter.type = 'all',
                                           guild.match = here::here('diagnostics','functional_groups_match.csv')
)

plot_fishing_sensitivity_relative_baseline_age(data.dir = here::here('data','fishing_sensitivity_extended_constant_2',''),
                                               fig.dir = here::here('Figures','fishing_sensitivity_extended_constant_2','relative_baseline',''),
                                               filter.type = 'all',
                                               ref.years = 20,
                                               guild.match = here::here('diagnostics','functional_groups_match.csv'),
                                               fgs.file = fgs.file = here::here('currentVersion','neus_groups.csv')
)