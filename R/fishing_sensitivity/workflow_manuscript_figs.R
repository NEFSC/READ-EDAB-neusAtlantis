#workflow scrip to generate all manuscript plot figures

script.dir = here::here('R','fishing_sensitivity','manuscript')

source(list.files(script.dir,'figure_1_domain.R',full.names = T))

source(list.files(script.dir,'figure_2_biomass_age_F.r',full.names = T))

source(list.files(script.dir,'figure_3_thresholds_v_exploitation_guild.R',full.names = T))

source(list.files(script.dir,'figure_4_5_robustness_exploitation.R',full.names = T))

source(list.files(script.dir,'figure_6_recovery_timeline.R',full.names = T))

source(list.files(script.dir,'figure_7_initial_disturbance_size.R',full.names = T))

source(list.files(script.dir,'figure_7b_initial_disturbance_size_noMulti.R',full.names = T))

source(list.files(script.dir,'figure_8_alt_recovery_5yr.R',full.names = T))

source(list.files(script.dir,'figure_8b_alt_recovery_15yr.R',full.names = T))

source(list.files(script.dir,'figure_8b_recovery_15yr.R',full.names = T))

source(list.files(script.dir,'figure_8c_recovery_diff_15yr.R',full.names = T))

source(list.files(script.dir,'figure_9_distance_sample.R',full.names = T))

source(list.files(script.dir,'make_fscale_model_stats.R',full.names = T))

source(list.files(script.dir,'table_1_species_metrics.r',full.names = T))

source(list.files(script.dir,'table_2_path_dependence.R',full.names = T))

source(list.files(script.dir,'table_2_recovery_stats.R',full.names = T))

source(list.files(script.dir,'table_3_path_dependence_stats.R',full.names = T))

