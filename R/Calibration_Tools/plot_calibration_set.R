#' Script to create plot comparisons between individual runs of the same Run.Group
#' Also produces mean biomass of target species

#Specify setup.csv used to generate calibration run set
experiment.id = 'test_1'
setup.df = read.csv(here::here('diagnostics','cloud_calibration_setup_example.csv'),as.is=T)


