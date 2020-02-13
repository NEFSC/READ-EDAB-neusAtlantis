#### Script to do all post-processing/diagnostic plots ( combination of RM post-processing, JC primary production diagnostics, and box-layer biomass plots)

  
  library(ncdf4)
  library(ggplot2)
  library(reshape2)
  library(tidyr)
  library(tidyverse)
  library(RColorBrewer)
  library(gridExtra)
  library(grid)
  library(atlantistools)
  library(stringr)

  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  
  
  #Name of model run, must match the prefix defined in the run code
  runfile='02062020_PrimProdDiag_1'
  #Parameter File Directory (Preferably a clone of the repository); where (PRM, bgm, group data) are saved
  param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/'
  #Run Directory; where output is generated
  run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/'
  #choose plots
  
  run_preprocess = F
  diagnostic_plots = F
  ## Visualization of spatial biomass by box/layer
  make_spatial_plots=F
  ## Visualization of diet data over time
  make_diet_pdf=F
  ## Timeseries of lower trophics
  make_LTL_plots=F
  ## Spatial biomass as timeseries
  make_box_layer_plots = F
  ## Plot last X years of model
  make_end_model_plots = T
  ## Primary Production Diagnostics
  make_ppdiag_plots = F
  ## Compare group diets across multiple runs
  make_dietcomp_plots =F
  ## Compare group biomass across multiple runs
  make_biomasscomp_plots = T
  #If catch was included in run:
  include_catch=T
  
  #check initial conditions scaling of biomass?
  check_scale_init=T
  
  #Choose focal box/layer for more detailed output
  tb=20 # choose box
  ll=4 # choose layer (4 is bottom for NEUS)
  
  #Additional Files
  layer.key.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Box Layer Key.csv'
  fgs.file = 'NeusGroups_v15_unix_RM.csv'
  
  #Plot Variables
  ##Variables by name in NC file
  # nc.vars = c('Diatom_N','vflux','eflux','Oxygen','salt','Temp','NH3','NO3','Chl_a','DON','Diatom_S',"Light",'Stress','Lobster_N','Scallop_N')
  nc.vars = c('Diatom_N','DON','Scallop_N')
  ##Which layers to plot for spatial plots (1 = surface, 5 = sediment)
  plot.layers = 1:5
  
  #When to start and stop plotting
  tstart = 20
  tstop = 50
  
  #Focal species for biomass plots
  spp.id = c('PL','SCA','LOB','BO','PL')
  #Focal species for diet comparisons
  prey.groups = c('SCA','LOB','BO','PL')
  #Runfile prefixes for comparison runs
  other.runs = c('atneus_v15_01272020','02042020_NewBivalvePred_3','02062020_PrimProdDiag_KN')
  #Method for calculating top predators/prey; 'percent' means top 'diet.fraction' of prey; 'topn' means top 'diet.topn' prey groups
  diet.method = 'percent'
  diet.fract = 0.9
  diet.topn = 3
  #Flag for plotting biomass timeseries on log scale. Useful for runs with widely different magnitude biomass
  log.biomass = T
  #Which primary producer group for prim.prod diaganostics
  prim.prod = 'PL'
  
  setwd(paste0(run.dir,runfile,'/'))
  #Runs RM preprocess code 
  if(run_preprocess == T){
      #Generates PreProcess output in long list
    source(paste0(param.dir,'R/JC_PreProcess_Output.R'))
  } else {
    load(paste0(run.dir,runfile,'/',runfile,'_prepro.rdata'))
  }
  if(diagnostic_plots == T){
    source(paste0(param.dir,'R/JC_RM_DiagnosticPlots.R'))
    graphics.off()
  }
  #Creates general diagnostic figures
  
  if(make_box_layer_plots ==T){
    source(paste0(param.dir,'R/Box_Layer_Biomass.R'))
  }
  
  if(make_end_model_plots==T){
    source(paste0(param.dir,'R/End_of_Model_Biomass.R'))
  }
  
  if(make_dietcomp_plots == T | make_biomasscomp_plots == T){
    source(paste0(param.dir,'R/Diet_Comparisons.R'))
  }
  
  if(make_ppdiag_plots == T){
    source(paste0(param.dir,'R/Primary_Producer_Diagnostics.R'))
  }
  

  