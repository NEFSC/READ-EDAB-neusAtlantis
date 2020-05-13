#' Check initial scaling
#' 
#' @description Generates a CSV file to check that the realized initial conditions match those specified in the run.prm file.
#' 
#' @param.dir string. Path to parameter files
#' @out.dir string. Path to save output
#' @log.start numeric. Line number for start of first timestep in log.txt
#' @ngroups numeric. Number of functional groups in model
#' 
#' @return CSV file containing functional groups, initial biomass, first timestep biomass, and realized initial scalar
#' 
#' 
#' Author: Ryan Morse, modified by Joseph Caracappa
#' 

# log.start = 59611
# ngroups = 89
check_init_scale = function(param.dir,out.dir,log.start, ngroups ){
  
  #Load in lines from log.txt for first timestep
  con = file(paste0(atl.dir,'log.txt'),'r')
  log.lines = readLines(con)
  close(con)
  
  #Separate each line by punctuation and extract current and initial biomass in tonnes
  timesteps = data.frame(line = log.lines[log.start, log.start+ngroups])
  line.split = tidyr::separate(timesteps,line, c("t", "z1", "z2", "z3",
                                                 "s", "code", "a1", "a2",
                                                 "i1", "i2", "b1","b2",
                                                 "b3","b4","tonnes.now1",
                                                 "tonnes.now2","c1",'c2',
                                                 'c3','tonnes.init1',
                                                 'tonnes.init2','d1'))
  tonnes.now = as.numeric(paste0(line.split$tonnes.now1,'.',line.split$tonnes.now2))
  tonnes.init = as.numeric(paste0(line.split$tonnes.init1,'.',line.split$tonnes.init2))
  
  vir.biomass = data.frame(code = line.split$code, biomass.now = tonnes.now, biomass.init = tonnes.init)
  vir.biomass$scalar = vir.biomass$biomass.now/vir.biomass$biomass.init
  write.csv(vir.biomass, file = paste0(out.dir,'Diagnostic_virgin_biomass_scalar.csv'),row.names = T)
  
  }