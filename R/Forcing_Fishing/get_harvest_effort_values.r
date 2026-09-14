#'
#'
#'
#'
#'
#'
#'

get_harvest_effort_values <- function(fileName="at_harvest.prm",fleet="dredgeSCA",species="SCA"){
  
  file <- here::here("currentVersion",fileName)
  ifile=scan(file,what="character",flush=T,blank.lines.skip=F,quiet=T,comment.char="#")
  # read in input file
  content <- readLines(con = file)
  
  # scan contents for effort params related to fleet or species
  # fleet_sweptarea
  # fleet_Effort
  # Effort_vdistribfleet
  # Effort_hdistribfleetd
  # flagimposecatch_species
  # flagincidmort
  # target_fleet
  # flag_access_thru_wc_species
  
  
  for (iline in 1:nrow(content)){
    
    
  }
  
  
}