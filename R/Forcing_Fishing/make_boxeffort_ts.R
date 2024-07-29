#' Create all box specific effort files
#'
#' Assumes starts at zero effort for all fleets 

boxes <- c(0:29)
source(here::here("R/Calibration_Tools/scale_forcing_ts.r"))
source(here::here("R/Calibration_Tools/create_effort_ts.r"))

#if they dont exist create them
create_boxeffort_ts <- function(boxes) {
  for (ibox in boxes) {
    fname <- paste0("effort_box",ibox)
    if (file.exists(here::here(paste0("curentVersion/CatchFiles",fname)))) {
      next
    } else {
      create_effort_ts(filename=fname)
    }
  }
  
}

## AMMEND VALUES IN TS FILES
fleets <- paste0("dredgeSCA",c(1:3))
footprint <- list()
#footprint$dredgeSCA1 <- c(2,3,5,6,7,8,9,12,13,14,15)
#footprint$dredgeSCA2 <- c(2,3,5,6,9)
#footprint$dredgeSCA3 <- footprint$dredgeSCA2
footprint$dredgeSCA1 <- 1
footprint$dredgeSCA2 <- 4
footprint$dredgeSCA3 <- 7


for (ibox in boxes) {
  message(paste0("Box = ",ibox))
  for (ifleet in 1:length(fleets)) {
    fleetName <- fleets[ifleet]
    # fleet fishes in box then write effort
    filename = paste0("effort_box",ibox)
    print(filename)
    if (ibox %in% footprint[[fleetName]]) {
      print(c(fleetName,ibox))
      scale_forcing_ts(fleetName,tstype="effort",value = 1000,operation="add",filename=filename)
    } else {
      print(fleetName)
      # write empty file
      scale_forcing_ts(fleetName,tstype="effort",value = 0,operation="add",filename=filename)
    }
  }
  
  
  
}
