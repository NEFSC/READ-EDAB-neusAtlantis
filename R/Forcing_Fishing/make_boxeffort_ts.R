#' Create all box specific effort files + new catch
#'
#' Assumes starts at zero effort for all fleets 


boxes <- c(0:29)
nYrs <- 58
startYear <- 1964
t = 1:(365*nYrs)
source(here::here("R/Forcing_Fishing/scale_forcing_ts.r"))
source(here::here("R/Forcing_Fishing/create_effort_ts.r"))
source(here::here("R/Forcing_Fishing/replace_forcing_ts.r"))
source(here::here("R/Forcing_Fishing/edit_forcing_ts.r"))

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

create_boxeffort_ts(boxes)

grid <- expand.grid(Day = 1:365,Year=1964:(1964+nYrs-1))


# read in GROUNDFISH effort data and write effort to appropriate box file

gfeffortData <- readRDS(here::here("data-raw/data/groundfishFleetData.rds"))$effort
ports <- gfeffortData |>
  dplyr::distinct(newport) |>
  dplyr::mutate(newportcollapse = gsub("\\s+","",newport)) |>
  dplyr::mutate(fleet=paste0("gf",tolower(newportcollapse))) 

for (ibox in boxes) {
  fboxname <- paste0("effort_box",ibox)
  message(fboxname)
  for (iport in 1:nrow(ports)) {
    aport <- ports$newport[iport]
    afleet <- ports$fleet[iport]
    message(afleet)
    
    # extract box, port data
    boxPortData <- gfeffortData |>
      dplyr::filter(Box == ibox,newport == aport) |>
      dplyr::select(Year,effort)
    
    ## for annual data, fill in missing years with zeros and then distribute annual effort over year
    useEffort <- grid |>
      dplyr::left_join(boxPortData, by ="Year") |>
      dplyr::mutate(effort = dplyr::case_when(is.na(effort) ~ 0,
                                              .default = effort)) |>
      dplyr::mutate(effort = round(effort/365,digits = 5)) 
    
    ## Edit the ts file
    neweff <- replace_forcing_ts(afleet,tstype="effort",tseries=useEffort$effort,filename=fboxname,keep=F)
  }
  
}

# read in SCALLOP effort data and write effort to appropriate box file
SCAeffortData <- readRDS(here::here("data-raw/data/scallopFleetData.rds"))$effort
ports <- SCAeffortData |>
  dplyr::distinct(newport) |>
  dplyr::mutate(newportcollapse = gsub("\\s+","",newport)) |>
  dplyr::mutate(fleet=paste0("SCA",tolower(newportcollapse))) 

for (ibox in boxes) {
  fboxname <- paste0("effort_box",ibox)
  message(fboxname)

  for (iport in 1:nrow(ports)) {
    aport <- ports$newport[iport]
    afleet <- ports$fleet[iport]
    message(afleet)
    
    # extract box, port data
    boxPortData <- SCAeffortData |>
      dplyr::filter(Box == ibox,newport == aport) |>
      dplyr::select(Year,effort)
    
    ## for annual data, fill in missing years with zeros and then distribute annual effort over year
    useEffort <- grid |>
      dplyr::left_join(boxPortData, by ="Year") |>
      dplyr::mutate(effort = dplyr::case_when(is.na(effort) ~ 0,
                                              .default = effort)) |>
      dplyr::mutate(effort = round(effort/365,digits = 5)) 
    
    ## Edit the ts file
    neweff <- replace_forcing_ts(afleet,tstype="effort",tseries=useEffort$effort,filename=fboxname,keep=F)
  }
  
}

## Edit the forced catch
# All species need to have forced catch up to 1996.
# Then from 1996 - present SCA and groundfish Species need to have 0 catch since they are now driven by effort

gfCodes <- readRDS(here::here("data-raw/data/groundfishFleetData.rds"))$landings |>
  dplyr::filter(Code != "NA") |>
  dplyr::distinct(Code) |>
  dplyr::pull()

# create df to use to sub into catch
df <- data.frame(t = ((1+(1996-1964)*365):((2022-1964)*365)), value = 0)
newcatch <- edit_forcing_ts(gfCodes,tstype="catch",trange=df,filename="total_catch2",keep=F)


