#' Read in effort ts files and plot
#'
#'

# read in fleet file
nBoxes <- 30
a <- list()
for (ibox in 1:nBoxes) {
  boxID <- ibox-1
  filenm <- paste0("effort_box",boxID)
  a[[as.character(boxID)]] <- get_forcing_ts(filenm = filenm,time = "annual")
}

# select just groundfish fleets
df <- dplyr::bind_rows(a,.id ="Box") |> 
  dplyr::filter(grepl("gf",Variable)) |> 
  dplyr::mutate(Box = as.numeric(Box)) |> 
  dplyr::filter(Time >= 33)

p <- ggplot2::ggplot(df) +
  ggplot2::geom_line(ggplot2::aes(x = Time,y = Value, col = Variable)) +
  ggplot2::facet_wrap(~Box)
print(p)
  
# select scallop fleets
df <- dplyr::bind_rows(a,.id ="Box") |> 
  dplyr::filter(grepl("SCA",Variable))

p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = Time,y = Value, col = Variable)) +
    ggplot2::facet_wrap(~Box)
  print(p)

# read in effort data file and plot
# this should look the same
gfFleet <- readRDS(here::here("data-raw/data/groundfishFleetData.rds"))

ggplot2::ggplot(gfFleet$effort) +
  ggplot2::geom_line(ggplot2::aes(x=Year,y=effort/365,col = newport)) +
  ggplot2::facet_wrap(~Box)
