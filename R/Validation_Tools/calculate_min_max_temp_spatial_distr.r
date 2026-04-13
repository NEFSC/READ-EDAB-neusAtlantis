library(dplyr)
library(here)
library(atlantisdiagnostics)

get_minmax_thresh <- function(sppname) {
  
  #  biofile <- here::here('currentVersion', '/at_biology.prm')
  threshold_file <- here::here('data', '/NRHA_Thresholds.csv')
  threshold_df <- read.table(threshold_file, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  spp_threshold_info <- filter(threshold_df,Code == sppname)
  if (nrow(spp_threshold_info > 0)) {
    spp_min_temp <- spp_threshold_info$min_bottom_temp
    spp_max_temp <- spp_threshold_info$max_bottom_temp
    minmax_temp <- c(spp_min_temp, spp_max_temp)
  } else {
    minmax_temp <- c(0,30)
  }
  return(minmax_temp)
}


get_box_layer_df <- function() {
  
  box_depth <- c(1,1,1,1,2,2,2,2,2,2,3,2,1,3,2,3,2,2,3,3,3,2)
  num_rows <- 45
  num_cols <- 4
  
  # Define the column names
  col_names <- c("Box", "Layer", "Min", "Max")
  
  # Create a data frame with specified dimensions and column names
  box_layer_df <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  colnames(box_layer_df) <- col_names
  r <- 0
  for (b in 1: 22) {
    num_layers<- box_depth[b]
    for (l in 1:num_layers) {
      r <- r + 1
      box_layer_df$Box[r] <- b
      box_layer_df$Layer[r] <- l
      box_layer_df$Min[r] <- -100
      box_layer_df$Max[r] <- 100
    }
  }
  return(box_layer_df)
}

sppname <- "HAL"
reference.dir <- here::here('Atlantis_Runs', 'dev_7_22_25','/neus_outputBoxBiomass.txt')
reference_box_df <- read.table(reference.dir, sep = "", header = TRUE, stringsAsFactors = FALSE)
ref_df <- filter(reference_box_df, Box > 0 & Box <= 22)

ref_df <- filter(ref_df, Time >= 16790)
summary_ref_df <- group_by(ref_df, Box)
summary_ref_df <- summarise(summary_ref_df,across(everything(), sum))
summary_ref_df[summary_ref_df < 0.01] <- 0


refname <- "dev_7_22_25"
run.prefix <- "neus_output"
test.dir = here::here("Atlantis_Runs", refname)
param.dir = here::here("currentVersion")

param.ls <- atlantisdiagnostics::get_atl_paramfiles(
  param.dir,
  test.dir,
  run.prefix,
  include_catch = F
)

vdist <- get_param_vert(param.ls$biol.prm)|> dplyr::filter(value > 1e-5)
#vdist <- select(vdist,group,layer,value)
vdist <- select(vdist,group,layer)
vdist <- distinct(vdist)
vdist <- rename(vdist, Code = group)
sppcodes <- unique(vdist$Code)

temp_df <- get_forcing_temperature(param.ls, plotFigs = F)
temp_df <- filter(temp_df, time > 57 & layer != 4)

box_layer_min_max_df <- get_box_layer_df()

# Generate full box/layer dataframe with min and max temperatures for each box/layer
# combination that shows the minimum and maximum values at which we can still achieve
# the same distribution we actually observe.

numspp <- length(sppcodes)
for (s in 1:numspp) {
  sppname <- sppcodes[s]
  print(sppname)
  minmax_vector <- get_minmax_thresh(sppname)
  spp_min <- minmax_vector[1]
  spp_max <- minmax_vector[2]
  vdist_spp <- filter(vdist, Code == sppname)
  summary_spp_names <- colnames(summary_ref_df)
  if (sppname %in% summary_spp_names) {
    spp_ref_df <- select(summary_ref_df, c("Box",sppname))
    spp_ref_df <- filter(summary_ref_df, summary_ref_df[sppname] > 0)
    spp_ref_df <- select(spp_ref_df, c("Box"))
    numBoxes <- nrow(spp_ref_df)
    for (b in 1:numBoxes) {
      box <- spp_ref_df$Box[b]
      numLayers <- nrow(vdist_spp)
      for (l in 1:numLayers) {
        layer <- vdist_spp$layer[l]
        numRows <- nrow(box_layer_min_max_df)
        for (r in 1:numRows) {
          if ((box_layer_min_max_df$Box[r] == box) && (box_layer_min_max_df$Layer[r] == layer)) {
            if (spp_min > box_layer_min_max_df$Min[r]) {
              box_layer_min_max_df$Min[r] <- spp_min
            }
            if (spp_max < box_layer_min_max_df$Max[r]) {
              box_layer_min_max_df$Max[r] <- spp_max
            }
          }
        }
      }
    }
  }
}
