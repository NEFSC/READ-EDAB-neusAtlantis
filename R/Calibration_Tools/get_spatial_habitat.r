library(dplyr)
library(here)
library(atlantisdiagnostics)

get_minmax_thresh <- function(sppname) {

#  biofile <- here::here('currentVersion', '/at_biology.prm')
  threshold_file <- here::here('data', '/NRHA_Thresholds.csv')
  threshold_df <- read.table(threshold_file, sep = ",", header = TRUE, stringsAsFactors = FALSE)

  spp_threshold_info <- filter(threshold_df,Code == sppname)
  spp_min_temp <- spp_threshold_info$min_bottom_temp
  spp_max_temp <- spp_threshold_info$max_bottom_temp
  minmax_temp <- c(spp_min_temp, spp_max_temp)
  return(minmax_temp)
}


sppname <- c("HAL")
sppnames <- c("COD")
reference.dir <- here::here('Atlantis_Runs', 'dev_7_22_25','/neus_outputBoxBiomass.txt')
reference_box_df <- read.table(reference.dir, sep = "", header = TRUE, stringsAsFactors = FALSE)
ref_df <- filter(reference_box_df, Box > 0 & Box <= 22)

ref_df <- filter(ref_df, Time >= 16790)
summary_ref_df <- group_by(ref_df, Box)
summary_ref_df <- summarise(summary_ref_df,across(everything(), sum))
summary_ref_df[summary_ref_df < 0.01] <- 0

summary_ref_df <- select(summary_ref_df, c("Box",sppname))
summary_ref_df <- filter(summary_ref_df, summary_ref_df[[sppname]] > 0)
summary_ref_df <- select(summary_ref_df, c("Box"))
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
vdist <- select(vdist,group,layer,value)
vdist <- distinct(vdist)
vdist <- rename(vdist, Code = group)

temp_df <- get_forcing_temperature(param.ls, plotFigs = F)
temp_df <- filter(temp_df, time > 57 & layer != 4)

# Specify the number of rows and columns
num_rows <- 22
num_cols <- 3

# Define the column names
col_names <- c("Box", "Min", "Max")

# Create a data frame with specified dimensions and column names
minmaxtemp_df <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
colnames(minmaxtemp_df) <- col_names

for (b in 1:22) {
  minmaxtemp_df$Box[b] <- b
}

spp_minmaxtemp_df <- inner_join(minmaxtemp_df,summary_ref_df, by = c('Box'))

for (p in 1:22) {
  overallMin <- 100
  overallMax <- -100
  spp_minmaxtemp_df[p,1] <- p
  polygon_df <- filter(temp_df,polygon == p)
  spp_minmaxtemp_df$Min[p] <- min(polygon_df$atoutput)
  if (spp_minmaxtemp_df$Min[p] < overallMin) {
    overallMin <- spp_minmaxtemp_df$Min[p]
  }
  spp_minmaxtemp_df$Max[p] <- max(polygon_df$atoutput)
  if (spp_minmaxtemp_df$Max[p] > overallMax) {
    overallMax <- spp_minmaxtemp_df$Max[p]
  }
}

minmax_vector <- get_minmax_thresh(sppname)

spp_min <- minmax_vector[1]
spp_max <- minmax_vector[2]

final_output <- mutate(spp_minmaxtemp_df, habitable = "0")
nboxes <- nrow(final_output)
for (i in 1: nboxes) {
     if (spp_minmaxtemp_df$Min[i] < spp_min) {
         final_output$habitable[i] <- -1
     }
     if (spp_minmaxtemp_df$Max[i] > spp_max) {
         final_output$habitable[i] <- 1
     }
     if ((spp_minmaxtemp_df$Min[i] < spp_min) && (spp_minmaxtemp_df$Max[i] > spp_max)) {
         final_output$habitable[i] <- 99
       }
   }
unsuitable_boxes <- filter(final_output, habitable !=0)
print(paste0('Species = ', sppname, " : Min = ", spp_min, " : Max = ", spp_max))
print(unsuitable_boxes)

unsuitable_boxes <- filter(final_output, habitable !=0)
print(paste0('Species = ', sppname, " : Min = ", spp_min, " : Max = ", spp_max))
print(unsuitable_boxes)
print(final_output)
