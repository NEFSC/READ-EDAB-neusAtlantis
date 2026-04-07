library(dplyr)
library(here)
library(atlantisdiagnostics)

sppname <- c("COD")
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

temp_df <- get_forcing_temperature(param.ls, plotFigs = F)
temp_df <- filter(temp_df, time > 57)

# Specify the number of rows and columns
num_rows <- 22
num_cols <- 3

# Define the column names
col_names <- c("Box", "Min", "Max")

# Create a data frame with specified dimensions and column names
minmaxtemp_df <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
colnames(minmaxtemp_df) <- col_names

overallMin <- 100
overallMax <- -100
for (p in 1:22) {
  minmaxtemp_df[p,1] <- p
  polygon_df <- filter(temp_df,polygon == p)
  minmaxtemp_df$Min[p] <- min(polygon_df$atoutput)
  if (minmaxtemp_df$Min[p] < overallMin) {
    overallMin <- minmaxtemp_df$Min[p]
  }
  minmaxtemp_df$Max[p] <- max(polygon_df$atoutput)
  if (minmaxtemp_df$Max[p] > overallMax) {
    overallMax <- minmaxtemp_df$Max[p]
  }
}

threshold.dir <- '/home/rgamble/Neus-Atlantis/DisMAP/envThresh/thresholds/atlantis_group_thresholds_survdat.csv'
threshold_df <- read.table(threshold.dir, sep = ",", header = TRUE, stringsAsFactors = FALSE)

spp_threshold_info <- filter(threshold_df,Code == sppname)
spp_min_temp <- spp_threshold_info$min_bottom_temp
spp_max_temp <- spp_threshold_info$max_bottom_temp

############### END END END END ##########################

test_df <- get_habitable_boxes(
  param.ls,
  speciesCodes = sppname,
  timeFrame = -1
)

test_layers_df <- rename(test_df, Box = habitableBoxes)
test_layers_df <- inner_join(test_layers_df, summary_ref_df, by="Box")
# Remove boxes from test_df that aren't in the reference spatial extent

test_boxes_df <- distinct(select(test_layers_df, c(group,Box)))
test_boxes_df <- arrange(test_boxes_df,Box)

expected_layers <- nrow(test_layers_df)
expected_boxes <- nrow(test_boxes_df)


param.ls <- atlantisdiagnostics::get_atl_paramfiles(
  param.dir,
  run.dir,
  run.prefix,
  include_catch = F
)

habitable_df <- get_habitable_boxes(
  param.ls,
  speciesCodes = sppnames,
  timeFrame = -1
)
habitable_df <- distinct(select(habitable_df, c(group,habitableBoxes)))
habitable_df <- arrange(habitable_df,habitableBoxes)



reference_box_df <- read.table(reference.dir, sep = "", header = TRUE, stringsAsFactors = FALSE)
compare_df <- read.table(reference.dir, sep = "", header = TRUE, stringsAsFactors = FALSE)

ref_df <- filter(reference_box_df, Box > 0 & Box <= 22)
comp_df <- filter(compare_df, Box > 0 & Box <= 22)

ref_df <- filter(ref_df, Time >= 16790)
summary_ref_df <- group_by(ref_df, Box)
summary_ref_df <- summarise(summary_ref_df,across(everything(), sum))

summary_total_df <- summarise(ref_df,across(everything(), sum))

num_ref_rows <- nrow(summary_ref_df)

ref_pct_df <- summary_ref_df

for (r in 1:num_ref_rows) {

  ref_pct_df[r,3:88] <- summary_ref_df[r,3:88] / summary_total_df[1,3:88]

}

comp_df <- filter(ref_df, Time >= 20805)
summary_comp_df <- group_by(comp_df, Box)
summary_comp_df <- summarise(summary_comp_df,across(everything(), sum))

summary_total_comp_df <- summarise(comp_df,across(everything(), sum))

num_comp_rows <- nrow(summary_comp_df)

comp_pct_df <- summary_comp_df

for (r in 1:num_comp_rows) {

  comp_pct_df[r,3:88] <- summary_comp_df[r,3:88] / summary_total_comp_df[1,3:88]

}

reference_pa_df <- ref_df / ref_df
reference_pa_df[is.na(reference_pa_df)] <- 0
