library("atlantistools")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")

#prefix for output file names
runfile='neus_output'
# runfile = 'atneus_v15_01272020'

git.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/'
#Input file directory where (PRM, bgm, group data) are saved
d1=paste0(git.dir,'currentVersion')
# d1 = 'C:/Users/joseph.caracappa/Documents/Branch Backup/neus-atlantis'
#Output file directory (.nc and .txt files)
d2=paste0(git.dir,'currentVersion/Output')
# d2 = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/'
#R scripts directory (where post-processing code is)
d3 = paste0(git.dir,'R/')
# d3 = 'C:/Users/joseph.caracappa/Documents/Branch Bacup/neus-atlantis/R'
#Location of Diagnostic Figures/tables
d4 = paste0(git.dir,'currentVersion/Diagnostics/')

  
#choose plots: spatial plots visualize group biomass across, time, boxes, and layers
make_spatial_plots=T
#diet plots show top predators and prey for all groups
make_diet_pdf=T
#LTL plots show timeseries of biomass for LTL groups
make_LTL_plots=T

#Choose for diagnostic focal box/layer
tb=17 # choose box
ll=4 # choose layer (4 is bottom for NEUS)

#If catch was included in run:
include_catch=T

#check initial conditions scaling of biomass?
check_scale_init=T

setwd(d2)
#preprocessing code: creates runfile_prepro.rdata
source(paste0(d3,'RM_preprocess_v2.R'))
#creates diagnostic figures
source(paste0(d3,'RM_atl_model_calibration.R'))

# save.image(paste(d2,"/ws.RData", sep='')) # done at end of preprocess_v2

## use to call this script in bat file for windows:
# cd C:\Users\ryan.morse\Documents\R\R-3.4.2.1\bin
# Rscript -e "source(''C:/Users/ryan.morse/Documents/GitHub/atneus_RM/R/RM_preprocess_v1.R')"