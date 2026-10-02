#Wrapper script to plot migrations from daily output runs
source(here::here('R','plot_yearlyoutput_function.R'))
# List of packages for session
.packages = c("devtools","dtplyr","stringi","data.table","tidyverse","stringr","R.utils","magrittr",
              "scales","RNetCDF","ggforce","pdftools",
              "rgdal","gridExtra", "esquisse","sf", "here")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#Run variables
run.name = 'Migration_6645_3yr_1d'
run.dir = here::here('Atlantis_Runs',run.name)
out.dir = here::here(run.dir,'Post_Processed')
fg.file <- here::here('currentVersion','neus_groups.csv')
nc.file = paste0(run.dir,'/neus_output.nc')
mig.file <- here::here('currentVersion','neus_migrations.csv')

eachgroup = 'BFT'
thisvariabletype <- "Nums"
yearsrun  <- 3
output_freqyrs <- 1
thistimestep <- (yearsrun*365)/output_freqyrs #this is max number of timesteps/73 output frequency

year_plot(run.dir, out.dir, yearsrun, output_freqyrs, thisncfile, fg.list, mig.file, thisvariabletype)
