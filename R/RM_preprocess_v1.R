library("atlantistools")
library("ggplot2")
library("gridExtra")
library("dplyr")
library("tidyr")

# d1="C:/Users/ryan.morse/Documents/GitHub/atneus_RM_20180827"
# d1='/home/ryan/Git/atneus737e3d' # for NEUS 1.0 on new code base RM

d1='/home/ryan/Git/atneus_RM'
d2='/home/ryan/AtlRuns/20190115a'

d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved
d2='E:/AtlantisRun/20161103/tes/20181218dta'


setwd(d2)
source(paste(d1,'/R/RM_preprocess_v2.R', sep=''))
source(paste(d1,'/R/RM_atl_model_calibration.R', sep=''))

save.image(paste(d1,"/ws.RData", sep=''))
