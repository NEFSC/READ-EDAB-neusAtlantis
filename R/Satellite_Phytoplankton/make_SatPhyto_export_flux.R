#' This script will calculate the export flux from Dunne et al. 2005 on Atlantis boxes and export as list
#' 1) Pull Time Series of Temperature
#' 2) Pull Time Series of Chlorophyll (mg Chl m-3)
#' 3) Pull Time Series of Euphotic Depth (m)
#' 4) Pull Time Series of total production (PPD) (mg C m-2 d-1)
#' 4) Calculate particle export ratio, pe (Particle Export: Primary Production)
#' 5) Apply pe to primary production (export flux = primary production * pe)
#' 6) Convert export flux to biomass per day.

library(dplyr)
library(ggplot2)

# 1) Load Temperature Data
load('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/temperature_allyears.R')
temp.data = full.data
temp.time = full.time
temp.dates = as.Date(as.POSIXct(temp.time,origin = '1964-01-01 00:00:00',tz = 'UTC'))
rm(full.data,full.time)

# 2) Load Euphotic Depth
load('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/ZEU_allyears.R')
zeu.year = var.year %>% 
  rename(zeu = 'values') %>%
  select(-doy,-variable)
zeu.year$ref.year = as.numeric(zeu.year$ref.year)
zeu.dates = sort(unique(zeu.year$date))


# 3)Pull Total Chlorophyll
load('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/CHLOR_A_allyears.R')
chl.year = var.year %>% 
  rename(chl_a = 'values') %>%
  select(-doy,-variable)
chl.dates = sort(unique(chl.year$date))

# 4) Pull total Production
load('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/PPD_allyears.R')
prod.year = var.year %>%
  rename(prod = 'values') %>%
  select(-doy, -variable)
prod.dates = sort(unique(prod.year$date))

#truncate Temperature data to satellite timeseries (save on memory)
date.overlap = seq.Date(min(zeu.dates),max(temp.dates),by = 'day')
temp.dates.overlap = which(temp.dates %in% date.overlap)
zeu.dates.overlap = which(zeu.dates %in% date.overlap)
chl.dates.overlap = which(chl.dates %in% date.overlap)
prod.dates.overlap = which(prod.dates %in% date.overlap)

temp.data = temp.data[,,temp.dates.overlap]
temp.time = temp.time[temp.dates.overlap]
temp.dates = temp.dates[temp.dates.overlap]

zeu.year = zeu.year[which(zeu.year$date %in% zeu.dates[zeu.dates.overlap]),]
chl.year = chl.year[which(chl.year$date %in% chl.dates[chl.dates.overlap]),]
prod.year = prod.year[which(prod.year$date %in% prod.dates[prod.dates.overlap]),]

zeu.dates = zeu.dates[zeu.dates.overlap]
chl.dates = chl.dates[chl.dates.overlap]
prod.dates = prod.dates[prod.dates.overlap]

#Convert temperature to long format data table
temp.year.ls = list()
boxes = 0:29
for(b in 1:30){
  dat = temp.data[1,b,]  
  dates = temp.dates
  temp.year.ls[[b]] = data.frame(date = temp.dates,
                            ref.year = as.numeric(format(temp.dates,format = '%Y')),
                            box = boxes[b],
                            temp = temp.data[1,b,],
                            stringsAsFactors = F)
}
temp.year = bind_rows(temp.year.ls)

# 5) Calculate Particle export ratio
## pe = -0.0081*T + 0.068*ln(Chl/zeu) + 0.426

#combine data into single dataframe
full.data = zeu.year %>% 
  left_join(chl.year, by = c('date','ref.year','box')) %>%
  left_join(prod.year, by = c('date','ref.year','box')) %>%
  left_join(temp.year, b= c('date','ref.year','box')) %>%
  mutate(pe.r = -0.0081*temp + 0.0668*log(chl_a/zeu) + 0.426)

#Dunne et al. define upper and lower bounds (0.04,0.72)
full.data$pe.r.2 = sapply(full.data$pe.r, function(x){
  if( is.na(x) ){
    return(NA)
  }else if(x < 0.04){
    return(0.04)
  }else if(x > 0.72){
    return(0.72)
  }else{
    return(x)
  }
})

#Plot pe.r(2) over time by box
pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/particle_export_ratio.pdf',width = 16,height = 6, onefile = T)
boxes = 0:29
for(b in 1:length(boxes)){
  DF = full.data %>% filter(box == boxes[b])
  
  fig = ggplot(DF, aes(x=date, y = pe.r.2))+
    geom_line()+
    xlab('')+
    ylab('Particle Export Ratio')+
    ggtitle(paste0('Box ',boxes[b]))+
    expand_limits(y = c(0,NA))+
    theme_bw()+
    theme(plot.title = element_text(hjust= 0.5))
  
  gridExtra::grid.arrange(fig)
}
dev.off()

#Plot total production
pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/primary_production.pdf',width = 16,height = 6, onefile = T)
boxes = 0:29
for(b in 1:length(boxes)){
  DF = full.data %>% filter(box == boxes[b])
  
  fig = ggplot(DF, aes(x=date, y = prod))+
    geom_line()+
    xlab('')+
    ylab('Primary Production (mg C m-2 m-d')+
    ggtitle(paste0('Box ',boxes[b]))+
    expand_limits(y = c(0,NA))+
    theme_bw()+
    theme(plot.title = element_text(hjust= 0.5))
  
  gridExtra::grid.arrange(fig)
}
dev.off()

#Bring in atlantis box areas
bgm.file = rbgm::bgmfile('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Geometry/neus_tmerc_RM2.bgm')
box.data = bgm.file$boxes %>% 
  select(.bx0, area) %>%
  rename(box = '.bx0')
full.data.2 = full.data %>% 
  left_join(box.data) %>%
  mutate(
    #prod.box.c in mg C m-3 d-1
    prod.box.C = prod/50,
    #export.box.N in mg N m-3 d-1
    export.box.N = prod * pe.r.2/50/5.7,
    doy = as.numeric(format(date,format = '%j')),
    ref.year = as.numeric(format(date,format = '%Y')),
    variable = 'export_prod') %>%
  select(date,doy,ref.year,box,variable,export.box.N) %>%
  rename(values = 'export.box.N') %>%
  filter(date >= as.Date('1997-09-19'))

# first.day =full.data.2 %>% group_by(box) %>%
#   summarize(first.day = date[min(which(!is.na(values)))])
source(here::here('R','fill_satphyto_gaps.R'))

#fill gaps in data
full.data.3 = fill_satphyto_gaps(input.mat = full.data.2,
                                var.name = 'export_prod',
                                doy.file = NA,
                                max.interp = 100,
                                write.gaps = F,
                                gaps.dir = NA
                                )
  
write.csv(full.data.3,file = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/export_flux_allyears.csv',row.names = F)

