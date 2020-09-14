#Create comparisons of COBALT box-level diagnostics and Satellite_Phyto Climatology
library(ncdf4)
library(dplyr)
library(ggplot2)

cobalt.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/combined_years/'
satphyto.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/'

fig.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/COBALT_comparison/'
#Load satphyto climatology
satphyto.nc = nc_open(paste0(satphyto.dir,'Phyto_Climatology.nc'))

var.names = names(satphyto.nc$var)

#Loop over all phyto variables and plot comparisons

for(v in 1:length(var.names)){
  
  #Load COBALT variable and determine COBALT dates
  load(paste0(cobalt.dir,var.names[v],'_allyears.R'))
  cobalt.dates = as.Date(as.POSIXct(full.time,origin = '1964-01-01 00:00:00',tz = 'UTC'))
  
  #Match climatology dates
  climatology.dates = seq.Date(as.Date('1998-01-01'),max(cobalt.dates),by = 'days')
  date.match = which(cobalt.dates %in% climatology.dates)
  
  #convert cobalt data to DOY medians
  full.data = full.data[,,date.match]
  full.time = full.time[date.match]
  
  #loop through years and add date and DOY in long format. 
  #Aggregated over all layers since all PP in top layer in satellite data
  years = unique(format(climatology.dates, format = '%Y'))
  cobalt.var.ls = list()
  for(y in 1:length(years)){
    
    year.match = which(format(climatology.dates,format = '%Y')==years[y])
    dat.year = full.data[,,year.match]
    dat.year.sum =apply(dat.year,c(2,3),sum,na.rm=T)
    dat.year.df =data.frame(box = rep(1:30,length(year.match)),
               doy = rep(1:length(year.match),each = 30),
               values = c(dat.year.sum))
    dat.year.df$date = climatology.dates[year.match]
    cobalt.var.ls[[y]] = dat.year.df
  }
  cobalt.var = bind_rows(cobalt.var.ls)
  
  #Aggreage cobalt.var over DOY (median) to get climatology comparison
  #Remove leap day (366)
  cobalt.doy = cobalt.var %>%
    group_by(box,doy) %>%
    summarize(values = median(values,na.rm=T)) %>%
    filter(doy != 366)
  cobalt.doy$model = 'COBALT'
  #Load satphyto data
  satphyto.var = ncvar_get(satphyto.nc,var.names[v])
  
  #Loop over boxes and create figure
  boxes = 0:29
  
  pdf(paste0(fig.dir,var.names[v],'_sat_cobalt.pdf'),width = 16, height = 6, onefile = T)
  for(b in 1:length(boxes)){
    
    #Format Sat phyto box into DF and combine with cobalt.doy
    satphyto.df = data.frame(box = boxes[b],doy = 1:365,values = satphyto.var[1,b,],model = 'Sat_Phyto')
    
    #Subset cobalt
    cobalt.df = cobalt.doy %>% filter(box == boxes[b])
    
    #Combine datasets
    both.df = bind_rows(satphyto.df,cobalt.df)
    
    #make plot
    fig = ggplot(data = both.df, aes(x=doy,y = values,col = model))+
      geom_line()+
      ggtitle(paste0('Box ',boxes[b]))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))
    gridExtra::grid.arrange(fig)
  }
  dev.off()
}