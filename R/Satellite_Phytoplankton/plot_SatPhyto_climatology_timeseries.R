#Script to generate timeseries from SatPhyto Climatology File on Atlantis Grid

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ncdf4)

fig.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/box_timeseries/'

phyto.nc = nc_open('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/Phyto_Climatology.nc')
phyto.names = names(phyto.nc$var)

for( v in 1:length(phyto.names)){
  
  dat = ncvar_get(phyto.nc,phyto.names[v])
  units = ncatt_get(phyto.nc,phyto.names[v],'units')$value
  dates = as.POSIXct(phyto.nc$dim$t$vals,origin = '1964-01-01 00:00:00',tz = 'UTC')
  
  boxes = 0:29
  
  #plot each box into one file for each variable
  pdf(paste0(fig.dir,phyto.names[v],'_climatology_box.pdf'),width = 14, height = 5)
  for(b in 1:length(boxes)){
    DF = data.frame(value = dat[1,b,], date = dates)
    fig = ggplot(DF,aes(x = date,y = value))+
      geom_line()+
      ggtitle(paste0('Box ',boxes[b]))+
      xlab('')+
      ylab(paste0(phyto.names[v],' ',units))+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))
    grid.arrange(fig)
  }
  dev.off()
  
}