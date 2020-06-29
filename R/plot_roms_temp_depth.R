library(dplyr)
library(ggplot2)

temp.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_Temp_Depth/'

#Combine all years to one file
temp.files = list.files(paste0(temp.dir,'Annual_Output/'),full.names = T)
year.temp.ls = list()
for(f in 1:length(temp.files)){
  load(temp.files[f])
  year.temp.ls[[f]] = year.temp.all
  rm(year.temp.all)
}
year.temp = dplyr::bind_rows(year.temp.ls)
rm(year.temp.ls)

save(year.temp,file = paste0(temp.dir,'ROMS_temp_depth_combined.R'))
load(paste0(temp.dir,'ROMS_temp_depth_combined.R'))

#Find Reference temps per epu per depth
ref.temps = year.temp %>% filter(year %in% 1981:2010) %>%
  group_by(epu,depth.bin) %>%
  summarize(temp = mean(temp,na.rm=T))



#Create Function to make plots with/without moving average

plot_temp_depth = function(data,epu.name,depth,move.avg,window = 365,is.anomaly){
  y.bounds = temp.bounds %>% filter(epu == epu.name)
  y.bounds = c(y.bounds$min.temp[1],y.bounds$max.temp[1])
  
  data2 = ungroup(data) %>% filter(epu == epu.name  & depth.bin == depth) %>% arrange(date)
  if(is.anomaly){
    ref.temp = (ref.temps %>% filter(epu == epu.name & depth.bin == depth))$temp
    data2$temp =  ref.temp-data2$temp 
    y.bounds = c(-2,2)
  }
  if(move.avg){
    data2$temp = zoo::rollmean(data2$temp,window,na.pad = T)
    y.bounds = temp.bounds.smooth %>% filter(epu == epu.name)
    y.bounds = c(y.bounds$min.temp[1],y.bounds$max.temp[1])
  }
  
  if(nrow(data2) == 0){
    return(NA)
  }
  
  model = lm(temp~date,data2)
  model.summ = summary(model)
  r2 = round(model.summ$r.squared,2)
  
  # if(move.avg){
    ggplot(data = data2, aes(x = date,y=temp))+
      geom_line()+
      geom_smooth(method = 'lm')+
      # annotate('text',x = min(data2$date), y = y.bounds[2], label = bquote(R^2~'='~.(r2)), hjust = 0)+
      # ylim(y.bounds)+
      ylab('Temperature (deg C)')+
      xlab('')+
      ggtitle(paste0(epu.name,' : ',depth+5,' m'))+
      theme_bw()+
      theme(plot.title= element_text(hjust = 0.5))
  # }else{
  #   ggplot(data = data2, aes(x = date,y=temp))+
  #     geom_line()+
  #     ylim(y.bounds)+
  #     ylab('Temperature (deg C)')+
  #     xlab('')+
  #     ggtitle(paste0(epu.name,' : ',depth+5,' m'))+
  #     theme_bw()+
  #     theme(plot.title= element_text(hjust = 0.5))
  # }
}

#Create PDF where temp is shown for each epu for each depth
epu.names = as.character(unique(year.temp$epu))
depth.bins = sort(unique(year.temp$depth.bin))

temp.bounds = year.temp %>% group_by(epu) %>%
  summarize(min.temp = min(temp,na.rm=T), max.temp = max(temp,na.rm=T))
temp.bounds.smooth = data.frame(epu = epu.names, min.temp = c(5,4,5,4), max.temp = c(13,10,15,10))

for(i in 1:length(epu.names)){
  
  pdf(file = paste0(temp.dir,epu.names[i],' Temperature by Depth.pdf'),onefile = T, width = 16, height = 6)
  for(z in 1:length(depth.bins)){
    p = plot_temp_depth(data = year.temp, epu.name = epu.names[i], depth = depth.bins[z],move.avg = F,is.anomaly = F)
    if(is.na(p)){next()}
    gridExtra::grid.arrange(p)
  }
  dev.off()

  pdf(file = paste0(temp.dir,epu.names[i],' Temperature by Depth - Interannual.pdf'),onefile = T, width = 16, height = 6)
  for(z in 1:length(depth.bins)){
    p = plot_temp_depth(data = year.temp, epu.name = epu.names[i], depth = depth.bins[z],move.avg = T,window = 365,is.anomaly = F)
    if(is.na(p)){next()}
    gridExtra::grid.arrange(p)
  }
  dev.off()
  
  pdf(file = paste0(temp.dir,epu.names[i],' Temperature by Depth - Anomaly.pdf'),onefile = T, width = 16, height = 6)
  for(z in 1:length(depth.bins)){
    p = plot_temp_depth(data = year.temp, epu.name = epu.names[i], depth = depth.bins[z],move.avg = F,window = 365,is.anomaly = T)
    if(is.na(p)){next()}
    gridExtra::grid.arrange(p)
  }
  dev.off()
  
  print(i)
}


for(z in 1:length(depth.bins)){
  p = plot_temp_depth(data = year.temp, epu.name = epu.names[2], depth = depth.bins[z],move.avg =T,is.anomaly = F,window = 365)
  if(is.na(p)){next()}
  png(file = paste0(temp.dir,'/GOM Images/',epu.names[2],'-',z,'.png'), width = 16, height = 6,units = 'in',res = 100)
  gridExtra::grid.arrange(p)
  dev.off()
}

#Make Gif
setwd(paste0(temp.dir,'/GOM Images/'))
unlink('GOM_Temp.gif')
system('convert -delay 10 GOM*.png GOM_Temp.gif')
