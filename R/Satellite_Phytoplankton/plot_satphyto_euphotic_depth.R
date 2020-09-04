#Script to pull and summarize euphotic depth from OCCCI data
library(dplyr)
library(ggplot2)

source(here::here('R','fill_satphyto_gaps.R'))

satphyto.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
years = 1997:2019
file.prefix = 'D8-OCCCI-ATLANTIS_'

y=1
zeu.year.ls = list()

#Loop over years, fill gaps, and output to list
for(y in 1:length(years)){
  year.dates = seq.Date(as.Date(paste0(years[y],'-01-01')),as.Date(paste0(years[y],'-12-31')),by = 1)
  date.index = data.frame(mid = rep(year.dates,each = 30),doy = rep(1:length(year.dates),each = 30), SUBAREA = rep(0:29,length(year.dates)))
  # date.index = data.frame(mid = year.dates,doy = 1:length(year.dates))
  
  dat = read.csv(paste0(satphyto.dir,file.prefix,years[y],'.csv'),header = T,as.is = T) %>% 
    filter(PROD == 'ZEU') %>%
    select(mid,mid.year,SUBAREA,PROD,MED) %>%
    mutate(mid = as.Date(mid)) %>%
    right_join(date.index)
  colnames(dat) = c('date','ref.year','box','variable','values','doy')
  dat$ref.year =  format(dat$date, format = '%Y')
  
  zeu.year.ls[[y]] = dat
  # dat2 = reshape2::dcast(dat, mid~SUBAREA,value.var = 'MED')
}

zeu.year = bind_rows(zeu.year.ls)


zeu.year = fill_satphyto_gaps(input.mat = zeu.year,
                   var.name = 'ZEU',
                   doy.file = NA,
                   max.interp = 100,
                   write.gaps = F)

zeu.year = zeu.year %>%
  filter(date >= as.Date('1997-09-07') & date <= as.Date('2019-12-27'))

save(zeu.year, file = paste0(satphyto.dir,'euphotic_depth_allyears.R'))
#Plot by box
boxes = 0:29

pdf('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/Euphotic_Depth_Box.pdf',width =  16, height = 6,onefile = T)

for(b in 1:length(boxes)){
  zeu.box = zeu.year %>% filter(box == boxes[b])
  deep.pct = round(100*length(which(zeu.box$values> 50))/nrow(zeu.box),1)
  zeu.med = median(zeu.box$values,na.rm=T)
  
  fig = ggplot(data = zeu.box,aes(x=date,y = values))+
    geom_path()+
    geom_hline(yintercept = 50,lty = 2,size = 1.2)+
    geom_hline(yintercept = zeu.med,lty = 2, col = 'red',size = 1.5)+
    geom_text(x = -Inf, y = Inf,label = paste0('Days >50m = ',deep.pct,'%'),hjust = -0.2,vjust = 1.5)+
    ggtitle(paste0('Box ',boxes[b]))+
    xlab('')+
    ylab('Euphotic Depth (m)')+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
    
  gridExtra::grid.arrange(fig)
  print(zeu.med)
}
dev.off()
