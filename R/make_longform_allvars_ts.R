# Script to pull together, format, and write a single long-form data table of forcing variables
library(dplyr)
library(ggplot2)
#Read in all forcing variables that are on the Atlantis box level
data.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/'
force.vars = c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Lab_Det_N','Diatom_S','salinity','temperature','verticalflux')
# surf.var.flag = c(T,T,T,T,T,F,F,F)
var.units = c(rep('mg N m-3',4),'mg Si m-3','psu','deg C','m3 s-1')

force.vars.df.ls = list()
#Loop over forcing variables, reformat, and write to list

boxes = 0:29
v=1
for(v in 1:length(force.vars)){
  
  #load in data
  load(paste0(data.dir,force.vars[v],'_allyears.R'))
  
  nlev = dim(full.data)[1]
  
  full.dates = as.POSIXct(full.time,origin = '1964-01-01 00:00:00',tz = 'UTC')
  box.data.ls = list()
  #loop over boxes
  for(b in 1:length(boxes)){
    box.data.ls[[b]] = data.frame(time = rep(full.time,each = nlev),
                                  date = rep(full.dates,each = nlev),
                         box = boxes[b],
                         level = rep(1:nlev,length(full.time)),
                         variable = force.vars[v],
                         units = var.units[v],
                         values = c(full.data[,b,]),
                         stringsAsFactors = F
    )
  }
  #combine boxes
  var.dat = dplyr::bind_rows(box.data.ls)
  #remove deeper layers if surface variable
  # if(surf.var.flag[v]){
  #   var.dat = var.dat %>%
  #     filter(level == 1)
  # }
  
  #write to master list
  force.vars.df.ls[[v]] = var.dat
  names(force.vars.df.ls)[v] = force.vars[v]
  
  #Plot timeseries
  # pdf(paste0(data.dir,force.vars[v],'_full_timeseries.pdf'),width = 16, height = 6, onefile = T)
  # for(b in 1:length(boxes)){
  #   dat = var.dat %>% filter(box == boxes[b])
  #   dat$level = factor(dat$level)
  #   
  #   fig = ggplot(data = dat, aes(x = date, y = values,col = level))+
  #     geom_line()+
  #     ggtitle(paste0('Box ',boxes[b]))+
  #     xlab('')+
  #     ylab(paste0(force.vars[v],' (',var.units[v],')'))+
  #     theme_bw()+
  #     theme(plot.title = element_text(hjust = 0.5))
  #   gridExtra::grid.arrange(fig)
  # }
  # dev.off()

}

force.vars.df = bind_rows(force.vars.df.ls)
save('force.vars.df', file=paste0(data.dir,'All_Forcing_Variables_Longform.R'))
# write.csv(force.vars.df,paste0(data.dir,'All_Forcing_Variables_Longform.csv'),row.names =F)

# load(paste0(data.dir,'All_Forcing_Variables_Longform.R'))
#plot(temp vs diatom)
# PL = force.vars.df %>% filter(variable == 'Diatom_N') %>% select(time,values) %>% rename(PL = 'values')
# temp = force.vars.df %>% filter(variable == 'temperature' & level == 1) %>% select(time,values) %>% rename(temp = 'values')
# temp.PL = cbind(temp,PL =PL$PL)
# 
# 
# ggplot(temp.PL, aes(x = temp, y = PL)) +
#   geom_point()
  # ggsave(paste0(data.dir,test.n)
