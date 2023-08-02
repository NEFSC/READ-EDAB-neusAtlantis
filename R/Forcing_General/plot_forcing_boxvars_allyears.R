# Function that plots any box-level variable for an arbitrary length
library(dplyr)

# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/'
# out.dir = 'C:/Users/joseph.caracppa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/boxvars_allyears/'
# file.name = 'temperature_allyears.R'
# var.name = 'temperature'
# units = 'deg C'
# plot.name = 'temperature_allyears'
# smooth.length = 30

plot_roms_boxvars_allyears = function(force.dir,out.dir,file.name,var.name,units,plot.name,smooth.length){
  
  load(paste0(force.dir,file.name))
  var.dims = dim(full.data)
  
  #Convert full.time into dates
  full.date = as.POSIXct(full.time,origin = '1964-01-01 00:00:00',tz = 'UTC')
  
  #Dimensions (boxes,levels,time)
  boxes = 0:(var.dims[2]-1)
  levels = 1:var.dims[1]
  nt = var.dims[3]
  
  
  pdf(paste0(out.dir,var.name,plot.name,'.pdf'),width = 16, height = 5, onefile = T)
  for(bx in 1:length(boxes)){
    
    box.var = as.data.frame(t(full.data[,bx,]))
    colnames(box.var) = paste0('L',levels)
    box.var$date = full.date
    box.var2 = reshape2::melt(box.var,id.vars = 'date') %>%
      filter(variable != 'L5') %>%
      group_by(variable) %>%
      mutate(value.smooth = zoo::rollmean(value,smooth.length,na.pad = T))
    
    plot.box = ggplot2::ggplot(data = box.var2,ggplot2::aes(x=date, y=value.smooth, col = variable))+
    # ggplot2::ggplot(data = box.var2, ggplot2::aes(x=date,y = zoo::rollmean(value,365,na.pad = T),col = variable))+
      ggplot2::geom_line()+
      ggplot2::ylab(paste0(var.name,' (',units,')'))+
      ggplot2::xlab('')+
      ggplot2::scale_color_manual(name = 'Atlantis Level',values = c('red3','blue3','green3','violet','black'))+
      ggplot2::ggtitle(paste0('Box ',boxes[bx],': ',smooth.length,' day smoothing'))+
      ggplot2::theme_bw()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
    
    gridExtra::grid.arrange(plot.box)
    print(bx)
  }
  dev.off()
}

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/boxvars_allyears/'

all.vars = c('temperature','salinity')
file.name = paste0(all.vars,'_allyears.R')
units = c('deg C','psu')

for(i in 1:length(all.vars)){

  plot_roms_boxvars_allyears(force.dir = force.dir,
                             out.dir = out.dir,
                             file.name = file.name[i],
                             var.name = all.vars[i],
                             units = units[i],
                             plot.name = '_daily',
                             smooth.length = 1
                             )
  plot_roms_boxvars_allyears(force.dir = force.dir,
                             out.dir = out.dir,
                             file.name = file.name[i],
                             var.name = all.vars[i],
                             units = units[i],
                             plot.name = '_annual',
                             smooth.length = 365
  )
  print(i)
}

# force.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Forcing_Files/combined_years/'
force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Forcing_Files/Annual_Output/combined_years'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/boxvars_allyears/'

all.vars = c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
file.name = paste0(all.vars,'_allyears.R')
units = rep('mg N m-3',4)

for(i in 1:length(all.vars)){
  
  plot_roms_boxvars_allyears(force.dir = force.dir,
                             out.dir = out.dir,
                             file.name = file.name[i],
                             var.name = all.vars[i],
                             units = units[i],
                             plot.name = '_daily',
                             smooth.length = 1
  )
  
  plot_roms_boxvars_allyears(force.dir = force.dir,
                             out.dir = out.dir,
                             file.name = file.name[i],
                             var.name = all.vars[i],
                             units = units[i],
                             plot.name = '_annual',
                             smooth.length = 365
  )
  
  print(i)
}
