# Function that plots any box-level variable for an arbitrary length

plot_roms_boxvars_allyears = function(roms.dir,out.dir,file.name,var.name,units,plot.name,smooth.length){
  
  load(paste0(roms.dir,file.name))
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
    box.var2 = reshape2::melt(box.var,id.vars = 'date')
    
    # box.var2 = na.omit(box.var2)
    # plot.box = ggplot2::ggplot(data = box.var2,ggplot2::aes(x=date, y=zoo::rollmean(value,smooth.length,na.pad = T), col = variable))+
    plot.box = ggplot2::ggplot(data = box.var2,ggplot2::aes(x=date, y=value, col = variable))+
    # ggplot2::ggplot(data = box.var2, ggplot2::aes(x=date,y = zoo::rollmean(value,365,na.pad = T),col = variable))+
      # ggplot2::geom_line()+
      ggplot2::ylab(paste0(var.name,' (',units,')'))+
      ggplot2::xlab('')+
      ggplot2::scale_color_manual(name = 'Atlantis Level',values = c('red3','blue3','green3','violet'))+
      ggplot2::ggtitle(paste0('Box ',boxes[bx]))+
      ggplot2::theme_bw()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position ='bottom',legend.box = 'horizontal')
    
    gridExtra::grid.arrange(plot.box)
    print(bx)
  }
  dev.off()
}

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/New_Levels_Output/combined_years/'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Allyears_Summary_NewAgg/'

roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT_Output/combined_years/'
out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Allyears_Summary_NewAgg/'

all.vars = c('temperature','Salinity',
             'ndi','nlg','nlgz','nmdz','nsm','nsmz','silg','nbact',
             'nh4','no3','o2','silg'
             )
file.name = paste0(all.vars,'_allyears.R')
units = c('deg C','psu',
          rep('mg N m-3',6),'mg Si m-3','mg N m-3',
          rep('mg N m-3',2), 'mg O2 m-3','mg Si m-3')

# all.vars = c('temperature')
# file.name = paste0(all.vars,'_allyears.R')
# units = c('deg C')


for(i in 1:length(all.vars)){
  plot_roms_boxvars_allyears(roms.dir = roms.dir,
                             out.dir = out.dir,
                             file.name = file.name[i],
                             var.name = all.vars[i],
                             units = units[i],
                             plot.name = ' original',
                             smooth.length = 1
                             )
  print(i)
}


plot_roms_boxvars_allyears(roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/combined_debias_temp/',
                           out.dir ='C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Debiased_Temperature/',
                           file.name = 'temperature_allyears.R',
                           var.name = 'temperature',
                           units = 'deg C',
                           plot.name = 'NCEI NWA Debiased',
                           smooth.length = 1)
