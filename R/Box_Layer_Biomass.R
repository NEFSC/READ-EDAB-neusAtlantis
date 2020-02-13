## Function to visualize the depth distribution of a given functional group across all (or specified boxes)
# library(ncdf4)
# library(ggplot2)
# library(reshape2)
# library(tidyverse)
# library(RColorBrewer)
# library(gridExtra)
# library(grid)

# run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Figures/LTL Diagnostics/'
# runfile = '02062020_PrimProdDiag_1'

layer.key = read.csv(layer.key.dir,stringsAsFactors = F)
layer.col = data.frame(layer = 1:5,color = brewer.pal(5,'Set1'))

# setwd(paste0(run.dir,runfile,'/'))

# View(names(output$var))

# nc.vars = c('Scallop_N','Lobster_N')

output = nc_open(paste0(runfile,'.nc'))

tsteps = round(output$dim$t$vals/(365*86400),2)
tstart = 0

volume = ncvar_get(output,'volume')

# plot.layers = 1:5
plot.all = list()

for(i in 1:length(nc.vars)){
  biomass = ncvar_get(output,nc.vars[i])  
  box.ls = list()
  
  for(j in 2:23){
    if(length(dim(biomass))==3){
      
      bio.box = as.data.frame(biomass[,j,]  )
      if(ncatt_get(output,nc.vars[i])$units == "mg N m-3"){
        box.vol = volume[,j,]
        bio.box = box.vol*bio.box
      }
      
      colnames(bio.box) = tsteps
      bio.box$layer = t(layer.key[j,2:6])
      bio.box = bio.box[plot.layers,]
      bio.box2 = gather(bio.box,time,biomass,as.character(tsteps[1]):as.character(tsteps[length(tsteps)]))
      bio.box2$time = as.numeric(bio.box2$time)
      bio.box2$layer = factor(bio.box2$layer)
      bio.box2$biomass[bio.box2$biomass==0] = NA
      bio.box2 = bio.box2 %>% filter(time >= tstart)
      
      plot.cols = as.character(layer.col$color[as.numeric(levels(unique(bio.box2$layer)))])
      
      box.ls[[j-1]] = ggplot(bio.box2,aes(x= time, y = biomass,col = layer))+
        geom_path(size = 0.5)+
        scale_color_manual(name = 'Depth Layer',values = plot.cols )+
        ggtitle(paste0('Box ',j-1))+xlab('Time (yr)')+ylab('Biomass')+
        theme_minimal()+
        theme(
          panel.grid = element_blank()
        )
      
      if(j-1 == 20) {box.ls[[23]] = g_legend(box.ls[[j-1]])}
      
      box.ls[[j-1]] = box.ls[[j-1]]+guides(col = F)
      
    } else if(length(dim(biomass))==2){
      if(ncatt_get(output,nc.vars[i])$units == "mg N m-3"){
        box.vol = volume[j,]
        bio.box = box.vol*bio.box
      }
      bio.box = data.frame(time = tsteps,biomass = biomass[j,])
      
      box.ls[[j-1]] = ggplot(bio.box,aes(x=time,y=biomass))+
        geom_path(size=0.5,color = as.character(layer.col$color[5]))+
        ggtitle(paste0('Box ',j-1))+xlab('Time (yr)')+ylab('Biomass')+
        theme_minimal()+
        theme(
          panel.grid = element_blank()
        )
      # if(j-1 == 20) {box.ls[[23]] = g_legend(box.ls[[j-1]])}
      
      box.ls[[j-1]] = box.ls[[j-1]]+guides(col = F)
    }
  }
  plot.all[[i]] = do.call('grid.arrange',c(box.ls,ncol = 5,top = nc.vars[i]))
  # ggsave(paste0(runfile,'_BoxLayer_',nc.vars[i],'.png'),sp.plot,width = 14, height = 14, units = 'in', dpi = 300)
}

pdf(paste0(runfile,'_Biomass_BoxLayer.pdf'),onefile = T,width = 12, height = 12)
for(k in seq_along(plot.all)){
  gridExtra::grid.arrange(plot.all[[k]])  
}
dev.off()

