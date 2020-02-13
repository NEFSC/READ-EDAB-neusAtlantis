### Calculate nutrient and light limiting scalar for each box/layer in specified run
# library(ggplot2)
# library(dplyr)
# library(gridExtra)
# library(ncdf4)
# library(tidyverse)

# run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Figures/LTL Diagnostics/Nutrient Limitation/'
# run.name = '02052020_NoGrazing'

# setwd(paste0(run.dir,run.name,'/'))

# layer.key = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Box Layer Key.csv',stringsAsFactors = F)
# layer.col = data.frame(layer = 1:5,color = brewer.pal(5,'Set1'))

output = nc_open(paste0(runfile,'.nc'))

# View(names(output$var))

#Nutrient and light arrays
nh3 = ncvar_get(output,'NH3')
no3 = ncvar_get(output,'NO3')
si = ncvar_get(output,'Si')
light = ncvar_get(output,'Light')
biomass = ncvar_get(output,'Diatom_N')
temp = ncvar_get(output,'Temp')

# bio.file =head(read.table(paste0(param.dir,strsplit(strsplit(logfile_lines[[14]],'-b ' )[[1]][2],' ')[[1]][1]),sep = '\t'))
bio.con = file(paste0(param.dir,strsplit(strsplit(logfile_lines[[14]],'-b ' )[[1]][2],' ')[[1]][1]),'r')
bio.lines = readLines(bio.con,-1)


#growth parameters
kn = as.numeric(strsplit(str_replace(gsub('\\s+',' ',str_trim(bio.lines[which(startsWith(bio.lines,paste0('KN_',prim.prod))==T)])),'B','b'),' ')[[1]][2])
ks = as.numeric(strsplit(str_replace(gsub('\\s+',' ',str_trim(bio.lines[which(startsWith(bio.lines,paste0('KS_',prim.prod))==T)])),'B','b'),' ')[[1]][2])
ki = as.numeric(strsplit(str_replace(gsub('\\s+',' ',str_trim(bio.lines[which(startsWith(bio.lines,paste0('KI_',prim.prod))==T)])),'B','b'),' ')[[1]][2])
q10 = as.numeric(strsplit(str_replace(gsub('\\s+',' ',str_trim(bio.lines[grep(paste0('\\bq10_',prim.prod,'\\b'),bio.lines)])),'B','b'),' ')[[1]][2])
klys = as.numeric(strsplit(str_replace(gsub('\\s+',' ',str_trim(bio.lines[which(startsWith(bio.lines,paste0('KLYS_',prim.prod))==T)])),'B','b'),' ')[[1]][2])

close.connection(bio.con)
#DIN = NH3 + NO3
din = nh3 + no3

delta.n = din/(din + kn)
delta.s = si/(si+ks)
delta.l = apply(light/ki,c(1,2,3),function(x) return(min(x,1)))
delta.t = q10^((temp-15)/10)

mort = ((klys*biomass)/(delta.n+0.1))

tsteps = round(output$dim$t$vals/(365*86400),2)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

# tstart = 0
# tstop = 10

plot.layers = 1:4

plot.custom = function(dat.array,box,plot.name,mort.flag = F){
  
  if(mort.flag == T){
    dat.box = as.data.frame(dat.array[1:4,box,])
    colnames(dat.box) = tsteps
    dat.box$layer = t(layer.key[box,2:5])
  } else {
    dat.box = as.data.frame(dat.array[,box,])
    colnames(dat.box) = tsteps
    dat.box$layer = t(layer.key[box,2:6])
  }
  
  # dat.box = bio.box[plot.layers,]
  dat.box2 = gather(dat.box,time,biomass,as.character(tsteps[1]):as.character(tsteps[length(tsteps)]))
  dat.box2$time = as.numeric(dat.box2$time)
  dat.box2$layer = factor(dat.box2$layer)
  dat.box2$biomass[dat.box2$biomass==0] = NA
  dat.box2 = dat.box2 %>% filter(time >= tstart & time <=tstop & layer %in% plot.layers)
  
  plot.cols = as.character(layer.col$color[as.numeric(levels(unique(dat.box2$layer)))])
  
  g = ggplot(dat.box2,aes(x= time, y = biomass,col = layer))+
    geom_path(size = 0.5)+
    scale_color_manual(name = 'Depth Layer',values = plot.cols )+
    ggtitle(paste0('Box ',box))+xlab('Time (yr)')+ylab('Biomass')+
    ylab(plot.name)+
    theme_minimal()+
    theme(
      panel.grid = element_blank()
    )
  return(g)
}

plot.vars = c('Diatom Biomass', 'Nitrogen Limitation Scalar','Silicon Limitation Scalar','Light Limitation Scalar','Temperature Scalar','Mortality')
diatom.n.plots = list()
delta.n.plots = list()
delta.s.plots = list()
delta.l.plots = list()
delta.t.plots = list()
mort.plots = list()


for(i in 2:23 ){
  diatom.n.plots[[i-1]] = plot.custom(dat.array = biomass, box = i,plot.name = 'Diatom N')
  delta.n.plots[[i-1]] = plot.custom(dat.array = delta.n, box = i,plot.name = 'Nitrogen Limitation Scalar')
  delta.s.plots[[i-1]] = plot.custom(dat.array = delta.s, box = i,plot.name = 'Silicon Limitation Scalar')
  delta.l.plots[[i-1]] = plot.custom(dat.array = delta.l, box = i,plot.name = 'Light Limitation Scalar')
  delta.t.plots[[i-1]] = plot.custom(dat.array = delta.t, box = i,plot.name = 'Temperature Scalar')
  mort.plots[[i-1]] = plot.custom(dat.array = mort, box = i,plot.name = 'Mortality',mort.flag = T)
  
  if( (i-1)== 20){
    diatom.n.plots[[23]] = g_legend(diatom.n.plots[[i-1]])
    delta.n.plots[[23]] = g_legend(delta.n.plots[[i-1]])
    delta.s.plots[[23]] = g_legend(delta.s.plots[[i-1]])
    delta.l.plots[[23]] = g_legend(delta.l.plots[[i-1]])
    delta.t.plots[[23]] = g_legend(delta.t.plots[[i-1]])
    mort.plots[[23]] = g_legend(mort.plots[[i-1]])
  }
  
  diatom.n.plots[[i-1]] = diatom.n.plots[[i-1]] + guides(col = F)
  delta.n.plots[[i-1]] = delta.n.plots[[i-1]] + guides(col = F)
  delta.s.plots[[i-1]] =  delta.s.plots[[i-1]] + guides(col = F)
  delta.l.plots[[i-1]] =  delta.l.plots[[i-1]] + guides(col = F)
  delta.t.plots[[i-1]] =  delta.t.plots[[i-1]] + guides(col = F)
  mort.plots[[i-1]] = mort.plots[[i-1]] + guides(col = F)
}

diatom.n.final = do.call('grid.arrange',c(diatom.n.plots,ncol = 5))
delta.n.final = do.call('grid.arrange',c(delta.n.plots,ncol = 5))
delta.s.final = do.call('grid.arrange',c(delta.s.plots,ncol = 5))
delta.l.final = do.call('grid.arrange',c(delta.l.plots,ncol = 5))
delta.t.final = do.call('grid.arrange',c(delta.t.plots,ncol = 5))
mort.final = do.call('grid.arrange',c(mort.plots,ncol = 5))

ggsave(paste0(run.dir,runfile,'/',runfile,' ',plot.vars[1],'_',prim.prod,'.png'),diatom.n.final,width = 14, height = 14, units = 'in', dpi = 300)
ggsave(paste0(run.dir,runfile,'/',runfile,' ',plot.vars[2],'_',prim.prod,'.png'),delta.n.final,width = 14, height = 14, units = 'in', dpi = 300)
ggsave(paste0(run.dir,runfile,'/',runfile,' ',plot.vars[3],'_',prim.prod,'.png'),delta.s.final,width = 14, height = 14, units = 'in', dpi = 300)
ggsave(paste0(run.dir,runfile,'/',runfile,' ',plot.vars[4],'_',prim.prod,'.png'),delta.l.final,width = 14, height = 14, units = 'in', dpi = 300)
ggsave(paste0(run.dir,runfile,'/',runfile,' ',plot.vars[5],'_',prim.prod,'.png'),delta.t.final,width = 14, height = 14, units = 'in', dpi = 300)
ggsave(paste0(run.dir,runfile,'/',runfile,' ',plot.vars[6],'_',prim.prod,'.png'),mort.final,width = 14, height = 14, units = 'in', dpi = 300)

