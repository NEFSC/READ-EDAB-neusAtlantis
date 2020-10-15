#Script to plot the bethic species from a box

library(dplyr)
library(ggplot2)
library(ncdf4)

# Set box
box = 4
lev = 5

#find box area
bgm = rbgm::bgmfile(here::here('Geometry','neus_tmerc_RM2.bgm'))
area = as.numeric(bgm$boxes[which(bgm$boxes$.bx0 == box),'area'])

#get main output
run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/'
run.name = 'Obs_Hindcast_ZooFix_OldParams'
out.nc = nc_open(paste0(run.dir,run.name,'/neus_output.nc'))

var.names = names(out.nc$var)
var.ls = list()
for(i in 1:length(var.names)){
  units = ncatt_get(out.nc,var.names[i],'units')$value
  if(!(units %in% c('mg N m-3','mg NO3 m-3','mg O2 m-3','mg NH3 m-3'))){
    var.ls[[i]] = NULL
    next()
  }else{
    var.i = ncvar_get(out.nc,var.names[i])[lev,box+1,]  
    if(all(var.i == 0)){
      var.ls[[i]] = NULL
      next()
    }else{
      if(var.i[1] == 0){
        first.nz = which(var.i != 0)[1]
        var.pct = var.i/var.i[first.nz]
      }else{
        var.pct = var.i/var.i[1]  
      }
      var.ls[[i]] = data.frame(Time = 1:length(var.i),var = var.names[i], value = var.i, value.rel = var.pct)  
    }
  }
  print(i)
}
var.df = bind_rows(var.ls) 

plot.name =paste0(run.dir,run.name,'/Figures/Box_',box,'_benth_') 

#var groups
new.vars= unique(var.df$var)

chem.vars = c('DON','NH3','NO3','Oxygen')
micro.vars = c('Diatom_N','Lab_Det_N','MicroPB_N','Pelag_Bact_N','PicoPhytopl_N','Ref_Det_N','Sed_Bact_N')
rest = new.vars[!(new.vars %in% c(chem.vars,micro.vars))]


#Chem plot
DF = var.df %>% filter(var %in% chem.vars)
ggplot(DF,aes(x=Time,y=value.rel,col=var))+geom_line()+ggsave(paste0(plot.name,'chem.png'),width = 18,height = 6)

#Micro Plot
DF = var.df %>% filter(var %in% micro.vars)
ggplot(DF,aes(x=Time,y=value.rel,col=var))+geom_line()+ggsave(paste0(plot.name,'micro.png'),width = 18,height = 6)

#Larger Groups plot
DF = var.df %>% filter(var %in% rest)
ggplot(DF,aes(x=Time,y=value.rel,col=var))+geom_line()+ggsave(paste0(plot.name,'others.png'),width = 18,height = 6)


rel.val = var.df %>% group_by(var) %>% summarize(last(value.rel))
# test.group = c(rest[1:10])
test.group = c('Lab_Det_N','Pelag_Bact_N','Sed_Bact_N')
DF = var.df %>% filter(var %in% test.group)
ggplot(DF,aes(x=Time,y=value.rel,col=var))+geom_line(lwd=1.2)
+ggsave(paste0(plot.name,'test.png'),width = 18,height = 6)
