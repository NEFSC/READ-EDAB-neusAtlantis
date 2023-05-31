#Plot forced phytoplankton groups in output and forcing files to compare
library(dplyr)
library(ggplot2)
library(ncdf4)

groups = c('Diatom','Dinoflagellates','Pico-phytoplankton')
nc.var.name = c('Diatom_N','DinoFlag_N','PicoPhytopl_N')

run.name = 'phyto_svel_1_0'
# output.data = readRDS(here::here('Atlantis_Runs',run.name,'Post_Processed','Data','biomass_spatial_stanza.rds'))%>%
#   filter(species %in% groups)
output.nc = nc_open(here::here('Atlantis_Runs','phyto_svel_1',run.name,'neus_output.nc'))
output.dat.ls = list()
for(i in 1:length(nc.var.name)){
  
  output.dat.ls[[i]] = ncvar_get(output.nc,nc.var.name[i]) %>%
    reshape2::melt()%>%
    rename(layer = 'Var1',box = 'Var2',time = 'Var3',biomass = 'value')%>%
    mutate(group = nc.var.name[i],
           time = time*73,
           source = 'output')%>%
    filter(biomass != 0)
}
output.dat = bind_rows(output.dat.ls)
nc_close(output.nc)

force.files = list.files(path = here::here('currentVersion','tsfiles','Annual_Files'),pattern = 'Phyto_',full.names = T)
i=1
force.dat.ls = list()
for(i in 1:length(force.files)){
  
  force.year = nc_open(force.files)  
  force.year.ls = list()
  j=1
  for(j in 1:length(nc.var.name)){
    
    force.year.ls[[j]] = x=  ncvar_get(force.year,nc.var.name[j]) %>%
      reshape2::melt()%>%
      rename(layer = 'Var1',box = 'Var2',doy = 'Var3',biomass = 'value')%>%
      mutate(group = nc.var.name[j],
             time = (doy-1+(365*(i-1))),
             source = 'forcing')%>%
      select(-doy)%>%
      filter(!(is.na(biomass)))
  }
  force.dat.ls[[i]] = bind_rows(force.year.ls)
}
force.dat = bind_rows(force.dat.ls)

# x = filter(force.dat,box ==1)
# 
# ggplot(x,aes(x=time,y=biomass,color = group))+geom_line()

data.all = bind_rows(output.dat,force.dat)%>%
  filter(!is.na(group))
boxes = 0:29
i=2

pdf(here::here('Figures','SatPhyto_Comparisons.pdf'))
for(i in 1:length(boxes)){
  
  data.box = data.all %>% filter(box == (boxes[i]+1))
  if(nrow(data.box)==0){next()}
  
  p = ggplot(data.box, aes(x = time, y = biomass,color = source))+
    geom_line()+
    facet_grid(layer~group,scale = 'free_y')+
    ggtitle(paste0('Box ',boxes[i]))
   
  gridExtra::grid.arrange(p)
}
dev.off()

