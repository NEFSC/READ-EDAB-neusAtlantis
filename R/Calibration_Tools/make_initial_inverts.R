#Script to refine initial conditions for invertebrates (area-based)
library(dplyr)
library(ncdf4)

fgs =  read.csv(here::here('currentVersion','neus_groups.csv')) %>%
  mutate(is.invert = ifelse(NumCohorts <10,1,0))%>%
  select(Code,Name,LongName,NumCohorts,is.invert)

bgm = rbgm::read_bgm(here::here('currentVersion','neus_tmerc_RM2.bgm'))$boxes
bgm$volume = NA
for(i in 1:nrow(bgm)){
  if(abs(bgm$botz[i])<50){
    bgm$volume[i] = abs(bgm$botz[i])*bgm$area[i]
  }else{
    bgm$volume[i] = abs(50*bgm$area[i])
  }
}
box.props = read.csv(here::here('diagnostics','Group_Box_Proportions.csv'))

ss.data = read.csv(here::here('diagnostics','StockSmart_Adjusted_Abundance.csv')) %>%
  left_join(fgs)%>%
  filter(is.invert == 1)


#Calculate new density value and write into nc file
init.nc = nc_open(here::here('currentVersion','neus_init.nc'),write = T)

group.names = unique(ss.data$Code)

i = 1
for(i in 1:nrow(ss.data)){

  group.box.props = box.props[,which(colnames(box.props)==group.names[i])]
  
  if(ss.data$NumCohorts[i] == 1){
    
    nc.name = paste0(ss.data$Name[i],'_N')
    
   # print(ncvar_get(init.nc,nc.name))
    # sum(dat.group * box.area,na.rm=T)*1E-9*5.7*20
    
    #Convert to N
    new.biomass.N = ss.data$biomass.tot[i] * 1E9 /20/5.7
    #Convert to area density
    new.biomass.box = (new.biomass.N * group.box.props)/bgm$area
    #Write to file
    ncvar_put(init.nc,nc.name,new.biomass.box)
    
  }else{
    #Loop through age classes
    for(j in 1:2){
      nc.name = paste0(ss.data$Name[i],'_N',j)
      
      # print(ncvar_get(init.nc,nc.name))
      # sum(dat.group * box.area,na.rm=T)*1E-9*5.7*20
      
      new.biomass.N = ss.data$biomass.tot[i]*1E9/20/5.7/2
      new.biomass.N = (new.biomass.N*group.box.props)/bgm$volume
      new.biomass.N[which(!is.finite(new.biomass.N)| new.biomass.N == 0)] = NA
      new.biomass.box = matrix(NA,nrow = 5,ncol = 30)
      new.biomass.box[1,] = new.biomass.N
      
      ncvar_put(init.nc,nc.name,new.biomass.box)
    }
  }
}

nc_close(init.nc)
#Reset Initial scalar in run.prm
source(here::here('R','edit_param_init_scalar.R'))
init.scalar = get_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                                    groups.file = here::here('currentVersion','neus_groups.csv'),
                                    write.output = F)
new.init.scalar = init.scalar
for( i in 1:length(group.names)){new.init.scalar$init.scalar[which(as.character(new.init.scalar$group) == as.character(group.names[i]))] = 1 }
new.init.scalar$init.scalar = as.numeric(as.character(new.init.scalar$init.scalar))
edit_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                       groups.file = here::here('currentVersion','neus_groups.csv'),
                       new.init.scalar = new.init.scalar,
                       overwrite = T)

