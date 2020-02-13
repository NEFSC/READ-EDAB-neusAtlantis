# library(ggplot2)
# library(dplyr)
# library(gridExtra)

# run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/'
# runfile = '02062020_PrimProdDiag_1'

# setwd(paste0(run.dir,runfile))

# load(paste0(runfile,'_prepro.rdata'))
biomass = result$biomass

fgs = read.csv(paste0(param.dir,fgs.file),stringsAsFactors = F)

if(spp.id == 'ALL'){
  spp.long = sort(fgs$LongName)
} else{
  spp.long =  fgs$LongName[match(spp.id,fgs$Code)]  
}

g.ls = list()

for(i in 1:length(spp.long)){
  sub = biomass %>% filter(species == spp.long[i] & time >= tstart & time <= tstop)
  
  if(log.biomass == T){
    sub$atoutput = log(sub$atoutput,10)
  }
  g.ls[[i]] = ggplot(data = sub, aes(x = time, y= atoutput))+
    geom_path()+
    ggtitle(spp.long[i])
}

full.plot = do.call('grid.arrange',c(g.ls))
ggsave(paste0(runfile,' BiomassEnd.png'),full.plot,width = 20, height = 14, units = 'in',dpi = 300)
