#Function to pull and format data on Hirata model Diatom:Dinoflagellate ratio (i.e. % Diatom)

# FDiatom.file  =  'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_FDIATOM-HIRATA.CSV'
# Micro.file =     'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-PSC_MICRO-TURNER.CSV'
# Chl.file =     'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Climatology/v6/DOY-OCCCI-ATLANTIS_NEUS-CHLOR_A-CCI.CSV'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/v6/'
# figure.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Figures/v6/'

make_satphyto_hirata_PLDF = function(FDiatom.file,Micro.file,Chl.file,out.dir,figure.dir){
  

  #Load Packages
  library(dplyr)
  library(ggplot2)
  
  #Read in data from CSV
  data.Fdiatom =  read.csv(FDiatom.file,header = T, as.is = T)
  data.micro =     read.csv(Micro.file,header = T, as.is = T)
  data.chl =     read.csv(Chl.file,header = T, as.is = T)
  
  #Extract DOY
  data =  bind_rows(data.Fdiatom,data.micro,data.chl) %>%
    select(PERIOD,PROD,SUBAREA,MED)%>%
    tidyr::spread(PROD,MED)%>%
    tidyr::separate(PERIOD,c('ID','DOY','START','END'))%>%
    group_by(DOY,SUBAREA)%>%
    summarise(PSC_FDIATOM = mean(PSC_FDIATOM,na.rm=T),
              CHLOR_A = mean(CHLOR_A,na.rm=T),
              PSC_MICRO = mean(PSC_MICRO,na.rm=T))%>%
    mutate(DOY = as.numeric(DOY),
           DIATOM.PROP = (PSC_FDIATOM*CHLOR_A)/PSC_MICRO)
    
  data.out = select(data,DOY,SUBAREA,DIATOM.PROP)%>%rename(MED = 'DIATOM.PROP')
  saveRDS(data.out,paste0(out.dir,'diatom_proportion_DOY_dataframe.rds'))
    
  diatom.prop = matrix(NA, nrow = 30, ncol = 365)
  PL.DF = matrix(NA, nrow = 30, ncol = 365)
  
  fig.diatom.prop = list()
  fig.PL.DF = list()
  
  boxes = 0:29
  b=1
  for(b in 1:30){
    dat = data %>% 
      filter(SUBAREA == boxes[b]) %>%
      arrange(DOY)%>%
      mutate(PLDF = DIATOM.PROP/(1-DIATOM.PROP))
    
    #make Diatom proportion plot
    fig.diatom.prop[[b]] = ggplot(dat,aes(x = as.numeric(DOY), y = DIATOM.PROP))+
      geom_line()+
      geom_hline(yintercept = 1)+
      xlab('DOY')+
      ylab('Diatom Proportion')+
      ggtitle(paste0('Box ',boxes[b]))+
      theme_bw()+
      theme(plot.title = element_text(hjust = '0.5'))
    
    fig.PL.DF[[b]] = ggplot(dat,aes(x = as.numeric(DOY), y = PLDF))+
      geom_line()+
      xlab('DOY')+
      ylab('Diatom:Dinoflagellate')+
      ggtitle(paste0('Box ',boxes[b]))+
      theme_bw()+
      theme(plot.title = element_text(hjust = '0.5'))
    
    diatom.prop[b,] = dat$DIATOM.PROP
    PL.DF[b,] = dat$PLDF
    
  }
  
  #save plots
  pdf(paste0(figure.dir,'Hirata_Diatom_Proportion.pdf'),width = 16, height = 6, onefile = T)
  for(b in 1:length(boxes)){gridExtra::grid.arrange(fig.diatom.prop[[b]])}
  dev.off()
  
  pdf(paste0(figure.dir,'Hirata_Diatom_Dinoflagellate_v6.pdf'),width = 16, height = 6, onefile = T)
  for(b in 1:length(boxes)){gridExtra::grid.arrange(fig.PL.DF[[b]])}
  dev.off()
  
  #Save Diatom Proportions and PL:DF
  saveRDS(diatom.prop,file = paste0(out.dir,'diatom_proportion_DOY_matrix.rds'))
  saveRDS(PL.DF,file = paste0(out.dir,'diatom_dinoflag_ratio_DOY.rds'))
}