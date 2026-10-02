#Function to plot recruitment for each species




plot_recruits = function(atl.dir, bio.dat, fgs, groups = NULL, fig.dir, fig.name){
  if(!dir.exists(fig.dir)){
    stop()
  }
  
  if(is.null(groups)){
    spp = fgs$Code[which(fgs$NumCohorts == 10)]
    spp.long = fgs$LongName[which(fgs$NumCohorts == 10)]
  }else{
    spp = groups
    spp.long = fgs$LongName[which(fgs$Code == groups)]
  }
  
  group.cols = paste0(spp,'.0')
  
  dat.sub = dplyr::select(bio.dat,Time,dplyr::all_of(group.cols))
  colnames(dat.sub) = c('Time',spp.long)
  dat.sub = tidyr::gather(dat.sub, key = 'spp',value = 'value',-Time)
  
  ggplot2::ggplot(dat.sub,ggplot2::aes(x = Time, y = value))+
    ggplot2::geom_line(size= 0.1)+
    ggplot2::facet_wrap(~spp,scales = 'free_y')+
    ggplot2::theme_bw()+
    ggplot2::ggsave(paste0(fig.dir,fig.name,'_recruitment.png'),width = 16, height = 12, units = 'in', dpi = 300)
    
}

atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/PersistCheck_1/'
plot_recruits(atl.dir = atl.dir,
              bio.dat = read.table(paste0(atl.dir,'neus_outputAgeBiomIndx.txt'),header= T, stringsAsFactors = F),
              fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T),
              fig.dir = paste0(atl.dir,'Post_Processed/'),
              fig.name = 'PersistCheck_1'
              )

