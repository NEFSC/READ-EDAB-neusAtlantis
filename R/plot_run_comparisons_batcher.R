#Function to analyze batch output

# #Set Project Name
# fig.dir = here::here('Figures/')
# #Define run name
# run.name = 'misc_BHalpha_3'
# #Define run set directory
# run.dir = here::here('Atlantis_Runs','misc_mum_C_BHalpha_1',run.name,'')
# #Define run setup file (long version with actual scalars)
# run.index = read.csv(here::here('Setup_Files',paste0(run.name,'_long.csv')),as.is = T)%>%
#   select(ID,dir.name)
# #Define Parameter Files Director
# param.dir = here::here('currentVersion')
# #Define groups you want plotted
# x = read.csv(here::here('currentVersion','neus_groups.csv'),header = T, as.is = T) %>% filter(IsTurnedOn == T)
# group.names = sort(x$Code)

plot_run_comparisons_batcher = function(
    fig.dir,
    run.name,
    run.dir,
    run.index,
    param.dir,
    group.names,
    plot.raw,
    plot.rel,
    plot.log){
  
  library(dplyr)
  library(ggplot2)
  
  #Define Directories
  folders = unique(paste0(run.dir,'/',run.index$dir.name))
  
  #Loop through folders and create a dataframe of desired group biomass using dir.name as index
  out.ls = list()
  fgs = read.csv(paste0(param.dir,'/neus_groups.csv'),header = T) %>% filter(IsTurnedOn == T)
  
  i=1
  for(i in 1:length(folders)){
    file.name = paste0(folders[i],'/neus_outputBiomIndx.txt')
    if(file.exists(file.name)){
      dat = read.table(paste0(folders[i],'/neus_outputBiomIndx.txt'),header =T, as.is = T)
      dat2 = dat %>% select(Time,all_of(group.names))
      dat.long = reshape2::melt(dat2,id.vars = 'Time')
      dat.long$dir.name = unique(run.index$dir.name)[i]
      dat.long$variable = as.character(dat.long$variable)
      dat.long = dat.long 
      out.ls[[i]] = dat.long
    }else{
      next()
    }
    
  }
  if(length(out.ls)==0){
    return(NA)
  }else{
    data.combined = bind_rows(out.ls)  
  }
  
  
  #Merge with run.index to get parameters
  data.combined = left_join(data.combined, run.index )
  
  # data.combined %>%filter(group == 'DOG')%>%select(Time,value,ID)%>%tidyr::spread(ID,value)
  #plot each of the interest groups
  plot.cols = c(RColorBrewer::brewer.pal(12,'Paired'),RColorBrewer::brewer.pal(8,'Dark2'),RColorBrewer::brewer.pal(8,'Accent'))[1:nrow(run.index)]
  
  if(plot.log){
    pdf(paste0(fig.dir,run.name,'_Log_Biomass.pdf'),onefile = T, width =10, height = 8)
    for(spp in 1:length(group.names)){
      dat = filter(data.combined, variable == group.names[spp])
      
      plot.out = ggplot(dat, aes(x=Time, y = log(value,10), color = as.factor(ID)))+
        scale_color_manual(values = plot.cols)+
        ylab('log(Biomass) (mT)')+
        labs(color = 'Init Scalar')+
        theme_bw()+
        geom_line()+
        ggtitle(group.names[spp])+
        guides(color = guide_legend(nrow = 2, byrow = T))+
        theme(legend.position = 'bottom',
              legend.box = 'horizontal',
              plot.title = element_text(hjust= 0.5))
      gridExtra::grid.arrange(plot.out)
    }
    dev.off()
  }
  
  if(plot.rel){
    pdf(paste0(fig.dir,run.name,'_Relative_Biomass.pdf'),onefile = T, width =10, height = 8)
    for(spp in 1:length(group.names)){
      dat = filter(data.combined, variable == group.names[spp])
      plot.out = ggplot(dat,aes(x = Time, y= value/value[1],color = as.factor(ID)))+
        ylab('Relative Biomass')+
        labs(color = 'Init Scalar')+
        theme_bw()+
        geom_line()+
        scale_color_manual(values = plot.cols)+
        ggtitle(group.names[spp])+
        guides(color = guide_legend(nrow = 2, byrow = T))+
        theme(legend.position = 'bottom',
              legend.box = 'horizontal',
              plot.title = element_text(hjust= 0.5))
      gridExtra::grid.arrange(plot.out)
    }
    dev.off()
  }
  
  if(plot.raw){
    pdf(paste0(fig.dir,run.name,'_Raw_Biomass.pdf'),onefile = T, width =10, height = 8)
    for(spp in 1:length(group.names)){
      dat = filter(data.combined, variable == group.names[spp])
      plot.out = ggplot(dat,aes(x = Time, y= value,color = as.factor(ID)))+
        ylab('Relative Biomass')+
        labs(color = 'Init Scalar')+
        theme_bw()+
        geom_line()+
        scale_color_manual(values = plot.cols)+
        ggtitle(group.names[spp])+
        guides(color = guide_legend(nrow = 2, byrow = T))+
        theme(legend.position = 'bottom',
              legend.box = 'horizontal',
              plot.title = element_text(hjust= 0.5))
      gridExtra::grid.arrange(plot.out)
    }
    dev.off()
  }
  
  
}

