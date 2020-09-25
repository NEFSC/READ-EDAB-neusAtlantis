#Function to create figures showing total predation based on biomass consumed and not split by age class
## Modified from plot_detailed_diet.R

# subset_diet -------------------------------------------------------------
#Reads in diet data

subset_diet = function(diet.file, spp.names,rm.zero = T){
  `%>%` = dplyr::`%>%`
  data = data.table::fread(diet.file)
  data = data %>%
    dplyr::select('Time','Predator',all_of(spp.names))
  data = data[apply(data[,-c(1:2)],1,function(x) return(!all(x==0))),]
  
  #Tranform to longform
  data.new = reshape2::melt(data,id.vars = c('Time','Predator'),variable.name = 'Prey',value.name = 'Consumption') %>%
    group_by(Time,Predator,Prey) %>%
    summarize(Consumption = sum(Consumption,na.rm=T))%>%
    ungroup()
  
  if(rm.zero){
    data.new = dplyr::filter(data.new, Consumption != 0)
  }
  return(data.new)
}


# plot_overall_predation --------------------------------------------------
#Overall predation is the sum of all consumed biomass from a prey group
#Need to calculate the relative contribution of each predator
#Consolidate groups below threshold
#Differs from existing plots since it doesn't differentiate age class or functional group type

plot_overall_predation =function(data,min.fract = 0.1,fig.dir,file.prefix){
  
  #Collapse small contributors into "Rest"
  data.new = data %>%
    dplyr::group_by(Time,Prey,Predator) %>%
    dplyr::summarise(n = sum(Consumption)) %>%
    dplyr::mutate(pct = n/sum(n)) %>%
    dplyr::mutate(less.min = pct < min.fract)  %>%
    dplyr::select(-n)
  data.small.pct = data.new %>%
    dplyr::filter(less.min == T) %>%
    dplyr::group_by(Time,Prey) %>%
    dplyr::summarize(pct = sum(pct)) %>%
    dplyr::mutate(Predator = 'Rest') %>%
    dplyr::arrange(Time,Prey,Predator,pct)
  data.final = data.new %>%
    dplyr::filter(less.min == F) %>%
    dplyr::select(-less.min) %>%
    dplyr::bind_rows(data.small.pct) %>%
    dplyr::arrange(Prey,Time,Predator,pct) %>%
    tidyr::tibble() %>%
    tidyr::complete(Predator,tidyr::nesting(Time,Prey),fill = list(pct = 0))
  
  #Loop through species
  plot.cols = c(RColorBrewer::brewer.pal(12,'Set3'),
                RColorBrewer::brewer.pal(8,'Dark2'),
                RColorBrewer::brewer.pal(8,'Set2'),
                RColorBrewer::brewer.pal(9,'Set1'))
  
  plot.spp = unique(data.new$Prey)
  filename = paste0(fig.dir,file.prefix,'_TotalConsumption.pdf')
  pdf(file = filename,width = 16, height = 8, onefile = T)
  for(i in 1:length(plot.spp)){
    
    data.spp = dplyr::filter(data.final,Prey == plot.spp[i])
    
    #Identify all groups who have zero consumption values across all times/box/layers
    which.zero = data.spp %>%
      dplyr::group_by(Predator) %>%
      dplyr::summarize(tot = sum(pct,na.rm=T)) %>%
      dplyr::mutate(all.zero = ifelse(tot==0,T,F)) %>%
      dplyr::filter(all.zero == T)
    which.zero = as.character(which.zero$Predator)
    
    #Remove zero consumption spp
    data.spp = data.spp %>%
      filter(!(Predator %in% which.zero))
    
    #Plot prey
    fig = ggplot2::ggplot(data.spp, ggplot2::aes(x= Time, y = pct, fill = Predator))+
        ggplot2::geom_area(alpha = 0.7, size = 0.25, color = 'black')+
        ggplot2::scale_fill_manual(values = plot.cols[1:length(unique(data.spp$Predator))])+
        ggplot2::ggtitle(plot.spp[i])+
        ggplot2::xlab('Day')+
        ggplot2::ylab('% Total Consumption')+
        ggplot2::theme_classic()+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      gridExtra::grid.arrange(fig)
  }
  dev.off()
}

#Example
atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_ZooFix_UpZL3/'
data.sub = subset_diet(diet.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_ZooFix_UpZL3/neus_outputDietCheck.txt',
                   spp.names  = c('ZL','ZM','ZS','BO','CLA','SCA','QHG','BFF','BD','LOB','PB','BB','DL','DR'),
                   rm.zero = T)
plot_overall_predation(data = data.sub,
                       min.fract = 0.1,
                       fig.dir = paste0(atl.dir,'Figures/'),
                       file.prefix = 'ZooFix_UpZL3')
