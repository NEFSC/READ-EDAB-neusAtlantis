#' Generates timeseries of functional groups for two models
#' 
#' To help diagnostics and callibration, it can often be useful to 
#' compare the timeseries of functional groups and other variables
#' between two models. This function generates a pdf of timeseries 
#' plots of both models and a second plots a timeseries as the difference
#' between both models. It also allows the user to specify the desired 
#' functional groups to compare. All data is based on the BiomIndx.txt 
#' output file
#' 
#' @model1.dir string. path to the first model output files
#' @model2.dir string. path to the second model output files. Convention is that model2 is most recent
#' @plot.raw logical. Do you want to plot timeseries of the raw data (biomass)?
#' @plot.diff logical. Do you want to plot only the difference between 2 models?
#' @plot.out string. path where output plots are saved
#' @table.out logical. Do you want to export raw data?
#' @groups character vector. Default is all functional groups. Otherwise specify a vector of 3-letter group codes.
#' 
#' @return up to 2 pdfs with each page being timeseries of a given functional group. Also can output raw data if selected
#' 
#' Author: J. Caracappa

# model1.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/'
# model2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/'
# plot.raw = T
# plot.diff = T
# plot.out = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/Figures/'
# table.out = T
# # groups = c('HER','CLA','LOB')
# groups = NULL

plot_run_comparisons = function(model.dirs,model.names,plot.raw = T,
                             plot.diff = T, plot.out, table.out = F, groups = NULL,remove.init = F,rm.rel = T){
  `%>%` = dplyr::`%>%`
  
  model.files =lapply(model.dirs,function(x) {return(sort(list.files(x,'*.nc')))})
  
  model.prefix = lapply(model.files,function(x) {return(strsplit(x[1],'\\.')[[1]][1])})
  
  model.bio = sapply(model.prefix,function(x) {return(paste0(x,'BiomIndx.txt'))})
  
  bio.ls = list()
  for(i in 1:length(model.dirs)){
    
    bio.run = read.table(file = paste0(model.dirs[i],model.bio[i]), header = T)  
    
    if(!is.null(groups)){
      bio = lapply(bio,function(x) {return(subset(x,select = c('Time',groups)))})
    }
    
    bio.long.run = tidyr::gather(bio.run,key = 'Group', 'Biomass', -Time)
    
    bio.long.run$Model = model.names[i]
    
    nc = ncdf4::nc_open(paste0(model.dirs,model.prefix,'.nc'))
    t.start = strsplit(ncdf4::ncatt_get(nc,'t')$units,'\\ ')[[1]][3]
    
    ncdf4::nc_close(nc)
    
    bio.long.run$Real.Time = as.Date(bio.long.run$Time, origin = t.start)
    
    bio.ls[[i]] = bio.long.run
  }

  bio.all = dplyr::bind_rows(bio.ls) %>%
    dplyr::mutate(Model = factor(Model,levels= model.names))
  
  if(remove.init){
    bio.all = bio.all %>% dplyr::filter(Time != 0)
  }
  
  if(is.null(groups)){
    plot.groups = sort(unique(bio.all$Group))  
    if(rm.rel){
      plot.groups = plot.groups[-grep('Rel',plot.groups)]  
    }
    
  }else{
    plot.groups = groups
  }
  
  #need to create contingency for >2 comparisons
  if(plot.diff){
    print('Error: difference plot temporarily disabled')
  }
  # if(plot.diff){
  # 
  #   if(length(unique(unlist(t.start)))!=1){
  #     print('Error: models do not have the same start date. Difference function not applicable')
  #   } else{
  #     bio.diff = bio.all %>% dplyr::ungroup() %>%
  #       dplyr::group_by(Time,Real.Time,Group) %>% 
  #       dplyr::arrange(Group,Time,Model) %>%
  #       dplyr::summarize(bio.diff = Biomass[1]-Biomass[2])
  #     # bio.diff$Real.Time = as.Date(bio.diff$Time,origin = t.start1)
  #     
  #     pdf(paste0(plot.out,'Biomass_Difference.pdf'),width = 14,onefile = T)
  #     for(i in 1:length(plot.groups)){
  #       p= ggplot2::ggplot(data = subset(bio.diff,Group == plot.groups[i]), ggplot2::aes(x=Real.Time,y = bio.diff))+
  #         ggplot2::geom_path()+
  #         ggplot2::ylab('Difference in Model Biomass (Tonnes)')+
  #         ggplot2::xlab('Date')+
  #         ggplot2::ggtitle(plot.groups[i])+
  #         ggplot2::theme_minimal()+
  #         ggplot2::theme(
  #           plot.title = ggplot2::element_text(hjust = 0.5)
  #         )
  #       gridExtra::grid.arrange(p)
  #     }
  #     dev.off()
  #   }
  # }
  
  plot.cols = RColorBrewer::brewer.pal(12,'Paired')
  if(plot.raw){
      pdf(paste0(plot.out,'Model_Comparison_Biomass.pdf'),width = 14,onefile = T)
    for(i in 1:length(plot.groups)){
      p= ggplot2::ggplot(data = subset(bio.all,Group == plot.groups[i]),
                         ggplot2::aes(x=Real.Time,y = Biomass,color = Model))+
        ggplot2::geom_path(size = 1)+
        ggplot2::scale_color_manual(name = 'Model',labels = model.names,values = plot.cols )+
        ggplot2::ylab('Group Biomass (Tonnes)')+
        ggplot2::xlab('Date')+
        ggplot2::ggtitle(plot.groups[i])+
        ggplot2::theme_minimal()+
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          legend.position = 'bottom',
          legend.box = 'horizontal'
        )
      gridExtra::grid.arrange(p)
    }
    dev.off()
  }
  
  if(table.out){
    if(plot.raw){
      write.csv(bio.all,file=paste0(plot.out,'Biomass_Difference_Data.csv'),row.names = F)
    }
    if(plot.diff){
      write.csv(bio.diff,file = paste0(plot.out,'Model_Comparison_Biomass_Data.csv'),row.names = F)
    }
  }
  
}

#Testing
# comp.model.groups(
#   model1.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/',
#   model2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_start1964/',
#   plot.raw = T,
#   plot.diff = T,
#   plot.out = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_start1964/Figures/',
#   table.out = T,
#   # groups = c('HER','CLA','LOB')
#   groups = NULL
# )
