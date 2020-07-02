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
#' @model.dirs string. Character vector to of paths to all run outputs
#' @model.names string. Character vector of model run names
#' @plot.raw logical. Do you want to plot timeseries of the raw data (biomass)?
#' @plot.diff logical. Do you want to plot only the difference between 2 models?
#' @plot.out string. path where output plots are saved
#' @table.out logical. Do you want to export raw data?
#' @groups character vector. Default is all functional groups. Otherwise specify a vector of 3-letter group codes.
#' 
#' @return up to 2 pdfs with each page being timeseries of a given functional group. Also can output raw data if selected
#' 
#' Author: J. Caracappa

# model.dirs = c('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output_DinoFlag/',
#                'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output/')
# model.names = c('model 1','model 2')
# # plot.raw = T
# # plot.diff = T
# # plot.out = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output/Figures/'
# # table.out = T
# groups = c('HER','CLA','LOB')
# # groups = NULL

plot_run_comparisons = function(model.dirs,model.names,plot.raw = T,
                             plot.diff = T, plot.out, table.out = F, groups = NULL){
  `%>%` = dplyr::`%>%`
  
  bio.long.ls = list()
  t.starts = character()
  
  for(i in 1:length(model.dirs)){
    
    model.files = sort(list.files(model.dirs[i],'*.nc'))
    model.prefix =  strsplit(model.files[1],'\\.')[[1]][1]
    model.bio = paste0(model.prefix, 'BiomIndx.txt')
    
    bio = read.table(paste0(model.dirs[i],model.bio),header = T)
    

    
    #If specifying groups, subset only those
    if(!is.null(groups)){
      fgs = groups
    }else{
      fgs = colnames(bio)[2:ncol(bio)]
      fgs = fgs[-grep('Rel',fgs)]
    }
    bio = subset(bio,select = c('Time',fgs))
    
    #convert each model to long format
    bio.long = tidyr::gather(bio,key = 'Group','Biomass',-Time)
    
    bio.long$Model = paste0('model',i)
    
    nc = ncdf4::nc_open(paste0(model.dirs[i],model.prefix,'.nc'))
    t.start = strsplit(ncdf4::ncatt_get(nc,'t')$units,'\\ ')[[1]][3]
    ncdf4::nc_close(nc)
    
    bio.long$Real.Time = as.Date(bio.long$Time,origin = t.start)
    
    t.starts[i] = t.start
    bio.long.ls[[i]] = bio.long
  }

  bio.all = dplyr::bind_rows(bio.long.ls)
  
  if(is.null(groups)){
    plot.groups = sort(unique(bio.all$Group))  
  }else{
    plot.groups = groups
  }
  
  if(plot.diff){

    if(length(unique(t.starts))!=1){
      print('Error: models do not have the same start date. Difference function not applicable')
    } else{
      bio.diff = bio.all %>% dplyr::ungroup() %>%
        dplyr::group_by(Time,Real.Time,Group) %>% 
        dplyr::arrange(Group,Time,Model) %>%
        dplyr::summarize(bio.diff = Biomass[1]-Biomass[2])
      # bio.diff$Real.Time = as.Date(bio.diff$Time,origin = t.start1)
      
      pdf(paste0(plot.out,'Biomass_Difference.pdf'),width = 14,onefile = T)
      for(i in 1:length(plot.groups)){
        p= ggplot2::ggplot(data = subset(bio.diff,Group == plot.groups[i]), ggplot2::aes(x=Real.Time,y = bio.diff))+
          ggplot2::geom_path()+
          ggplot2::ylab('Difference in Model Biomass (Tonnes)')+
          ggplot2::xlab('Date')+
          ggplot2::ggtitle(plot.groups[i])+
          ggplot2::theme_minimal()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )
        gridExtra::grid.arrange(p)
      }
      dev.off()
    }
  }
  
  
  
  if(plot.raw){
      pdf(paste0(plot.out,'Model_Comparison_Biomass.pdf'),width = 14,onefile = T)
    for(i in 1:length(plot.groups)){
      p= ggplot2::ggplot(data = subset(bio.all,Group == plot.groups[i]),
                         ggplot2::aes(x=Real.Time,y = Biomass,color = Model))+
        ggplot2::geom_path()+
        ggplot2::scale_color_manual(name = 'Model',labels = model.names,values = RColorBrewer::brewer.pal(length(model.dirs),'Set1'))+
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
