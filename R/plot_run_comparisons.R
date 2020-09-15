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

plot_run_comparisons = function(model1.dir,model2.dir,model1.name,model2.name,plot.raw = T,
                             plot.diff = T, plot.out, table.out = F, groups = NULL,remove.init = F){
  `%>%` = dplyr::`%>%`
  
  model1.files = sort(list.files(model1.dir,'*.nc'))
  model2.files = sort(list.files(model2.dir,'*.nc'))
  
  model1.prefix = strsplit(model1.files[1],'\\.')[[1]][1]
  model2.prefix = strsplit(model2.files[1],'\\.')[[1]][1]
  
  model1.bio = paste0(model1.prefix,'BiomIndx.txt')
  model2.bio = paste0(model2.prefix, 'BiomIndx.txt')
  
  bio1 = read.table(file = paste0(model1.dir,model1.bio),header = T)
  bio2 = read.table(file = paste0(model2.dir,model2.bio),header = T)
  
  fgs = colnames(bio1)[2:ncol(bio1)]
  
  #If specifying groups, subset only those
  if(!is.null(groups)){
    bio1 = subset(bio1,select = c('Time',groups))
    bio2 = subset(bio2,select = c('Time',groups))
  }
  
  #convert each model to long format
  bio1.long = tidyr::gather(bio1,key = 'Group','Biomass',-Time)
  bio2.long = tidyr::gather(bio2,key = 'Group','Biomass',-Time)
  
  bio1.long$Model = 'model1'
  bio2.long$Model = 'model2'
  
  nc1 = ncdf4::nc_open(paste0(model1.dir,model1.prefix,'.nc'))
  nc2 = ncdf4::nc_open(paste0(model2.dir,model2.prefix,'.nc'))
  
  t.start1 = strsplit(ncdf4::ncatt_get(nc1,'t')$units,'\\ ')[[1]][3]
  t.start2 = strsplit(ncdf4::ncatt_get(nc2,'t')$units,'\\ ')[[1]][3]
  
  ncdf4::nc_close(nc1)
  ncdf4::nc_close(nc2)
  
  bio1.long$Real.Time = as.Date(bio1.long$Time,origin = t.start1)
  bio2.long$Real.Time = as.Date(bio2.long$Time,origin = t.start2)
  
  bio.all = rbind(bio1.long,bio2.long)
  
  if(remove.init){
    bio.all = bio.all %>% filter(Time != 0)
  }
  
  if(is.null(groups)){
    plot.groups = sort(unique(bio.all$Group))  
  }else{
    plot.groups = groups
  }
  
  if(plot.diff){

    if(t.start1 != t.start2){
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
        ggplot2::scale_color_manual(name = 'Model',labels = c(model1.name,model2.name),values = c('red3','blue3'))+
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
