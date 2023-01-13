#' Generates Timeseries of Catch.txt for two Atlantis runs
#' 
#' @model1.dir string. path to the first model output files
#' @model2.dir string. path to the second model output files. Convention is that model2 is most recent
#' @plot.raw logical. Do you want to plot timeseries of the raw data (catch)?
#' @plot.diff logical. Do you want to plot only the difference between 2 models?
#' @plot.out string. path where output plots are saved
#' @table.out logical. Do you want to export raw data?
#' @groups character vector. Default is all functional groups. Otherwise specify a vector of 3-letter group codes.
#' 
#' @return up to 2 pdfs with each page being timeseries of a given functional group. Also can output raw data if selected
#' 
#' Author: J. Caracappa

# model1 = here::here('Atlantis_Runs','misc_BHalpha_3b','')
# model2 = here::here('Atlantis_Runs','misc_BHalpha_3c','')
# model.dirs = c(model1,model2)
# model.names = c('A','B')
# plot.raw = T
# plot.diff = T
# plot.out = here::here('Figures','Catch_Comparison_Test')
# plot.out ='C:/Users/joseph.caracappa/Documments/Atlantis/Obs_Hindcast/Diagnostic_Figures/Run_Catch_Comparisons/'
# table.out = T
# groups = c('HER','CLA','LOB')
# groups = NULL

plot_run_catch_comparisons = function(model.dirs,model.names,plot.raw = T,
                                plot.diff = T, plot.out, table.out = F, groups = NULL,remove.init = F){
  `%>%` = dplyr::`%>%`
  
  model.files =lapply(model.dirs,function(x) {return(sort(list.files(x,'*.nc')))})
  
  model.prefix = lapply(model.files,function(x) {return(strsplit(x[1],'\\.')[[1]][1])})
  
  model.catch = lapply(model.prefix,function(x) {return(paste0(x,'Catch.txt'))})
  
  catch = lapply(paste0(model.dirs,model.catch),function(x) {return(read.table(file = x,header =T))})
  
  fgs = colnames(catch[[1]])[2:ncol(catch[[1]])]
  fgs = fgs[-grep('TsAct',fgs)]
  
  #If specifying groups, subset only those
  if(!is.null(groups)){
    catch = lapply(catch,function(x) {return(subset(x,select = c('Time',groups)))})
  }
  
  #convert each model to long format
  catch.long = lapply(catch,function(x) {return(tidyr::gather(x,key = 'Group','Catch',-Time))})
  
  for(i in 1:length(catch.long)){catch.long[[i]]$Model = model.names[i]}
  
  nc = lapply(paste0(model.dirs,model.prefix,'.nc'),function(x) return(ncdf4::nc_open(x)))
  
  t.start = lapply(nc,function(x) return(strsplit(ncdf4::ncatt_get(x,'t')$units,'\\ ')[[1]][3]))
  
  lapply(nc,function(x) ncdf4::nc_close(x)) 
  
  for(i in 1:length(catch.long)){ catch.long[[i]]$Real.Time = as.Date(catch.long[[i]]$Time, origin = t.start[[i]])}
  
  catch.all = dplyr::bind_rows(catch.long) %>%
    dplyr::mutate(Model = factor(Model,levels= model.names))
  
  if(remove.init){
    catch.all = catch.all %>% dplyr::filter(Time != 0)
  }
  
  if(is.null(groups)){
    plot.groups = sort(unique(catch.all$Group))  
  }else{
    plot.groups = groups
  }
  
  plot.cols = c(RColorBrewer::brewer.pal(12,'Paired'),RColorBrewer::brewer.pal(8,'Set2'))
  if(plot.diff){
    
    
    if(all.equal(unlist(t.start),rep(unlist(t.start)[1],length(unlist(t.start))))){
      print('Error: models do not have the same start date. Difference function not applicable')
    } else{
      catch.diff = catch.all %>% dplyr::ungroup() %>%
        dplyr::group_by(Time,Real.Time,Group) %>% 
        dplyr::arrange(Group,Time,Model) %>%
        dplyr::summarize(catch.diff = Catch[1]-Catch[2])
      # bio.diff$Real.Time = as.Date(bio.diff$Time,origin = t.start1)
      
      pdf(paste0(plot.out,'Catch_Difference.pdf'),width = 14,onefile = T)
      for(i in 1:length(plot.groups)){
        p= ggplot2::ggplot(data = subset(catch.diff,Group == plot.groups[i]), ggplot2::aes(x=Real.Time,y = catch.diff))+
          ggplot2::geom_path()+
          ggplot2::ylab('Difference in Model Catch (Tonnes)')+
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
    pdf(paste0(plot.out,'Model_Comparison_Catch.pdf'),width = 14,onefile = T)
    for(i in 1:length(plot.groups)){
      p= ggplot2::ggplot(data = subset(catch.all,Group == plot.groups[i]),
                         ggplot2::aes(x=Real.Time,y = Catch,color = Model))+
        ggplot2::geom_path()+
        ggplot2::scale_color_manual(name = 'Model',labels = c(model.names),values = plot.cols)+
        ggplot2::ylab('Group Catch (Tonnes)')+
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
      write.csv(catch.all,file=paste0(plot.out,'Catch_Difference_Data.csv'),row.names = F)
    }
    if(plot.diff){
      write.csv(catch.diff,file = paste0(plot.out,'Model_Comparison_Catch_Data.csv'),row.names = F)
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
