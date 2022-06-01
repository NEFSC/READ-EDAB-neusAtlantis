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



# model1.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/New_CatchTS_6536/'
# model2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/New_CatchTS_6490/'
# model1.name = 'A'
# model2.name = 'B'
# plot.raw = T
# plot.diff = T
# plot.out ='C:/Users/joseph.caracappa/Documments/Atlantis/Obs_Hindcast/Diagnostic_Figures/Run_Catch_Comparisons/'
# table.out = T
# groups = c('HER','CLA','LOB')
# groups = NULL

plot_run_catch_comparisons = function(model1.dir,model2.dir,model1.name,model2.name,plot.raw = T,
                                plot.diff = T, plot.out, table.out = F, groups = NULL,remove.init = F){
  `%>%` = dplyr::`%>%`
  
  model1.files = sort(list.files(model1.dir,'*.nc'))
  model2.files = sort(list.files(model2.dir,'*.nc'))
  
  model1.prefix = strsplit(model1.files[1],'\\.')[[1]][1]
  model2.prefix = strsplit(model2.files[1],'\\.')[[1]][1]
  
  model1.catch = paste0(model1.prefix,'Catch.txt')
  model2.catch = paste0(model2.prefix, 'Catch.txt')
  
  catch1 = read.table(file = paste0(model1.dir,model1.catch),header = T)
  catch2 = read.table(file = paste0(model2.dir,model2.catch),header = T)
  
  fgs = colnames(catch1)[2:ncol(catch1)]
  fgs = fgs[-grep('TsAct',fgs)]
  
  #If specifying groups, subset only those
  if(!is.null(groups)){
    catch1 = subset(catch1,select = c('Time',groups))
    catch2 = subset(catch2,select = c('Time',groups))
  } else{
    groups1 = fgs[fgs %in% colnames(catch1)]
    groups2 = fgs[fgs %in% colnames(catch2)]
    catch1 = dplyr::select(catch1,Time,tidyselect::all_of(groups1))
    catch2 = dplyr::select(catch2,Time,tidyselect::all_of(groups2))
  }
  
  #convert each model to long format
  catch1.long = tidyr::gather(catch1,key = 'Group','Catch',-Time)
  catch2.long = tidyr::gather(catch2,key = 'Group','Catch',-Time)
  
  catch1.long$Model = 'model1'
  catch2.long$Model = 'model2'
  
  nc1 = ncdf4::nc_open(paste0(model1.dir,model1.prefix,'.nc'))
  nc2 = ncdf4::nc_open(paste0(model2.dir,model2.prefix,'.nc'))
  
  t.start1 = strsplit(ncdf4::ncatt_get(nc1,'t')$units,'\\ ')[[1]][3]
  t.start2 = strsplit(ncdf4::ncatt_get(nc2,'t')$units,'\\ ')[[1]][3]
  
  ncdf4::nc_close(nc1)
  ncdf4::nc_close(nc2)
  
  catch1.long$Real.Time = as.Date(catch1.long$Time,origin = t.start1)
  catch2.long$Real.Time = as.Date(catch2.long$Time,origin = t.start2)
  
  catch.all = rbind(catch1.long,catch2.long)
  
  if(remove.init){
    catch.all = catch.all %>% dplyr::filter(Time != 0)
  }
  
  if(is.null(groups)){
    plot.groups = sort(unique(catch.all$Group))  
  }else{
    plot.groups = groups
  }
  
  if(plot.diff){
    
    if(t.start1 != t.start2){
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
        ggplot2::scale_color_manual(name = 'Model',labels = c(model1.name,model2.name),values = c('red3','blue3'))+
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
