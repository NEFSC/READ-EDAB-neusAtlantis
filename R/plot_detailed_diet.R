#Functions to generate box-level-cohort level realized diets from DetailedDietCheck output



# subset_detailed_diet ----------------------------------------------------

#Function to read, pull, and write subset of large detailed diet check .txt
#pred.prey: 'prey' named group is prey, 'pred' named group is predator
#spp.names: codes for species
#write.new: If TRUE, write new file. If FALSE, write to memory
#out.name: Output filename if writing
#boxes: box numbers
#levels: level numbers
#rm.zero: If TRUE removes all predation values equal to zero
#readAll: If TRUE reads entire DetailedDietCheck.txt into memory. Not recommended if file size >1-2GB
#line.incr: Number of lines to be read at a time if readALL = F

subset_detailed_diet = function(detailed.diet.file,
                                spp.names,
                                pred.prey,
                                boxes = 0:29,
                                levels = -1:3,
                                rm.zero = T,
                                write.new=F,
                                out.name = NA,
                                readAll = F,
                                line.incr = 100000){
  `%>%` = dplyr::`%>%`
  
  if(readAll){
    data = data.table::fread(detailed.diet.file)
    data = data %>%
      filter(Box %in% boxes & Layer %in% levels)
    prey.names = colnames(data)[-c(1:5)]
    if(pred.prey == 'pred'){
      data = data %>% 
        filter(Predator %in% spp.names)
    }else if(pred.prey == 'prey'){
      prey.match = sapply(spp.names,function(x)return(grep(paste0('\\b',x,'\\b'),prey.names)))
      data = data[,c(1:5,5+prey.match)]
    }
    
    if(rm.zero){
      if(pred.prey == 'pred'){
        data = data[,apply(data,2,function(x) return(!all(x==0)))]
      }else if(pred.prey == 'prey'){
        data = data[apply(data[,-c(1:5)],1,function(x) return(!all(x==0))),]
      }else if(pred.prey == 'both'){
        data = data[apply(data[,-c(1:5)],1,function(x) return(!all(x==0))),apply(data,2,function(x) return(!all(x==0)))]
      }
    }
  }else{
    #Determines number of lines in code. Only works on WINDOWS. On LINUX it would be "wc -l filename"
    nline.str = system(paste0('find /c /v "" ',detailed.diet.file),intern = T)[2]
    nline = as.numeric(strsplit(nline.str,' ')[[1]][3])
    line.seq = c(seq(0,nline,line.incr),nline)
    
    data.cols = colnames(data.table::fread(detailed.diet.file,skip = 0,nrow = 1))
    data.slice.ls = list()
    for(i in 1:(length(line.seq)-1)){
      lines2read = line.seq[i+1]-line.seq[i]
      data.slice = data.table::fread(detailed.diet.file,skip = line.seq[i],nrow = lines2read,)
      colnames(data.slice) = data.cols
      data.slice = data.slice %>%
        dplyr::filter(Box %in% boxes & Layer %in% levels)
      prey.names = colnames(data.slice)[-c(1:5)]
      if(pred.prey == 'pred'){
        data.slice = data.slice %>% 
          filter(Predator %in% spp.names)
      }else if(pred.prey == 'prey'){
        prey.match = sapply(spp.names,function(x)return(grep(paste0('\\b',x,'\\b'),prey.names)))
        data.slice = data.slice[,c(1:5,5+prey.match)]
      }
      
      if(rm.zero){
        if(pred.prey == 'pred'){
          data.slice = data.slice[,apply(data.slice,2,function(x) return(!all(x==0)))]
        }else if(pred.prey == 'prey'){
          data.slice = data.slice[apply(data.slice[,-c(1:5)],1,function(x) return(!all(x==0))),]
        }else if(pred.prey == 'both'){
          data.slice = data.slice[apply(data.slice[,-c(1:5)],1,function(x) return(!all(x==0))),apply(data.slice,2,function(x) return(!all(x==0)))]
        }
        print(paste0(round(100*line.seq[i+1]/nline,0),' %'))
      }
      data.slice.ls[[i]] = data.slice
      gc()
    }
    data = dplyr::bind_rows(data.slice.ls)
  }
  
  #add age class data
  age.mat = read.csv(here::here('R','group_mature_age.csv'),as.is = T)
  age.mat = dplyr::filter(age.mat,spp %in% unique(data$Predator))
  mat.index.ls = list()
  for(i in 1:nrow(age.mat)){
    dum = dplyr::filter(data,Predator == age.mat$spp[i])
    n.cohort = length(unique(dum$Cohort))
    mat.index.ls[[i]] = data.frame(Predator = age.mat$spp[i],
                                   Cohort = (1:n.cohort)-1,
                                   age.class = c(rep('juv',(age.mat$age.mat[i]-1)),
                                                 rep('adult',1+n.cohort -age.mat$age.mat[i])))
  }
  mat.index = dplyr::bind_rows(mat.index.ls)
  
  data = data %>%
    left_join(mat.index)
  #turn age.class NA into bio.pool
  age.class.na = which(is.na(data$age.class))
  if(length(age.class.na) != 0){
    data$age.class[age.class.na] = 'bioPool'
  }
  
  
  if(write.new){
    write.table(data, file = out.name,row.names = F)
  }else{
    return(data)
  }
}

# longform_detailed_diet --------------------------------------------------
#Function to transform subsetted DetailedDietCheck into long-format data.frame
#data: output from subset_detailed_diet()
#rm.zero: If TRUE, remove all zero for consumption

longform_detailed_diet = function(data,rm.zero = T){
  data = dplyr::select(data,-Cohort)
  data.new = reshape2::melt(data,id.vars = c('Time','Predator','age.class','Box','Layer'),variable.name = 'Prey',value.name = 'Consumption')
  if(rm.zero){
    data.new = dplyr::filter(data.new, Consumption != 0)
  }
  return(data.new)
}

# data_pred_detailed_diet -------------------------------------------------

#Function to transform longform data into Predator format
#data: output from longform_detailed_diet
#min.fract: Threshold for minimum pct consumption reported (0,1). All under threshold combined into "Rest" group

data_pred_detailed_diet = function(data,min.fract){
  `%>%` = dplyr::`%>%`
  data.new = data %>%
    dplyr::group_by(Time,Predator,age.class,Box,Layer,Prey) %>%
    dplyr::summarise(n = sum(Consumption)) %>%
    dplyr::mutate(pct = n/sum(n)) %>%
    dplyr::mutate(less.min = pct < min.fract)  %>%
    select(-n)
  data.small.pct = data.new %>%
    dplyr::filter(less.min == T) %>%
    dplyr::group_by(Time,Predator,age.class,Box,Layer) %>%
    dplyr::summarize(pct = sum(pct)) %>%
    dplyr::mutate(Prey = 'Rest') %>%
    dplyr::arrange(Time,Predator,age.class,Box,Layer,Prey,pct)
  data.final = data.new %>%
    dplyr::filter(less.min == F) %>%
    dplyr::select(-less.min) %>%
    dplyr::bind_rows(data.small.pct) %>%
    dplyr::arrange(Predator,age.class,Time,Box,Layer,Prey,pct) %>%
    tidyr::tibble() %>%
    tidyr::complete(Prey,tidyr::nesting(Time,Predator,age.class,Box,Layer),fill = list(pct = 0))
  return(data.final)
}

# data_prey_detailed_diet -------------------------------------------------

#Function to transform longform data into Prey format
data_prey_detailed_diet = function(data,min.fract=0.05){
  `%>%` = dplyr::`%>%`
  data.new = data %>%
    dplyr::group_by(Time,Prey,Box,Layer,Predator) %>%
    dplyr::summarize(n = sum(Consumption)) %>%
    dplyr::mutate(pct = n/sum(n)) %>%
    dplyr::mutate(less.min = pct < min.fract)
  data.small.pct = data.new %>%
    dplyr::filter(less.min == T) %>%
    dplyr::group_by(Time,Prey,Box,Layer) %>%
    dplyr::summarize(pct = sum(pct)) %>%
    dplyr::mutate(Predator = 'Rest') %>%
    dplyr::arrange(Time,Prey,Box,Layer,Predator,pct)
  data.final = data.new %>%
    dplyr::filter(less.min == F) %>%
    dplyr::select(-less.min) %>%
    dplyr::bind_rows(data.small.pct) %>%
    dplyr::arrange(Prey,Time,Box,Layer,Predator,pct) %>%
    tidyr::tibble() %>%
    tidyr::complete(Predator,tidyr::nesting(Time,Prey,Box,Layer),fill = list(pct = 0))
}

# plot_detailed_diet ------------------------------------------------------

#Function to create plots, by box, layer


#data: output from either longform_detailed_diet
#min.fract: Threshold for minimum pct consumption reported (0,1). All under threshold combined into "Rest" group
#pred.prey: If 'pred' all prey spp plotted, if 'prey' all pred spp plotted
#plot.spp: Species code(s) you wish to plot

plot_detailed_diet = function(data,plot.spp,min.fract,pred.prey,fig.dir,file.prefix){
  
  #Determine dataset based on pred.prey flag, create pred/prey spp names 
  if(pred.prey == 'pred'){
    data.new = data_pred_detailed_diet(data,min.fract = 0.05)
  } else{
    data.new = data_prey_detailed_diet(data,min.fract = 0.05)
  }
  
  boxes = unique(data.new$Box)
  
  #Loop through species
  for(i in 1:length(plot.spp)){
    
    if(pred.prey == 'pred'){
      data.spp = dplyr::filter(data.new,Predator == plot.spp[i])
      
      #Identify all groups who have zero consumption values across all times/box/layers
      which.zero = data.spp %>%
        dplyr::group_by(Prey) %>%
        dplyr::summarize(tot = sum(pct,na.rm=T)) %>%
        dplyr::mutate(all.zero = ifelse(tot==0,T,F)) %>%
        dplyr::filter(all.zero == T)
      which.zero = as.character(which.zero$Prey)
      
      #Remove zero consumption spp
      data.spp = data.spp %>%
        filter(!(Prey %in% which.zero))
     
      age.groups = unique(data.spp$age.class)
      for(a in 1:length(age.groups)){
        filename = paste0(fig.dir,file.prefix,plot.spp[i],'_',age.groups[a],'_Pred.pdf')
        pdf(file = filename,width = 16, height = 8, onefile = T)
        for(b in 1:length(boxes)){
          
          data.spp.box = data.spp %>% dplyr::filter(Box == boxes[b] & age.class == age.groups[a] )
          if(nrow(data.spp.box)==0){next()}
          
          #Plot prey
          fig = ggplot2::ggplot(data.spp.box, ggplot2::aes(x= Time, y = pct, fill = Prey))+
            ggplot2::geom_area(alpha = 0.6, size = 0.25, color = 'black')+
            ggplot2::ggtitle(paste0('Box ',boxes[b]))+
            ggplot2::xlab('Day')+
            ggplot2::ylab('% Consumption')+
            ggplot2::facet_grid(Layer ~ .,labeller = ggplot2::label_both)+
            ggplot2::theme_classic()+
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
          gridExtra::grid.arrange(fig)
        }
        dev.off()
      }
    } else if(pred.prey == 'prey') {
      data.spp = dplyr::filter(data.new,Prey == plot.spp[i])
      
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
      
      filename = paste0(fig.dir,file.prefix,plot.spp[i],'_Prey.pdf')
      pdf(file = filename,width = 16, height = 8, onefile = T)
      for(b in 1:length(boxes)){
        
        data.spp.box = data.spp %>% dplyr::filter(Box == boxes[b])
        if(nrow(data.spp.box)==0){next()}
        
        #Plot prey
        fig = ggplot2::ggplot(data.spp.box, ggplot2::aes(x= Time, y = pct, fill = Predator))+
          ggplot2::geom_area(alpha = 0.6, size = 0.25, color = 'black')+
          ggplot2::ggtitle(paste0('Box ',boxes[b]))+
          ggplot2::xlab('Day')+
          ggplot2::ylab('% Consumption')+
          ggplot2::facet_grid(Layer ~ .,labeller = ggplot2::label_both)+
          ggplot2::theme_classic()+
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        gridExtra::grid.arrange(fig)
      }
      dev.off()
    }

  }
}

# Example -----------------------------------------------------------------

#Example Params
detailed.diet.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_DetailedDiet2/neus_outputDetailedDietCheck.txt'
out.name = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_DetailedDiet2/neus_outputDetailedDietCheck_2.txt'

subset_detailed_diet(detailed.diet.file,
                     spp.names = c('MAK','HER','BO'),
                     pred.prey = 'both',
                     boxes = 0:9,
                     levels = -1:3,
                     rm.zero = T,
                     write.new = T,
                     out.name = out.name,
                     readAll= F,
                     line.incr = 1E5)
gc()
data.orig = data.table::fread(out.name)
data2 = longform_detailed_diet(data.orig,rm.zero = T)
plot_detailed_diet(data = data2,
                   plot.spp = c('MAK','HER'),
                   fig.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_DetailedDiet2/Figures/Detailed_Diet/',
                   file.prefix = 'test_',
                   pred.prey = 'prey')
plot_detailed_diet(data = data2,
                   plot.spp = c('MAK','HER'),
                   fig.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_DetailedDiet2/Figures/Detailed_Diet/',
                   file.prefix = 'test_',
                   pred.prey = 'pred')


