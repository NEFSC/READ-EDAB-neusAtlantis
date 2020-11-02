#Function to create figures showing total predation based on biomass consumed and not split by age class
## Modified from plot_detailed_diet.R


# Gets total consumption for all groups -----------------------------------

get_consumption = function(prod.file,fgs.file){
  
  prod.nc = ncdf4::nc_open(prod.file)
  prod.vars = names(prod.nc$var)
  groups = read.csv(fgs.file,as.is = T)
  time.vals = as.Date(as.POSIXct(prod.nc$dim$t$vals,origin = '1964-01-01 00:00:00',tz = 'UTC'))
  time.days = as.numeric(difftime(time.vals,as.Date('1964-01-01'),units = 'd'))
  
  consumption.all.ls = list()
  for(i in 1:nrow(groups)){
    prod.vars.group = prod.vars[grep(groups$Name[i],prod.vars)]
    group.eat = prod.vars.group[c(grep('Eat',prod.vars.group),grep('Grazing',prod.vars.group))]
    
    if(length(group.eat) == 0){
      next()
    }
    group.df.ls = list()
    for(v in 1:length(group.eat)){
      age.var = colSums(ncvar_get(prod.nc,group.eat[v]))  
      if(grepl('_Eat',group.eat[v])){
        group.cohort = as.numeric(strsplit(group.eat[v],paste0(groups$Name[i],'|_Eat'))[[1]][2])-1
        group.df.ls[[v]] =data.frame(Predator = groups$Code[i], 
                                     Cohort = group.cohort,
                                     Time = time.days,
                                     date = time.vals,
                                     Consumption = age.var,
                                     stringsAsFactors = F)
      }else{
        group.df.ls[[v]] =data.frame(Predator = groups$Code[i], 
                                     Cohort = 0,
                                     Time = time.days,
                                     date = time.vals,
                                     Consumption = age.var,
                                     stringsAsFactors = F)
      }
    }
    consumption.all.ls[[i]] = dplyr::bind_rows(group.df.ls)
  }
  consumption.all = dplyr::bind_rows(consumption.all.ls)
  return(consumption.all)
}


# subset_diet -------------------------------------------------------------
#Reads in diet data, transforms to longform, adds total consumption, and subsets

subset_diet = function(diet.file,consumption, spp.names){
  `%>%` = dplyr::`%>%`
  data = data.table::fread(diet.file) %>%
    dplyr::select(-Stock,-Updated)
  #Convert Data to long format
  data.long = reshape2::melt(data,id.vars = c('Time','Predator','Cohort'),variable.name = 'Prey',value.name = 'consumed.prop')%>%
    left_join(consumption, by = c('Time','Predator','Cohort')) %>%
    dplyr::mutate(consumed.quant = Consumption * consumed.prop) %>%
    dplyr::filter(Prey %in% spp.names & consumed.quant != 0) %>%
    dplyr::group_by(Time,date,Prey,Predator) %>%
    dplyr::summarize(consumed.prey = sum(consumed.quant,na.rm=T))

  return(data.long)
}


# plot_overall_predation --------------------------------------------------
#Overall predation is the sum of all consumed biomass from a prey group
#Need to calculate the relative contribution of each predator
#Consolidate groups below threshold
#Differs from existing plots since it doesn't differentiate age class or functional group type

plot_overall_predation =function(data,bioindex.file,catch.file,min.fract = 0.1,fig.dir,file.prefix){
  
  
  #Collapse small contributors into "Rest"
  data.tot = data %>%
    dplyr::group_by(Time,date,Prey) %>%
    dplyr::summarize(consumed.tot = sum(consumed.prey,na.rm=T))
  
  data.new = data %>%
    dplyr::left_join(data.tot)%>%
    # dplyr::group_by(Time,date,Prey,Predator) %>%
    dplyr::mutate(consumed.pct = consumed.prey/consumed.tot,
                  less.min = consumed.pct < min.fract) 
  
  data.small.pct = data.new %>%
    dplyr::filter(less.min == T) %>%
    dplyr::group_by(Time,date,Prey) %>%
    dplyr::summarize(consumed.pct = sum(consumed.pct)) %>%
    dplyr::mutate(Predator = 'Rest') %>%
    dplyr::arrange(Time,Prey,Predator,consumed.pct)
  
  data.final = data.new %>%
    dplyr::filter(less.min == F) %>%
    dplyr::select(-less.min) %>%
    dplyr::bind_rows(data.small.pct) %>%
    dplyr::arrange(Prey,Time,date,Predator,consumed.pct) %>%
    tidyr::tibble() %>%
    tidyr::complete(Predator,tidyr::nesting(Time,date,Prey),fill = list(consumed.pct = 0,consumed.prey = 0, consumed.tot = 0))
    
  #Get Biomass Data
  biomass.data = read.table(bioindex.file,header = T,stringsAsFactors = F)
  biomass.colnames = colnames(biomass.data)
  
  #Get catch Data
  catch.data = read.table(catch.file, header = T, stringsAsFactors = F)
  catch.colnames = colnames(catch.data)
  
  #Loop through species
  plot.cols = c(RColorBrewer::brewer.pal(12,'Set3'),
                RColorBrewer::brewer.pal(8,'Dark2'),
                RColorBrewer::brewer.pal(8,'Set2'),
                RColorBrewer::brewer.pal(9,'Set1'))
  
  plot.spp = sort(unique(as.character(data.new$Prey)))
  
  filename = paste0(fig.dir,file.prefix,'_TotalConsumption.pdf')
  pdf(file = filename,width = 16, height = 8, onefile = T)
  
  for(i in 1:length(plot.spp)){
    
    data.spp = dplyr::filter(data.final,Prey == plot.spp[i])
    
    #Identify all groups who have zero consumption values across all times/box/layers
    which.zero = data.spp %>%
      dplyr::group_by(Predator) %>%
      dplyr::summarize(tot = sum(consumed.pct,na.rm=T)) %>%
      dplyr::mutate(all.zero = ifelse(tot==0,T,F)) %>%
      dplyr::filter(all.zero == T)
    which.zero = as.character(which.zero$Predator)
    
    #Remove zero consumption spp
    data.spp = data.spp %>%
      filter(!(Predator %in% which.zero))
    
    #Total consumption
    data.tot.spp = data.tot %>% filter(Prey == plot.spp[i])
    
    #Biomass Timeseries
    biomass.spp = biomass.data[,c(1,grep(paste0('\\b',plot.spp[i],'\\b'),biomass.colnames))]
    colnames(biomass.spp)[2] = 'value'
    biomass.spp$Metric = 'biomass'
    biomass.spp$date = as.POSIXct(biomass.spp$Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC')
    
    #Catch Timeseries
    catch.match = grep(paste0('\\b',plot.spp[i],'\\b'),catch.colnames)
    if(length(catch.match) == 0){
      dum.dat = biomass.spp
      dum.dat$value = NA
      dum.dat$Metric = 'catch'
      bio.catch.data = rbind(biomass.spp, dum.dat )  
    }else{
      catch.spp = catch.data[,c(1,catch.match)]
      colnames(catch.spp)[2] = 'value'
      catch.spp$Metric = 'catch'
      catch.spp$date = as.POSIXct(catch.spp$Time*86400,origin = '1964-01-01 00:00:00',tz = 'UTC')
      
      #Combine Bio Catch
      bio.catch.data = rbind(biomass.spp, catch.spp)
    }

    
    #Plot prey
 
    f1=ggplot2::ggplot(data.spp, ggplot2::aes(x= date, y = consumed.pct, fill = Predator))+
        ggplot2::geom_area(alpha = 0.7, size = 0.25, color = 'black')+
        ggplot2::scale_fill_manual(values = plot.cols[1:length(unique(data.spp$Predator))])+
        ggplot2::xlab('date')+
        ggplot2::ylab('% Total Consumption')+
        ggplot2::theme_classic()+
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))+
      ggplot2::theme(legend.position = 'bottom')
        
    f2 = ggplot2::ggplot(data.tot.spp, ggplot2::aes(x = date, y = consumed.tot),size = 1.5)+
      ggplot2::geom_line()+
      ggplot2::xlab('')+
      ggplot2::ylab('Total consumption (mg N m-3 d-1)')+
      ggplot2::theme_classic()
    

    f3=ggplot2::ggplot(bio.catch.data,ggplot2::aes(x=date,y=value, lty = Metric))+
      ggplot2::geom_line(size = 1)+
      ggplot2::scale_linetype(name = '')+
      ggplot2::xlab('')+
      ggplot2::ylab('Value (tonnes)')+
      ggplot2::ggtitle(plot.spp[i])+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     legend.position = 'bottom')

    
    grid::grid.newpage()
    grid::grid.draw(gtable:::rbind.gtable(ggplot2::ggplotGrob(f3),ggplot2::ggplotGrob(f2),ggplot2::ggplotGrob(f1),size = 'last'))
      
  }
  dev.off()
}

#Example
# atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ReducePred11/'
# prod.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ReducePred11/neus_outputPROD.nc'
# fgs.file = here::here('currentVersion','neus_groups.csv')
# consumption = get_consumption(prod.file,fgs.file)
# data.sub = subset_diet(diet.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ReducePred11/neus_outputDietCheck.txt',
#                        consumption = consumption,
#                    # spp.names  = c('ZL','ZM','ZS','BO','CLA','SCA','QHG','BFF','BD','LOB','PB','BB','DL','DR'),
#                    spp.names = c('MAK','ZL','PL'))
# plot_overall_predation(data = data.sub,
#                        min.fract = 0.1,
#                        bioindex.file = paste0(atl.dir,'neus_outputBiomIndx.txt'),
#                        catch.file = paste0(atl.dir,'neus_outputCatch.txt'),
#                        fig.dir = paste0(atl.dir,'Figures/'),
#                        file.prefix = 'Obs_Hindcast_DLFix2')
