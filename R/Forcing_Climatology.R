#' Generates climatology figures for all state variables from forcing files
#' 
#' Uses the forcing files for the NEUS Atlantis model to generate climatological data
#' on physical and biological state variables. All data converted to a longformat table,
#' saved, and aggregated on a month and year-by-month grouping. Temperature, salinity,
#' and lower trophic biomass all all plotted. Includes the option to aggregate data over
#' entire domain or to split by user-defined region column (MAB and GoM for NEUS).
#' 
#' @force.dir string. path to forcing files
#' @file.pattern  string. file name pattern to be searched by
#' @plot.dir string. path where files are saved to
#' @time.group string. 'm' for monthly aggregates, 'ym' for year-month aggregates
#' @plot.region logical. Whether to split data by region (MAB and GoM)
#' 
#' @return Figures of all variable within forcing NC files
#' 
#' Author: J. Caracappa

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'
file.pattern = 'roms_ltl_force_*'
plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/'
time.group = 'ym'
plot.region = T

forcing.climatology = function(force.dir, file.pattern, plot.dir, time.group = 'ym', plot.region = NULL){
  
  `%>%` = dplyr::`%>%`
  
  source(here::here('R','forcing_longform.R'))
  file.names = list.files(force.dir,file.pattern)
  years = as.numeric(sort(gsub(".*_(\\d{4}).+","\\1",file.names)))
  
  dumm.nc = ncdf4::nc_open(paste0(force.dir,file.names[1]))
  var.names = names(dumm.nc$var)
  ncdf4::nc_close(dumm.nc)
  
  data.ls = list()
  for(f in 1:length(file.names)){
    data.ls[[f]] = make.force.long(force.dir,file.names[f])
    print(f)
  }
  data.full = dplyr::bind_rows(data.ls)
  save(data.full, file = paste0(plot.dir,'Forcing_Data_Flat.R'))
  load(paste0(plot.dir,'Forcing_Data_Flat.R'))
  
  if(plot.region){
    data.full = data.full %>% filter(box %in% 1:22)
    data.full$region = 'GM'
    data.full$region[which(data.full$box %in% 1:7)]='MAB'
  }
  
  data.full$month = as.numeric(data.full$month)
  data.full$year = as.numeric(data.full$year)
  
  group.m = function(data){
    if(plot.region){
      data.month = data.full %>% group_by(var.name,month,region,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value)))
    } else{
      data.month = data.full %>% group_by(var.name,month,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value)))
    }
    return(data.month)
  }
  group.ym = function(data){
    if(plot.region){
      data.year.month = data.full %>% group_by(var.name,year,month,region,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value))) %>%
        tidyr::unite('year.month',year:month,remove=F)
    } else{
      data.year.month = data.full %>% group_by(var.name,year,month,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value))) %>%
        tidyr::unite('year.month',year:month,remove=F)
    }
    return(data.year.month)
  }
  
  if(time.group == 'm'){
    data.group = group.m(data.full)
  }else if(time.group == 'ym'){
    data.group = group.ym(data.full)
  }else{
    stop('Select a "m" or "my" for grouping code')
  }
  
  plot.m = function(data,variable){
    data = data %>% dplyr::filter(var.name == variable)
    if(plot.region){
      g= ggplot2::ggplot(data, ggplot2::aes(x = month, y = value.mu,color = region,
                                            fill = region,ymin = value.mu - value.se, ymax = value.mu+value.se))+
        ggplot2::scale_color_manual(name = 'Region',values = c('Red3','Blue3'))+
        ggplot2::scale_fill_manual(values = c('Red3','Blue3'))+
        ggplot2::guides(fill = F)
    }else{
      g = ggplot2::ggplot(data, ggplot2::aes(x = month, y = value.mu,ymin = value.mu - value.se, ymax = value.mu+value.se))
    }
    g=g+ggplot2::geom_line()+
      ggplot2::facet_grid(alt.level~.,labeller = ggplot2::label_both)+
      ggplot2::geom_ribbon(alpha=0.5)+
      ggplot2::ylab(variable)+
      ggplot2::xlab('Time')
    return(g)
  }
  
  plot.ym = function(data,variable){
    data = data %>% dplyr::filter(var.name == variable)
    ym.labels = unique(data$year.month)
    data$year.month = as.numeric(as.factor(data$year.month))
    if(plot.region){
      g= ggplot2::ggplot(data, ggplot2::aes(x = year.month, y = value.mu,color = region,
                                            fill = region,ymin = value.mu - value.se, ymax = value.mu+value.se))+
        ggplot2::scale_color_manual(name = 'Region',values = c('Red3','Blue3'))+
        ggplot2::scale_fill_manual(values = c('Red3','Blue3'))+
        ggplot2::guides(fill = F)
    }else{
      g = ggplot2::ggplot(data, ggplot2::aes(x = year.month, y = value.mu,ymin = value.mu - value.se, ymax = value.mu+value.se))
    }
    g=g+ggplot2::geom_line()+
      ggplot2::facet_grid(alt.level~.,labeller = ggplot2::label_both)+
      ggplot2::geom_ribbon(alpha=0.5)+
      scale_x_continuous(breaks = seq(1,max(data$year.month),1),labels = ym.labels)+
      ggplot2::ylab(variable)+
      ggplot2::xlab('Time')
    return(g)
  }
  
  png(file = paste0(plot.dir,''))
  for(v in 1:length(var.names)){
    
    if(time.group == 'm'){
      g.var = plot.m(data.group,var.names[v])
    }else if(time.group == 'ym'){
      g.var = plot.ym(data.group, var.names[v])
    }else{
      stop('Select a "m" or "my" for grouping code')
    }
    print(var.names[v])
  }
  
}
