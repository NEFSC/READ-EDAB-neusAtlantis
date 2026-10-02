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

# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/'
# file.pattern = 'roms_ltl_force_*'
# force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/'
# file.pattern = '^salt.*\\.nc$'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/'
# time.group = 'ym'
# plot.region = T

plot_force_timeavg = function(force.dir, file.pattern, plot.dir, time.group = 'ym', plot.region = NULL){
  
  `%>%` = dplyr::`%>%`
  
  source(here::here('R','flatten_force.R'))
  file.names = list.files(force.dir,file.pattern)
  years = as.numeric(sort(gsub(".*(\\d{4}).+","\\1",file.names)))
  file.prefix = strsplit(file.names[1],paste0('[',years[1],']+'))[[1]][1]
  
  dumm.nc = ncdf4::nc_open(paste0(force.dir,file.names[1]))
  var.names = names(dumm.nc$var)
  ncdf4::nc_close(dumm.nc)
  
  data.ls = list()
  for(f in 1:length(file.names)){
    data.ls[[f]] = flatten_force(force.dir,file.names[f])
    print(f)
  }
  data.full = dplyr::bind_rows(data.ls)
  save(data.full, file = paste0(plot.dir,file.prefix,'Forcing_Data_Flat.R'))
  # load(paste0(plot.dir,file.prefix,'Forcing_Data_Flat.R'))
  
  if(plot.region){
    data.full = data.full %>% dplyr::filter(box %in% 1:22)
    data.full$region = 'GM'
    data.full$region[which(data.full$box %in% 1:7)]='MAB'
  }
  
  # data.full$month = as.numeric(data.full$month)
  # data.full$year = as.numeric(data.full$year)
  
  group.m = function(data){
    data$month = as.numeric(data$month)
    if(plot.region){
      data.month = data.full %>% dplyr::group_by(var.name,month,region,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value)))
    } else{
      data.month = data.full %>% dplyr::group_by(var.name,month,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value)))
    }
    return(data.month)
  }
  group.ym = function(data){
    if(plot.region){
      data.year.month = data.full %>% dplyr::group_by(var.name,year,month,region,alt.level) %>%
        dplyr::summarize(value.mu = mean(value),value.se = sd(value)/sqrt(length(value))) %>%
        tidyr::unite('year.month',year:month,remove=F)
    } else{
      data.year.month = data.full %>% dplyr::group_by(var.name,year,month,alt.level) %>%
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
    stop('Select a "m" or "ym" for grouping code')
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
    # data$year.month = as.numeric(as.factor(data$year.month))
    data$year.month = as.Date(format(paste0(data$year.month,'_01'),format = '%Y_%m_$d' ), format = '%Y_%m_%d')
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
      ggplot2::geom_vline(xintercept = as.Date(paste0(years,'-01-01'),format = '%Y-%m-%d'),size = 0.25,lty = 3)+
      ggplot2::facet_grid(alt.level~.,labeller = ggplot2::label_both)+
      ggplot2::geom_ribbon(alpha=0.5)+
      # ggplot2::scale_x_continuous(breaks = seq(1,max(data$year.month),1),labels = ym.labels)+
      ggplot2::ylab(variable)+
      ggplot2::xlab('Time')+
      ggplot2::theme_minimal()

    return(g)
  }
  
  pdf(file = paste0(plot.dir,file.prefix,'Climatology_',time.group,'.pdf'),width =14,onefile = T)
  for(v in 1:length(var.names)){
    
    if(time.group == 'm'){
      g.var = plot.m(data.group,var.names[v])
    }else if(time.group == 'ym'){
      g.var = plot.ym(data.group, var.names[v])
    }else{
      stop('Select a "m" or "ym" for grouping code')
    }
    g.var = g.var + ggplot2::ggtitle(var.names[v])
    print(var.names[v])
    gridExtra::grid.arrange(g.var)
  }
  dev.off()
  
}

# plot_force_timeavg(
#   force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/phys_statevars/',
#   file.pattern = '^salt.*\\.nc$',
#   plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing Aggregated Timeseries/',
#   time.group = 'ym',
#   plot.region = T
# )


# forcing.climatology(
#   force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_COBALT output/ltl_statevars/',
#   file.pattern = 'roms_ltl_force_*',
#   plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Forcing_Climatology/',
#   time.group = 'ym',
#   plot.region = T
# )

