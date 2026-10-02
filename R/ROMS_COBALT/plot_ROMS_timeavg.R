#' Create Climatology Plots for ROMS Data
#' 
#' Creates climatology timeseries for fluxes and state variables based 
#' on the aggregated ROMS data on the Atlantis domain structure. There 
#' are two main options. The first is to summarize model output on a yearly,
#' seasonal, or monthly basis. The Second is to condense timeseries into a
#' single seasonal or monthly aggregate. Choice of error calculation (stdev, SE, 
#' or CI) is possible. Option to plot data as fully aggregated "aggregated" where
#' all years are consolidated by a single temporal scale (monthly or seasonally) or 
#' can be plotted "sequentially" where each year is aggregated on seasonal or monthly
#' timescales and all years are plotted sequentially.
#' 
#' @roms.dir string. Location of roms output files (ncdf)
#' @roms.prefix string. Prefix of roms output files
#' @output.dir string. Location for gnereated output files
#' @output.prefix string. Prefix for generated output files
#' @plot.transport logical. Plot transport?
#' @plot.statevars logical. Plot state variables (T,S, vertflux)?
#' @plot.ltlvars logical. Plot lower trophic level variables(phytoplankton and zooplankton biomass)?
#' @plot.monthly logical. plot values as monthly averages?
#' @plot.seasonal logical. Plot values as seasonal
#' @plot.type string. Specify whether to plot timeseries as fully aggregated to one year ("aggregate") or just grouped by specified timeframe ("sequential") or"both"
#' @error.type string. Specify whether error bars should be standard deviation "SD", standard error "SE", or confidence intervals "CI" 
#' @stat.type string. Specify whether to aggregate variables by "mean" or "median"
#' @export.tables logical. Whether to save output as table
#' 
#' Author: J Caracappa

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing Files/'
# output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Climatology/'
# roms.prefix = 'roms_'
# output.prefix = 'roms_allyears_'
# bgm.file = 'neus_ll_WGS84.bgm'
# plot.transport = T
# plot.statevars = T
# plot.ltlvars = T
# plot.monthly = T
# plot.seasonal = T
# plot.type = 'both'
# error.type = 'SE'
# stat.type = 'mean'
# export.tables = T

plot_ROMS_timeavg = function(roms.dir,roms.prefix,output.dir, output.prefix,
                            plot.transport,plot.statevars,plot.ltlvars,plot.monthly,plot.seasonal,
                            plot.type, error.type,stat.type,export.tables){
  
  `%>%` = dplyr::`%>%`
  
  nc.files = list.files(roms.dir,pattern = '*.nc',full.names = T)
  transport.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
  statevars.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='statevars') & all(strsplit(x,'[_.]+')[[1]] != 'ltl')))]
  ltlvars.file = nc.files[which(sapply(nc.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'ltl')))]
  
  source(here::here('R','flatten_ROMS.R'))

  #Functions for confidence intervals
  
  #Function to generate data.frame with time needed temporal variables
  format.time = function(data,plot.seasonal){
    DF = data[,1:4]
    colnames(DF)[4] = 'variable'
    DF$year = format(DF$time,format = '%Y')
    DF$month = format(DF$time,format = '%m')
    DF$month.name = format(DF$time,format = '%B')
    if(plot.seasonal){
      DF$season = sapply(DF$month,function(x){
      if(x %in% c('12','01','02')){
        return(1)
      }else if(x %in% c('03','04','05')){
        return(2)
      }else if(x %in% c('06','07','08')){
        return(3)
      }else if(x %in% c('09','10','11')){
        return(4)
      }else {
        return(NA)
      }})
    }
    return(DF)
  }
    
  #Function to make "monthly" "aggregate"
  month.aggregate = function(data,is.hflux){
    if(is.hflux){
      DF = data %>% dplyr::group_by(month,face,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    } else {
      DF = data %>% dplyr::group_by(month,box,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    }
  }
  
  #Function to make "seasonal" "aggregate"
  season.aggregate = function(data, is.hflux){
    if(is.hflux){
      DF = data %>% dplyr::group_by(season,face,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    } else {
      DF = data %>% dplyr::group_by(season,box,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    }
  }
  
  #Function to make "monthly" "sequential"
  month.sequential = function(data,is.hflux){
    if(is.hflux){
      DF = data %>% dplyr::group_by(year,month,face,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    } else {
      DF = data %>% dplyr::group_by(year,month,box,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    }
  }
  
  #Function to make "seasonal" "sequential"
  season.sequential = function(data,is.hflux){
    if(is.hflux){
      DF = data %>% dplyr::group_by(year,season,face,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    } else {
      DF = data %>% dplyr::group_by(year,season,box,level) %>%
        dplyr::summarize(count = dplyr::n(),
                         var.mean = mean(variable,na.rm=T),
                         var.median = median(variable,na.rm=T),
                         var.sd = sd(variable,na.rm=T),
                         var.mean.ci.lwr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[2],
                         var.mean.ci.upr = Hmisc::smean.cl.normal(variable,conf.int = 0.95)[3],
                         var.med.ci.lwr = quantile(variable,0.025,na.rm=T),
                         var.med.ci.upr = quantile(variable,0.975,na.rm=T)) %>%
        dplyr::mutate(var.se = var.sd/sqrt(count))
    }
  }
  #Function to aggregate by season
  
  #Function to plot 'aggregate' data
  plot.aggregate = function(data,error.type,stat.type,space.id,var.name,time.name,title){
    DF = data
    colnames(DF)[1:3] = c('time.group','space.group','level')
    DF$level = as.factor(DF$level)
    DF = DF[which(DF$space.group == space.id),]
    if(error.type == 'SD'){
      error.name = 'var.sd'
    }else if(error.type == 'SE'){
      error.name = 'var.se'
    }else if(error.type =='CI' & stat.type == 'mean'){
      error.name = c('var.mean.ci.lwr','var.mean.ci.upr')
    }else if(error.type == 'CI' & stat.type == 'median'){
      error.name = c('var.med.ci.lwr','var.med.ci.upr')
    }
    if(stat.type == 'mean'){
      stat.name = 'var.mean'
    }else if(stat.type == 'median'){
      stat.name = 'var.median'
    }
    DF = DF[,c('time.group','space.group','level',stat.name,error.name)]
    if(error.type == 'CI'){
      colnames(DF) = c('time.group','space.group','level','var','var.min','var.max')
    } else{
      colnames(DF) = c('time.group','space.group','level','var','var.error')
      DF$var.min = DF$var - DF$var.error
      DF$var.max = DF$var + DF$var.error
    }
    ggplot2::ggplot(data =DF, ggplot2::aes(x=time.group, y = var,color = level,fill = level,group = level,ymin= var.min, ymax = var.max)) +
      ggplot2::geom_ribbon(alpha = 0.5,col = 'grey80')+
      # ggplot2::geom_point(size = 2)+
      ggplot2::geom_path()+
      ggplot2::theme_minimal()+
      ggplot2::xlab(time.name)+
      ggplot2::ylab(var.name)+
      ggplot2::ggtitle(title)+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
  
  #Function to plot 'sequential' data
  plot.sequential = function(data,error.type,stat.type,space.id,var.name,time.name,title){
    DF = data
    colnames(DF)[1:4] = c('year','time.group','space.group','level')
    DF$level = as.factor(DF$level)
    DF = DF[which(DF$space.group == space.id),]
    DF = DF %>% tidyr::unite('time.var',year:time.group,sep = '-', remove = F)
    if(error.type == 'SD'){
      error.name = 'var.sd'
    }else if(error.type == 'SE'){
      error.name = 'var.se'
    }else if(error.type =='CI' & stat.type == 'mean'){
      error.name = c('var.mean.ci.lwr','var.mean.ci.upr')
    }else if(error.type == 'CI' & stat.type == 'median'){
      error.name = c('var.med.ci.lwr','var.med.ci.upr')
    }
    if(stat.type == 'mean'){
      stat.name = 'var.mean'
    }else if(stat.type == 'median'){
      stat.name = 'var.median'
    }
    DF = DF[,c('time.var','year','time.group','space.group','level',stat.name,error.name)]
    if(error.type == 'CI'){
      colnames(DF) = c('time.var','year','time.group','space.group','level','var','var.min','var.max')
    } else{
      colnames(DF) = c('time.var','year','time.group','space.group','level','var','var.error')
      DF$var.min = DF$var - DF$var.error
      DF$var.max = DF$var + DF$var.error
    }
    DF$time.var = as.Date(paste0(DF$time.var,'-01'),'%Y-%m-%d')
    decade = round(as.numeric(DF$year),-1)
    ggplot2::ggplot(data =DF, ggplot2::aes(x=time.var, y = var,color = level,fill = level,group = level,ymin= var.min, ymax = var.max)) +
      ggplot2::geom_ribbon(alpha = 0.5,col = 'grey80')+
      # ggplot2::geom_point(size = 2)+
      ggplot2::geom_path()+
      ggplot2::scale_x_date(date_labels = '%Y')+
      ggplot2::geom_vline(xintercept = as.numeric( as.Date(paste0(seq(min(decade),max(decade),10),'-01-01'),'%Y-%m-%d')),lty =2 )+
      ggplot2::theme_minimal()+
      ggplot2::xlab(time.name)+
      ggplot2::ylab(var.name)+
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )+
      ggplot2::ggtitle(title)
  }
  
  if(plot.transport){
    
    transport.df = roms2long(nc.file = transport.file, is.hflux = T)
    faces = sort(unique(transport.df$face))
    
    if(plot.monthly){
      transport.monthly = format.time(transport.df,plot.seasonal=F)
        
      if(plot.type %in% c('aggregate','both')){
        dumm = month.aggregate(transport.monthly,is.hflux = T)
        if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_monthly_transport_aggregated.csv'),row.names = F)}
        g.ls = list()
        for(i in 1:length(faces)){
          g.ls[[i]] = plot.aggregate(data = dumm, error.type,stat.type,space.id = faces[i],
                                      var.name = expression('Transport ('~m^3~s^-1~')'),
                                     time.name = 'Month',title = paste0('Face',faces[i]))
        }
        pdf(file = paste0(output.dir,output.prefix,'_montly_transport_aggregated.pdf'),onefile = T,width = 14)
        for(i in 1:length(faces)){gridExtra::grid.arrange(g.ls[[i]])}
        dev.off()
      }
      if(plot.type %in% c('sequential','both')){
        dumm = month.sequential(transport.monthly,is.hflux = T)
        if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_monthly_transport_sequential.csv'),row.names = F)}
        g.ls = list()
        for(i in 1:length(faces)){
          g.ls[[i]] = plot.sequential(data = dumm, error.type,stat.type,space.id = faces[i],
                                     var.name = expression('Transport ('~m^3~s^-1~')'),
                                     time.name = 'Month',title = paste0('Face',faces[i]))
        }
        pdf(file = paste0(output.dir,output.prefix,'_montly_transport_sequential.pdf'),onefile = T,width = 14)
        for(i in 1:length(faces)){gridExtra::grid.arrange(g.ls[[i]])}
        dev.off()
      }
      rm(transport.monthly,dumm)
    }
    if(plot.seasonal){
      transport.seasonal = format.time(transport.df,plot.seasonal=T)
      
      if(plot.type %in% c('aggregate','both')){
        dumm = season.aggregate(transport.seasonal,is.hflux = T)
        if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_seasonal_transport_aggregated.csv'),row.names = F)}
        g.ls = list()
        for(i in 1:length(faces)){
          g.ls[[i]] = plot.aggregate(data = dumm, error.type,stat.type,space.id = faces[i],
                                     var.name = expression('Transport ('~m^3~s^-1~')'),
                                     time.name = 'Season',title = paste0('Face',faces[i]))
        }
        pdf(file = paste0(output.dir,output.prefix,'_seasonal_transport_aggregated.pdf'),onefile = T,width = 14)
        for(i in 1:length(faces)){gridExtra::grid.arrange(g.ls[[i]])}
        dev.off()
      } 
      if(plot.type %in% c('sequential','both')){
        dumm = season.sequential(transport.seasonal,is.hflux = T)
        if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_season_transport_sequential.csv'),row.names = F)}
        g.ls = list()
        for(i in 1:length(faces)){
          g.ls[[i]] = plot.sequential(data = dumm, error.type,stat.type,space.id = faces[i],
                                      var.name = expression('Transport ('~m^3~s^-1~')'),
                                      time.name = 'Season',title = paste0('Face',faces[i]))
        }
        pdf(file = paste0(output.dir,output.prefix,'_seasonal_transport_sequential.pdf'),onefile = T,width = 14)
        for(i in 1:length(faces)){gridExtra::grid.arrange(g.ls[[i]])}
        dev.off()
      }
    }
    
  }
  
  if(plot.statevars){
    
    statevars.df = roms2long(nc.file = statevars.file,is.hflux = F)
    boxes = sort(unique(statevars.df[[1]]$box))
    var.names = names(statevars.df)
    
    nc.dumm = ncdf4::nc_open(statevars.file)
    units = sapply(1:length(var.names),function(x){nc.dumm$var[[x]]$units})
    ncdf4::nc_close(nc.dumm)
    
    for(var in 1:length(var.names)){
      if(plot.monthly){
        statevars.monthly = format.time(statevars.df[[var]],plot.seasonal=F)
        
        if(plot.type %in% c('aggregate','both')){
          dumm = month.aggregate(statevars.monthly,is.hflux = F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_monthly_',var.names[var],'_aggregated.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.aggregate(data = dumm, error.type,stat.type,space.id = boxes[i],
                                       var.name = paste0(var.names[var],'(',units[var],')'),
                                       time.name = 'Month',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_montly_',var.names[var],'_aggregated.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
        } 
        if(plot.type %in% c('sequential','both')){
          dumm = month.sequential(statevars.monthly,is.hflux = F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_monthly_',var.names[var],'_sequential.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.sequential(data = dumm, error.type,stat.type,space.id = boxes[i],
                                        var.name =  paste0(var.names[var],'(',units[var],')'),
                                        time.name = 'Month',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_montly_',var.names[var],'_sequential.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
        }
        rm(statevars.monthly,dumm)
      }
      if(plot.seasonal){
        statevars.seasonal = format.time(statevars.df[[var]],plot.seasonal=T)
        
        if(plot.type %in% c('aggregate','both')){
          dumm = season.aggregate(statevars.seasonal,is.hflux =F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_seasonal_',var.names[var],'_aggregated.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.aggregate(data = dumm, error.type,stat.type,space.id = boxes[i],
                                       var.name = paste0(var.names[var],'(',units[var],')'),
                                       time.name = 'Season',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_seasonal_',var.names[var],'_aggregated.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
        }
        if(plot.type %in% c('sequential','both')){
          dumm = season.sequential(statevars.seasonal,is.hflux = F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_season_',var.names[var],'_sequential.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.sequential(data = dumm, error.type,stat.type,space.id = boxes[i],
                                        var.name = paste0(var.names[var],'(',units[var],')'),
                                        time.name = 'Season',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_seasonal_',var.names[var],'_sequential.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
          rm(statevars.seasonal)
        }
      }
    }
    
    rm(statevars.df)
  }
  
  if(plot.ltlvars){
    statevars.df = roms2long(nc.file = ltlvars.file,is.hflux = F)
    boxes = sort(unique(statevars.df[[1]]$box))
    var.names = names(statevars.df)
    
    nc.dumm = ncdf4::nc_open(ltlvars.file)
    units = sapply(1:length(var.names),function(x){nc.dumm$var[[x]]$units})
    ncdf4::nc_close(nc.dumm)
    
    for(var in 1:length(var.names)){
      if(plot.monthly){
        statevars.monthly = format.time(statevars.df[[var]],plot.seasonal=F)
        
        if(plot.type %in% c('aggregate','both')){
          dumm = month.aggregate(statevars.monthly,is.hflux = F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_monthly_',var.names[var],'_aggregated.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.aggregate(data = dumm, error.type,stat.type,space.id = boxes[i],
                                       var.name = paste0(var.names[var],'(',units[var],')'),
                                       time.name = 'Month',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_montly_',var.names[var],'_aggregated.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
        }
        if(plot.type %in% c('sequential','both')){
          dumm = month.sequential(statevars.monthly,is.hflux = F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_monthly_',var.names[var],'_sequential.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.sequential(data = dumm, error.type,stat.type,space.id = boxes[i],
                                        var.name =  paste0(var.names[var],'(',units[var],')'),
                                        time.name = 'Month',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_montly_',var.names[var],'_sequential.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
        }
        rm(statevars.monthly,dumm)
      }
      if(plot.seasonal){
        statevars.seasonal = format.time(statevars.df[[var]],plot.seasonal=T)
        
        if(plot.type %in% c('aggregate','both')){
          dumm = season.aggregate(statevars.seasonal,is.hflux =F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_seasonal_',var.names[var],'_aggregated.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.aggregate(data = dumm, error.type,stat.type,space.id = boxes[i],
                                       var.name = paste0(var.names[var],'(',units[var],')'),
                                       time.name = 'Season',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_seasonal_',var.names[var],'_aggregated.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
        }
        if(plot.type %in% c('sequential','both')){
          dumm = season.sequential(statevars.seasonal,is.hflux = F)
          if(export.tables){write.csv(dumm,file = paste0(output.dir,output.prefix,'_season_',var.names[var],'_sequential.csv'),row.names = F)}
          g.ls = list()
          for(i in 1:length(boxes)){
            g.ls[[i]] = plot.sequential(data = dumm, error.type,stat.type,space.id = boxes[i],
                                        var.name = paste0(var.names[var],'(',units[var],')'),
                                        time.name = 'Season',title = paste0('Box',boxes[i]))
          }
          pdf(file = paste0(output.dir,output.prefix,'_seasonal_',var.names[var],'_sequential.pdf'),onefile = T,width = 14)
          for(i in 1:length(boxes)){gridExtra::grid.arrange(g.ls[[i]])}
          dev.off()
          rm(statevars.seasonal)
        }
      }
    }
  }
  
  
  

  }

roms.climatology(
  roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing Files/',
  output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Climatology/',
  roms.prefix = 'roms_',
  output.prefix = 'roms_allyears_',
  # bgm.file = 'neus_ll_WGS84.bgm',
  plot.transport = T,
  plot.statevars = T,
  plot.ltlvars = T,
  plot.monthly = T,
  plot.seasonal = T,
  plot.type = 'both',
  error.type = 'CI',
  stat.type = 'mean',
  export.tables = T
)  
