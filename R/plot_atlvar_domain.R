#'Creates timeseries plots of any domain-wide variable in main output.nc file
#'
#'@description Specifies a given variable and creates a box-level timeseries of a 
#'specified variable found in the main output.nc file. This is mainly used for 
#'diagnostics/calibration, and allows for a more refined look into specific variables
#'than other aggregative plots.
#'
#'@nc.file string. path to output.nc file
#'@variable.name string. name of variable found in output.nc
#'@plot.dir string. path where output to be saved
#'@plot.name string. name of output plot
#'
#'@return pdf with each page showing timeseries of specified variable for each level within a box. 
#'
#'Author: J. Caracappa
#'

# nc.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/neus_output_test.nc'
# variable.name = 'NH3'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/Figures/geophysical_output/'
# plot.name = 'NH3_timeseries'


plot_atlvar_domain = function(nc.file, variable.name, plot.dir, plot.name,save.data = T){
  
  source(here::here('R','atl_var_to_longform.R'))

  `%>%` = dplyr::`%>%`

  output.nc = ncdf4::nc_open(nc.file)
  var.data = ncdf4::ncvar_get(output.nc,variable.name)
  var.plotname = ncdf4::ncatt_get(output.nc,variable.name)$long_name
  var.units = ncdf4::ncatt_get(output.nc,variable.name)$units
  var.time = ncdf4::ncvar_get(output.nc,'t')
  t.start = strsplit(ncdf4::ncatt_get(output.nc,'t')$units,' ')[[1]][3]
  var.time = as.POSIXct(var.time, origin = paste0(t.start,' 00:00:00'),tz = 'UTC')
  ncdf4::nc_close(output.nc)
  
  boxes = 0:(dim(var.data)[2] -1)
  levels = 1:dim(var.data)[1]
  

  # var.domain = as.data.frame(t(apply(var.data,c(1,3),mean,na.rm=T)))
  var.domain = atl_var_to_longform(nc.file,variable.name,'1964-01-01')
  var.domain$region = sapply(var.domain$box, function(x){
    if(x %in% c(0,23:29)){
      return('boundary')
    }else if(x %in% c(1:9)){
      return('MAB')
    }else if(x %in% c(10:11,16:22)){
      return('GOM')
    }else if(x %in% c(12:15)){
      return('GB')
    }else{
      return(NA)
    }
  })
  # colnames(var.domain) = paste0('L',levels)
  # var.domain$time = var.time
  # var.domain.lev = reshape2::melt(var.domain, id.vars = 'time')
  
  var.domain.lev = var.domain %>% dplyr::group_by(time,time.date,level,region) %>%
    dplyr::summarize( value.mu = mean(value,na.rm=T))
  var.domain.lev = var.domain.lev[which(var.domain.lev$value.mu != 0),]

  
  var.domain.lev$time.group = NA
  var.domain.lev$time.group[which(var.domain.lev$time.date < as.POSIXct('1979-01-01',tz = 'UTC'))] = 'early'
  var.domain.lev$time.group[which(var.domain.lev$time.date > as.POSIXct('1994-01-01',tz = 'UTC'))] = 'late'
  
  box.level.means = na.omit(var.domain.lev) %>% dplyr::group_by(region,level,time.group) %>%
    dplyr::summarize(value.mu = mean(value.mu,na.rm=T),
                     value.min = min(value.mu,na.rm=T),
                     value.max = max(value.mu,na.rm=T))
  box.level.means$x = as.POSIXct('1964-01-01',tz = 'UTC')
  box.level.means$x[box.level.means$time.group=='late'] = as.POSIXct('1994-01-01',tz = 'UTC',origin = '1964-01-01')
  box.level.means$xend = as.POSIXct('1979-01-01',tz = 'UTC')
  box.level.means$xend[box.level.means$time.group=='late'] = as.POSIXct('2014-12-12',tz = 'UTC',origin = '1964-01-01')
  
  origin = '1964-01-01'
  

  ggplot2::ggplot(data = var.domain.lev,ggplot2::aes(x=time.date, y=value.mu,color = region))+
    ggplot2::geom_line()+
    ggplot2::geom_segment(data = box.level.means,
                          ggplot2::aes(x = x, xend = xend, y = value.mu,
                                       yend = value.mu, lty = time.group,
                                       color = region),size = 1.2)+
    ggplot2::facet_wrap(~level,nrow = 5)+

    ggplot2::xlab('Date')+
    ggplot2::ylab(paste0(var.plotname,' (',var.units,')'))+
    ggplot2::theme_classic()+
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank()
    )+
    ggplot2::ggsave(paste0(plot.dir,plot.name,'_domain.png'),width = 14, height = 8, units = 'in', dpi = 350)
  
  if(save.data){
    write.csv(box.level.means,row.names = F,file = paste0(plot.dir,variable.name,'_means.csv'))
  }
}
