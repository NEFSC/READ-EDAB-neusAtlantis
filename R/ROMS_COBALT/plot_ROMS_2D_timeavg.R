#' Generates heatmaps to visualize roms-aggregated data
#' 
#' Used to summarize/diagnose aggregated ROMS output that is fed
#' into hydroconstruct. Assumes input files are netcdfs with horizontal 
#' fluxes as arrays of dimension [level,face,time] and state variables 
#' as arrays of [level, box, time]. Horizontal fluxes plotted as
#' map with vectors on faces with color representing the magnitude. 
#' State variables are heatmaps over domain structure.Assumes yearly files.
#' 
#' @roms.dir string. Path for aggregated roms data
#' @plot.transport logical. Plot horizontal flux?
#' @plot.statevars logical. Plot state variables (temperature, salinity, vertical flux)?
#' @plot.ltlvars logical. Plot COBALT variables (phytoplankton, zooplankton)?
#' @plot.yearly logical. Plot yearly averages?
#' @plot.seasonal logical. Plot seasonal averages?
#' @plot.monthly logical. Plot monthly averages?
#' @plot.prefix string. Prefix for output plots
#' @plot.dir string. path for saving plots
#' @bgm.file  string. name of bgm file with lat/lon coordinates (not Atlantis coords)
#' 
#' @return yearly, seasonal, and/or monthly plots of specified variables
#' 
#'#' Author: J. Caracappa

# roms.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/1980/'
# plot.transport = T
# plot.statevars = T
# plot.ltlvars = T
# plot.yearly = T
# plot.seasonal = T
# plot.monthly = T
# plot.prefix = '1980'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/'
# bgm.file = 'Neus_ll_WGS84.bgm'

plot_ROMS_2D_timeavg = function(roms.dir,plot.transport=T,plot.statevars=T,plot.ltlvars=T,plot.yearly=T,plot.seasonal=T,
                     plot.monthly=T,plot.prefix,plot.dir,bgm.file){
  
  `%>%` = dplyr::`%>%`
  
  nc.files = list.files(roms.dir,pattern = '*.nc',full.names = T)
  transport.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='transport')))]
  statevars.file = nc.files[which(sapply(nc.files,function(x) any(strsplit(x,'[_.]+')[[1]]=='statevars') & all(strsplit(x,'[_.]+')[[1]] != 'ltl')))]
  ltlvars.file = nc.files[which(sapply(nc.files, function(x) any(strsplit(x,'[_.]+')[[1]] == 'ltl')))]
  
  bgm = rbgm::bgmfile(here::here('Geometry',bgm.file))
  bgm.all = dplyr::left_join(bgm$faces,bgm$facesXverts) %>% dplyr::left_join(.,bgm$vertices) %>% dplyr::select(.fx0,.p0,x,y)
  bgm.all = bgm.all %>% tidyr::gather(variable,value,-.fx0,-.p0) %>%
    tidyr::unite(.p0_variable,.p0,variable) %>%
    tidyr::spread(.p0_variable,value)
  bgm.all = dplyr::rename(bgm.all, pt1_x = '1_x',pt1_y = '1_y',pt2_x = '2_x',pt2_y = '2_y',face = .fx0)
  bgm.all$face = as.factor(bgm.all$face)
  # bgm.boundary = dplyr::left_join(bgm$boxes,bgm$boxesXverts, by = '.bx0') %>% 
  #   dplyr::left_join(bgm$vertices,by = '.vx0') %>%
  #   dplyr::filter(.bx0 %in% c(0,25:29))%>%
  #   dplyr::arrange(.bx0,.vx0)
  
  
  box.map = ggplot2::fortify(rbgm::boxSpatial(bgm),region ='.bx0') %>% dplyr::rename(box = id) 
  
  # ggplot2::ggplot(b,ggplot2::aes(x=long,y=lat,group = id)) + ggplot2::geom_polygon(fill='white',color='black')
  
  source(here::here('R','flatten_ROMS.R'))
  
  face.vector = function(dat,D){
    out = dat
    # out$midx = (bgm.dat$pt1_x+bgm.dat$pt2_x)/2
    # out$midy = (bgm.dat$pt1_y+bgmdat$pt2_y)/2
    out$midx = (out$pt1_x+out$pt2_x)/2
    out$midy = (out$pt1_y+out$pt2_y)/2
    out$slope = (out$pt2_y-out$pt1_y)/(out$pt2_x-out$pt1_x)
    out$p.slope = -1/out$slope
    out$dx = abs((D/sqrt( (out$pt1_y-out$pt2_y)^2 + (out$pt2_x-out$pt1_x)^2))*(out$pt1_y-out$pt2_y))
    out$dy = abs((D/sqrt( (out$pt1_y-out$pt2_y)^2 + (out$pt2_x-out$pt1_x)^2))*(out$pt2_x-out$pt1_x))
    out$new.x = apply(cbind(out$hflux,out$slope,out$midx,out$dx),1,function(x){
      if(any(is.na(x))){
        return(NA)
      } else if(x[2]>0){
        if(x[1]>=0){
          return(x[3]-x[4])
        }else{
          return(x[3]+x[4])
        }
      }else if(x[2]<0){
        if(x[1]>=0){
          return(x[3]+x[4])
        }else{
          return(x[3]-x[4])
        }
      }else {
        return(x[3])
      }
    })
    out$new.y = apply(cbind(out$hflux,out$slope,out$midy,out$dy),1,function(x){
      if(any(is.na(x))){
        return(NA)
      } else {
        if(x[1]>0){
          return(x[3]+x[4])
        }else{
          return(x[3]-x[4])
        }
      }
    })
    return(out)
  }

  plot.fluxes.level = function(data,lev,title,var.breaks){
    xx = subset(data,level == lev,c(face,hflux,midx,midy,new.x,new.y))
    # xx = subset(data,level == lev,c(face,hflux,pt1_x,pt1_y,pt2_x,pt2_y,midx,midy,new.x,new.y))
    ggplot2::ggplot(box.map,ggplot2::aes(x=long,y=lat,group = box)) +
    ggplot2::geom_polygon(fill='grey95',color='black')+
    ggplot2::coord_fixed()+
      # ggplot2::coord_map()+
    # ggplot2::ggplot()+
    ggplot2::geom_segment(data = bgm.all,ggplot2::aes(x=pt1_x,xend = pt2_x,y = pt1_y,yend = pt2_y,group = face))+
    
    # ggplot2::geom_path(data = bgm$boundaryvertices, ggplot2::aes(x=x,y=y))+
    # ggplot2::geom_path(data = bgm.boundary, ggplot2::aes(x=x,y=y,group=.bx0))+
    ggplot2::geom_segment(data = xx,
                          ggplot2::aes(x=midx,y=midy,xend = new.x,yend = new.y,col = hflux,group=face),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2,'cm')),
                          size = 0.75)+
      
    ggplot2::scale_color_gradientn(name = expression(atop('Horiz. Flux','Log('*m^3~s^-1*')')),
                                  limits = c(min(var.breaks),max(var.breaks)),
                                  colors = c('blue2','red2'),breaks = var.breaks)+
    ggplot2::ggtitle(title)+
    ggplot2::theme_void()+
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  }
  
  plot.boxvars = function(data,lev,var,title,var.breaks){
    ggplot2::ggplot(data = dplyr::filter(data,level == lev),ggplot2::aes(x=long,y=lat,group = box,fill = statevar ))+
      ggplot2::geom_polygon(color = 'black',size=1)+
      ggplot2::scale_fill_gradientn(name = bquote(atop(.(var.names[var]),.(units[var]))),limits = c(min(var.breaks),max(var.breaks)),
                                   colors = c('blue2','red2'),breaks = var.breaks)+
      ggplot2::ggtitle(title)+
      ggplot2::theme_void()+
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.margin = ggplot2::unit(c(0.5,0.5,0.5,0.5),'in')
      )
  }
  
  if(plot.transport){
    dat = roms2long(transport.file,is.hflux = T) %>% 
      dplyr::select(level,time,face,hflux,dest_b,source_b) %>%
      dplyr::left_join(bgm.all, by = 'face')
    # dat$hflux = log(abs(dat$hflux),10)*sign(dat$hflux)
    var.breaks = signif(seq(floor(quantile(log(dat$hflux,10),0.01,na.rm=T)),ceiling(quantile(log(dat$hflux,10),0.99,na.rm=T)),by=1),2)

    if(plot.yearly){
      dat$year = format(dat$time,format = '%Y')
      dat.year = dat %>% 
        dplyr::group_by(year,face,level) %>% 
        dplyr::filter(!dest_b %in% c(23,24) & !source_b %in% c(23,24)) %>%
        dplyr::summarise_at(dplyr::vars(hflux:pt2_y),mean,na.rm=T)
      dat.year = face.vector(dat.year,D=0.5)
      dat.year$hflux = log(abs(dat.year$hflux),10)
      years = unique(dat.year$year)
      year.plots = list()
      for(yr in 1:length(years)){
      # for(yr in 1:2){
        lev.plots = list()
        for(lev in 1:4){
         lev.plots[[lev]] = plot.fluxes.level(dat.year %>% dplyr::filter(year == years[yr]),lev = lev,
                                              title = paste0('Yearly (',years[yr],'): Level',lev),var.breaks)
        }
        year.plots[[yr]]= do.call(gridExtra::grid.arrange,c(lev.plots,ncol = 2))
        print(yr)
      }
      pdf(paste0(plot.dir,plot.prefix,'_yearly_transport.pdf'),onefile=T,width = 14, height = 14)
      for(i in 1:length(year.plots)){
        gridExtra::grid.arrange(year.plots[[i]])
      }
       dev.off()
      # year.plots.all = do.call(gridExtra::grid.arrange,c(year.plots,ncol = 2))
      # ggplot2::ggsave(paste0(plot.dir,plot.prefix,'_yearly_transport.pdf'),year.plots.all,width = 14,height = 14)
    }
    if(plot.seasonal){
      dat$month = format(dat$time,format = '%m')
      dat$season = sapply(dat$month, function(x){
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
        }
      })
      
      dat.season = dat %>% 
        dplyr::group_by(season,face,level) %>%
        dplyr::filter(!dest_b %in% c(23,24) & !source_b %in% c(23,24)) %>%
        dplyr::summarise_at(dplyr::vars(hflux:pt2_y),mean,na.rm=T)
      dat.season = face.vector(dat.season, D = 0.5)
      dat.season$hflux = log(abs(dat.season$hflux),10)

      season.plots = list()
      for(s in 1:4){
        lev.plots = list()
        for(lev in 1:3){
          lev.plots[[lev]] = plot.fluxes.level(dat.season %>% dplyr::filter(season == s),
                                               lev = lev,
                                               title = paste0('Season',s,': Level ',lev),var.breaks)
        }
        season.plots[[s]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol=2))
      }
      pdf(file = paste0(plot.dir,plot.prefix,'_seasonal_transport.pdf'),onefile = T,width = 14, height = 14)
      for(i in 1:4){
        gridExtra::grid.arrange(season.plots[[i]])
      }
      dev.off()
    }
    
    if(plot.monthly){
      dat$month = format(dat$time,format = '%m')
      dat$month.name = format(dat$time,format = '%B')
      dat.month = dat %>%
        dplyr::group_by(month,month.name,face,level) %>%
        dplyr::filter(!dest_b %in% c(23,24) & !source_b %in% c(23,24)) %>%
        dplyr::summarise_at(dplyr::vars(hflux:pt2_y),mean,na.rm=T)
      dat.month = face.vector(dat.month, D = 0.5)
      dat.month$hflux = log(abs(dat.month$hflux),10)
      months = unique(dat$month)
      month.names = unique(dat$month.name)
      month.plots = list()
      for(m in seq_along(months)){
        lev.plots = list()
        for(lev in 1:3){
          lev.plots[[lev]] = plot.fluxes.level(dat.month %>% dplyr::filter(month == months[m]),
                                               lev = lev,
                                               title = paste0(month.names[m],': Level ',lev),
                                               var.breaks)
        }
        month.plots[[m]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol =2))
      }
      pdf(file = paste0(plot.dir,plot.prefix,'_monthly_transport.pdf'),onefile = T,width = 14, height = 14)
      for(i in 1:length(months)){
        gridExtra::grid.arrange(month.plots[[i]])
      }
      dev.off()
    }
  }
  if(plot.statevars){
    dat.ls = roms2long(statevars.file,is.hflux = F)
    nvars=length(dat.ls)
    nlev = length(unique(dat.ls[[1]]$level))
    var.names = names(dat.ls)
    dumm = ncdf4::nc_open(statevars.file)
    units = sapply(1:nvars,function(x){dumm$var[[x]]$units})
    ncdf4::nc_close(dumm)
    var.breaks = lapply(dat.ls,function(x) {
      vals = x[,4] 
      return(signif(seq(quantile(vals,0.01,na.rm=T),quantile(vals,0.99,na.rm=T),length.out=8),2))
      # return(signif(seq(min(vals,na.rm=T),max(vals,na.rm=T),length.out = 8),2))
    })
    
    if(plot.yearly){
      
      dat.year.ls = lapply(dat.ls,function(x){
        DF = x
        DF$year = format(DF$time,format = '%Y')
        DF2 = DF %>%
          dplyr::group_by(year,box,level) %>%
          dplyr::filter(!box %in% c(23,24)) %>%
          dplyr::summarize_all(mean,na.rm=T)
        DF2$box = as.character(DF2$box)
         return(DF2) 
      })
      years = unique(dat.year.ls[[1]]$year)
      # year.plots = list()
      # for(i in 1:nvars) { year.plots[[i]] = list()}

      for(var in 1:nvars){
        
        
        dat.var = dat.year.ls[[var]] %>% dplyr::left_join(box.map,by='box')
        colnames(dat.var)[5] = 'statevar'
        year.plots = list()
        for(yr in 1:length(years)){
        # for(yr in 1:2){
          lev.plots = list()
          for(lev in 1:nlev){
            lev.plots[[lev]] = plot.boxvars(dat.var %>% dplyr::filter(year == years[yr]),lev=lev,var=var,var.breaks = var.breaks[[var]],
                                            title = paste0('Yearly (',years[yr],'): Level',lev))
          }
          year.plots[[yr]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol = 2,padding = ggplot2::unit(1,'line')))
        }
        pdf(paste0(plot.dir,plot.prefix,'_yearly_',var.names[var],'.pdf'),width = 14, height = 14, onefile = T)
        for(i in 1:length(year.plots)){
          gridExtra::grid.arrange(year.plots[[i]])
        }
        dev.off()
      }
    }
    
    if(plot.seasonal){
      dat.season.ls = lapply(dat.ls,function(x) {
        DF = x
        DF$month = format(DF$time,format = '%m')
        DF$season =  sapply(DF$month, function(x){
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
          }
        })
        DF2 = DF %>%
          dplyr::select(-month,-time) %>%
          dplyr::group_by(season,box,level) %>%
          dplyr::filter(!box %in% c(23,24)) %>%
          dplyr::summarize_all(mean,na.rm=T)
        DF2$box = as.character(DF2$box)
        return(DF2)
      })
      
      for(var in 1:nvars){
        dat.var = dat.season.ls[[var]] %>% dplyr::left_join(box.map,by='box')
        colnames(dat.var)[4] = 'statevar'
        season.plots = list()  
        
        for(s in 1:4){
          lev.plots = list()
        
          for(lev in 1:nlev){
            lev.plots[[lev]] = plot.boxvars(dat.var %>% dplyr::filter(season == s),
                                            lev = lev,var = var,var.breaks = var.breaks[[var]],
                                            title = paste0('Season',s,': Level ',lev))

          }
          season.plots[[s]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol=2))
        }
        pdf(file = paste0(plot.dir,plot.prefix,'_seasonal_',var.names[var],'.pdf'),onefile = T,width = 14, height = 14)
        for(i in 1:4){
          gridExtra::grid.arrange(season.plots[[i]])
        }
        dev.off()
      }
    }
    if(plot.monthly){
      dat.month.ls = lapply(dat.ls,function(x){
        DF = x
        DF$month.name = format(DF$time,format = '%B')
        DF$month = as.numeric(format(DF$time,format = '%m'))
        DF2 = DF %>%
          dplyr::group_by(month,month.name,box,level) %>%
          dplyr::filter(!box %in% c(23,24)) %>%
          dplyr::summarize_all(mean,na.rm=T)
        DF2$box = as.character(DF2$box)
        return(DF2)
      })
      months = unique(dat.month.ls[[1]]$month)
      month.names = unique(dat.month.ls[[1]]$month.name)
  
      for(var in 1:length(var.names)){
        dat.var = dat.month.ls[[var]] %>% dplyr::left_join(box.map,by='box')
        colnames(dat.var)[6] = 'statevar'
        
        month.plots = list()  
        for(m in 1:length(months)){
          lev.plots = list()
          for(lev in 1:nlev){
            lev.plots[[lev]] = plot.boxvars(dat.var %>% dplyr::filter(month == months[m]),
                                            lev = lev,var = var,var.breaks = var.breaks[[var]],
                                            title = paste0(month.names[m],': Level ',lev))
          }
          month.plots[[m]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol=2))
        }
        pdf(file = paste0(plot.dir,plot.prefix,'_monthly_',var.names[var],'.pdf'),onefile = T,width = 14, height = 14)
        for(i in 1:length(months)){
          gridExtra::grid.arrange(month.plots[[i]])
        }
        dev.off()
      }
    }
  }
  if(plot.ltlvars){
    dat.ls = roms2long(ltlvars.file,is.hflux = F)
    nvars=length(dat.ls)
    nlev = length(unique(dat.ls[[1]]$level))
    var.names = names(dat.ls)
    dumm = ncdf4::nc_open(ltlvars.file)
    units = sapply(1:nvars,function(x){dumm$var[[x]]$units})
    ncdf4::nc_close(dumm)
    var.breaks = lapply(dat.ls,function(x) {
      vals = x[,4] 
      # return(signif(seq(min(vals,na.rm=T),max(vals,na.rm=T),length.out = 8),2))
      return(signif(seq(quantile(vals,0.01,na.rm=T),quantile(vals,0.99,na.rm=T),length.out=8),2))
    })
    
    if(plot.yearly){
      
      dat.year.ls = lapply(dat.ls,function(x){
        DF = x
        DF$year = format(DF$time,format = '%Y')
        DF2 = DF %>%
          dplyr::group_by(year,box,level) %>%
          dplyr::filter(!box %in% c(23,24)) %>%
          dplyr::summarize_all(mean,na.rm=T)
        DF2$box = as.character(DF2$box)
        return(DF2) 
      })
      years = unique(dat.year.ls[[1]]$year)
      # year.plots = list()
      # for(i in 1:nvars) { year.plots[[i]] = list()}
      
      for(var in 1:nvars){
        
        dat.var = dat.year.ls[[var]] %>% dplyr::left_join(box.map,by='box')
        colnames(dat.var)[5] = 'statevar'
        year.plots = list()
        for(yr in 1:length(years)){
        # for(yr in 1:2){
          lev.plots = list()
          for(lev in 1:nlev){
            lev.plots[[lev]] = plot.boxvars(dat.var %>% dplyr::filter(year == years[yr]),lev=lev,var=var,var.breaks = var.breaks[[var]],
                                            title = paste0('Yearly (',years[yr],'): Level',lev))
          }
          year.plots[[yr]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol = 2,padding = ggplot2::unit(1,'line')))
        }
          pdf(paste0(plot.dir,plot.prefix,'_yearly_',var.names[var],'.pdf'),width = 14, height = 14, onefile = T)
          for(i in 1:length(year.plots)){
            gridExtra::grid.arrange(year.plots[[i]])
          }
          dev.off()
      }
    }


    if(plot.seasonal){
      dat.season.ls = lapply(dat.ls,function(x) {
        DF = x
        DF$month = format(DF$time,format = '%m')
        DF$season =  sapply(DF$month, function(x){
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
          }
        })
        DF2 = DF %>%
          dplyr::select(-month,-time) %>%
          dplyr::group_by(season,box,level) %>%
          dplyr::filter(!box %in% c(23,24)) %>%
          dplyr::summarize_all(mean,na.rm=T)
        DF2$box = as.character(DF2$box)
        return(DF2)
      })
      
      for(var in 1:nvars){
        dat.var = dat.season.ls[[var]] %>% dplyr::left_join(box.map,by='box')
        colnames(dat.var)[4] = 'statevar'
        season.plots = list()  
        
        for(s in 1:4){
          lev.plots = list()
          
          for(lev in 1:nlev){
            lev.plots[[lev]] = plot.boxvars(dat.var %>% dplyr::filter(season == s),
                                            lev = lev,var = var,var.breaks = var.breaks[[var]],
                                            title = paste0('Season',s,': Level ',lev))
            
          }
          season.plots[[s]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol=2))
        }
        pdf(file = paste0(plot.dir,plot.prefix,'_seasonal_',var.names[var],'.pdf'),onefile = T,width = 14, height = 14)
        for(i in 1:4){
          gridExtra::grid.arrange(season.plots[[i]])
        }
        dev.off()
      }
    }
    if(plot.monthly){
      dat.month.ls = lapply(dat.ls,function(x){
        DF = x
        DF$month.name = format(DF$time,format = '%B')
        DF$month = as.numeric(format(DF$time,format = '%m'))
        DF2 = DF %>%
          dplyr::group_by(month,month.name,box,level) %>%
          dplyr::filter(!box %in% c(23,24)) %>%
          dplyr::summarize_all(mean,na.rm=T)
        DF2$box = as.character(DF2$box)
        return(DF2)
      })
      months = unique(dat.month.ls[[1]]$month)
      month.names = unique(dat.month.ls[[1]]$month.name)
      
      for(var in 1:length(var.names)){
        dat.var = dat.month.ls[[var]] %>% dplyr::left_join(box.map,by='box')
        colnames(dat.var)[6] = 'statevar'
        
        month.plots = list()  
        for(m in 1:length(months)){
          lev.plots = list()
          for(lev in 1:nlev){
            lev.plots[[lev]] = plot.boxvars(dat.var %>% dplyr::filter(month == months[m]),
                                            lev = lev,var = var,var.breaks = var.breaks[[var]],
                                            title = paste0(month.names[m],': Level ',lev))
          }
          month.plots[[m]] = do.call(gridExtra::grid.arrange,c(lev.plots,ncol=2))
        }
        pdf(file = paste0(plot.dir,plot.prefix,'_monthly_',var.names[var],'.pdf'),onefile = T,width = 14, height = 14)
        for(i in 1:length(months)){
          gridExtra::grid.arrange(month.plots[[i]])
        }
        dev.off()
      }
    }
  }
}
  
# file.dirs = paste0('C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/ROMS_OUT/',1980:2014,'/')
# prefixes = 1980:2014

# 
# for(f.dir in 20:length(file.dirs)){
#   
#   roms_maps(roms.dir = file.dirs[f.dir],
#             plot.transport = T,
#             plot.statevars = T,
#             plot.ltlvars = T,
#             plot.yearly = T,
#             plot.seasonal = T,
#             plot.monthly = T,
#             plot.prefix = prefixes[f.dir],
#             plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/',
#             bgm.file = 'Neus_ll_WGS84.bgm')
#   graphics.off()
#   print(f.dir)
#   
# }  
