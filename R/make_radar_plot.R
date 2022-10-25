# data = readRDS(here::here('diagnostics','run_set_test_data_diff.RDS'))%>%
#   filter(set.name == 'Run_Set_1')%>%
#   ungroup()%>%
#   select(run.name,FuncGroup,Value.diff)%>%
#   rename(Group = 'run.name')
# plot.cols = RColorBrewer::brewer.pal(4,'Set2')
# plot.max = 2
# plot.min = 0
# ngrid = 3
# margin.scale = 1.5

make_radar_plot = function(data,plot.max,plot.min,ngrid,margin.scale = 1.5,plot.cols){
  library(ggplot2)
  
  set.min = min(data$Value.diff)
  set.max = max(data$Value.diff)
  
  data$Value.diff.norm = NA
  for(i in 1:nrow(data)){
    pos = data$Value.diff[data$Value.diff>=0]
    neg = data$Value.diff[data$Value.diff<=0]
    
    if(data$Value.diff[i]<0){
      data$Value.diff.norm[i] = ((data$Value.diff[i]-min(neg))/(max(neg)-min(neg))) -1
    }else if(data$Value.diff[i]>0){
      data$Value.diff.norm[i] = ((data$Value.diff[i]-min(pos))/(max(pos)-min(pos)))
    }else{
      data$Value.diff.norm[i] = 0
    }
  }
  data$Value.diff.norm = data$Value.diff.norm +1

  run.names = unique(data$Group)
  group.names = sort(unique(data$FuncGroup))
  group.names.plot = sapply(group.names,function(x){
    dum = strsplit(x,' |-')[[1]]
    return(paste(dum,collapse = '\n'))
  },USE.NAMES = F)
  
  ngroup = length(group.names)
  nset = length(run.names)
  
  grid.seq = seq(plot.min, plot.max,length.out = ngrid)
  group.seq = seq(0,2*pi,length.out = ngroup+1)[-(ngroup+1)]
  
  # grid.ls = list()
  # for(i in 1:ngrid){
  #   x.seq = grid.seq[i]*cos(group.seq)
  #   y.seq = grid.seq[i]*sin(group.seq)
  #   grid.ls[[i]] = data.frame(x = x.seq, y = y.seq,group = i)
  # }
  # grid.df = bind_rows(grid.ls)
  mid.grid = data.frame(x = cos(group.seq),y = sin(group.seq))
  upper.grid = data.frame(x = plot.max*cos(group.seq),y = plot.max*sin(group.seq))
  
  radii.df = data.frame(
    x.0 = rep(0,ngroup),
    y.0 = rep(0,ngroup),
    x.1 = plot.max*cos(group.seq),
    y.1 = plot.max*sin(group.seq)
  )
  
  plot.data.ls = list()
  for(i in 1:nset){
    plot.data.ls[[i]] = data %>%
      filter(Group == run.names[i])%>%
      mutate(
        grid.pos = group.seq,
        x = Value.diff.norm*cos(grid.pos),
        y = Value.diff.norm*sin(grid.pos)
        )
  }
  plot.data.df = bind_rows(plot.data.ls)
  
  label.df = data.frame(
    label = group.names.plot,
    x = plot.max*1.1*cos(group.seq),
    y = plot.max*1.1*sin(group.seq),
    group.angle = group.seq,stringsAsFactors = F)%>%
    mutate(label.slope = -x/(sqrt((plot.max*1.1)^2 - x^2)),
           label.slope.norm = -1/label.slope,
           label.angle = ifelse(group.angle >pi, -atan(label.slope.norm)*180/pi,atan(label.slope.norm)*180/pi),
           label.angle2 = (label.angle+360)%%360
    )
    label.df$label.hjust = sapply(label.df$group.angle,function(x){
      label.hjust = if(x<=pi/2){
        hjust = 0
      }else if(x> (pi/2) & x <= pi){
        hjust = 1
      }else if(x> pi & x<= (3*pi/2)){
        hjust = 1
      }else if(x > 3*pi/2){
        hjust = 0
      }
    }
    )
   
  p=ggplot()+
    coord_fixed()+
    geom_polygon(data = mid.grid, aes(x=x,y=y),fill = 'transparent',color = 'black',lty = 1, lwd = 1)+
    geom_polygon(data = upper.grid, aes(x=x,y=y),fill = 'transparent',color = 'grey',lty = 1, lwd = 1)+
    geom_segment(data = radii.df,aes(x = x.0, y = y.0, xend = x.1, yend = y.1),lty = 2, color = 'black')+
    geom_polygon(data = plot.data.df,aes(x=x,y=y,color = Group),fill = 'transparent',lwd = 1.25)+
    geom_text(data = label.df,aes(x=x,y=y,label = label,angle = label.angle2,hjust = label.hjust),size = 3,parse =F)+
    ylim(-margin.scale*plot.max,margin.scale*plot.max)+
    xlim(-margin.scale*plot.max,margin.scale*plot.max)+
    scale_color_manual(name = '',values = plot.cols)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  return(p)
}
