# data = bio.diff.set
# plot.cols = RColorBrewer::brewer.pal(4,'Set2')
# plot.max = 1
# plot.min = 0
# ngrid = 4
# margin.scale = 1.5

make_radar_plot = function(data,plot.max,plot.min,ngrid,margin.scale = 1.5,plot.cols){
  library(ggplot2)
  
  set.min = min(data$Value.diff)
  set.max = max(data$Value.diff)
  
  data = data %>%
    mutate(Value.diff = (Value.diff-set.min)/(set.max-set.min))
  

  
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
  
  grid.ls = list()
  for(i in 1:ngrid){
    x.seq = grid.seq[i]*cos(group.seq)
    y.seq = grid.seq[i]*sin(group.seq)
    grid.ls[[i]] = data.frame(x = x.seq, y = y.seq,group = i)
  }
  grid.df = bind_rows(grid.ls)
  
  radii.df = data.frame(
    x.0 = rep(0,ngroup),
    y.0 = rep(0,ngroup),
    x.1 = cos(group.seq),
    y.1 = sin(group.seq)
  )
  
  plot.data.ls = list()
  for(i in 1:nset){
    plot.data.ls[[i]] = data %>%
      filter(Group == run.names[i])%>%
      mutate(
        grid.pos = group.seq,
        x = Value.diff*cos(grid.pos),
        y = Value.diff*sin(grid.pos)
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
   
  p =ggplot()+
    coord_fixed()+
    geom_polygon(data = grid.df, aes(x=x,y=y,group = group),fill = 'transparent',color = 'grey',lty=2)+
    geom_segment(data = radii.df,aes(x = x.0, y = y.0, xend = x.1, yend = y.1),lty = 2, color = 'black')+
    geom_polygon(data = plot.data.df,aes(x=x,y=y,color = Group),fill = 'transparent')+
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