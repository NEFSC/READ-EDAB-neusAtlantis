atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Runs/Atlantis_Output_checkDetritus/'
con = file(paste0(atl.dir,'log2.txt'))
log.lines = readLines(con)
close(con)

fgs = read.csv(here::here('currentVersion','neus_groups.csv'),stringsAsFactors = F)
groups = c(fgs$Code,'Remin')

empty.lines = grep('^$',log.lines)
output.ls = time.ls = list()
i=1

flux.full.ls = list()

for(i in 1:length(empty.lines)){
  time.line = log.lines[empty.lines[i] + 1]
  data.lines = log.lines[(empty.lines[i]+2):(empty.lines[i+1]-1)]
  data.lines.rm = grep('Time:',data.lines)
  if(length(data.lines.rm)!=0){
    data.lines = data.lines[-data.lines.rm]  
  }
  time.flux = grep('Time',data.lines)
  if(length(time.flux)>0){data.lines = data.lines[-time.flux]}
  
  #Format Timestep info
  time.split = strsplit(time.line,' |,|:')[[1]]
  location = strsplit(time.split[5],'-')[[1]]
  box = strsplit(location[1],'box')[[1]][2]
  layer = strsplit(location[2],'layer')[[1]][2]
  time = as.numeric(time.split[3])
  
  flux.split = strsplit(time.split[7],'Flux')[[1]]
  if(flux.split[1] == 'wcwc'){
    flux.from = 'wc';flux.to = 'wc'
  }else if(flux.split[1] == 'wcsed'){
    flux.from = 'wc';flux.to = 'sed'
  }
  flux.type = flux.split[2]
  flux.val = as.numeric(time.split[9])
  
  #Format Data
  flux.ls =lapply(data.lines,function(x){
    split  = strsplit(x,': ')[[1]]
    to.from = strsplit(split[1],'_')[[1]]
    source.loc = strsplit(to.from[1],paste(groups,collapse='|'))[[1]]
    source.group = strsplit(to.from[1],'wc|sed')[[1]][2]
    
    dest.loc = strsplit(to.from[2],paste(groups,collapse='|'))[[1]]
    dest.group = strsplit(to.from[2],'wc|sed|Flux')[[1]][3]
    
    flux.val = as.numeric(split[2])
    
    return(data.frame(source.loc,source.group,dest.loc,dest.group,flux.val))
  })
  
  flux.df = dplyr::bind_rows(flux.ls)
  flux.df$time = time
  flux.df$box = box
  flux.df$layer = layer
  flux.df$flux.from.tot = flux.from
  flux.df$flux.to.tot = flux.to
  flux.df$flux.type.tot = flux.type
  flux.df$flux.val.tot = flux.val
  
  flux.full.ls[[i]] = flux.df
  
  if(i %% 1000 == 0){print(i)}
}

flux.full.df = dplyr::bind_rows(flux.full.ls)

write.csv(flux.full.df,paste0(atl.dir,'Atlantis_Output_Detritus.csv'),row.names =F)
