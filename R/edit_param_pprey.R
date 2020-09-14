#'Edit pPREY values from biology.prm file
#'
#'User specifies predator and prey groups as well as the new value. The new value is substituted in the biology.prm file
#'and is either written as a new file or overwriting the existing file
#'
#'@atl.dir string. Path to the directory containing atlantis parameter files
#'@biol.file string. Name of the biology prm file
#'@nskip numeric. Line to start on after first pPREY comment in biology.prm
#'@fgs.file string. Name of the functional groups file
#'@overwrite  logical. TRUE for overwriting existing file. FALSE for creating a new biology.prm file
#'@new.file.name string. Name of new file if not overwriting existing
#'@spp.names character vector. Names of groups to extract values
#'@is.pred logical. If TRUE names are predator groups (extract prey)
#'@pred.list character vector. Names of predator groups to change
#'@prey.list character vector. Names of corresponding prey groups to change
#'@pprey.vals numeric. Corresponding pprey values to substitute

#Set directories
# atl.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis2/currentVersion/'
# biol.file = paste0(atl.dir,'at_biology.prm')
# fgs.file = here::here('currentVersion','neus_groups.csv')
# spp.names=pred.list = c('ZS','ZM','ZL')
# prey.list = c('PS','PL','ZL')
# pprey.vals = c(0.03,0.05,0.1)
# is.pred = T
# overwrite = T
# new.file.name = 'at_biology_test.prm'


get_pprey_lines = function(atl.dir,biol.file){
  #Open/close connection and read lines in .prm
  con = file(biol.file)
  biol.lines = readLines(con)
  close(con)
  
  #Identify start section for pPrey
  pprey.lines = grep('pPREY',biol.lines)
  
  #Remove comments
  is.comment = sapply(biol.lines[pprey.lines],function(x) return(strsplit(x,'')[[1]][1] == '#'),USE.NAMES = F)
  #remove header lines
  pprey.names = biol.lines[pprey.lines[2]+1]
  pprey.names = strsplit(pprey.names,'##| ')[[1]]
  pprey.names = pprey.names[which(nchar(pprey.names)>0)]
  
  pprey.lines = pprey.lines[!is.comment]
  
  return(list(prey.names = pprey.names,
              line.num = pprey.lines[1]:(pprey.lines[length(pprey.lines)]+1),
              line.chars = biol.lines[pprey.lines[1]:(pprey.lines[length(pprey.lines)]+1)])
  )
}

get_pprey_vals = function(atl.dir,biol.file,fgs.file,spp.names,is.pred,remove.zero){
  
  #Extract pprey lines  
  pprey.ls = get_pprey_lines(atl.dir,biol.file)
  pprey.lines = pprey.ls$line.chars
  prey.names = pprey.ls$prey.names
  
  #Extract Functional Groups
  fgs = read.csv(fgs.file,stringsAsFactors = F)
  groups = fgs$Code
  
  #Format pprey to matrix 
  name.id = seq(1,(length(pprey.lines)-1),2)
  val.id = seq(2,length(pprey.lines),2)
  
  pred.names = sapply(pprey.lines[name.id],function(x) return(strsplit(x,'pPREY|pPREY1|pPREY2| ')[[1]][2]),USE.NAMES = F)
  pprey.mat = matrix(0,ncol = length(prey.names),nrow = length(pred.names))
  for(i in 1:length(pred.names)){
    pprey.mat[i,]=as.numeric(strsplit(pprey.lines[val.id[i]]  ,' ')[[1]])
  }
  
  #If specifying predators, extract prey vals
  if(is.pred){
    pred.match =unname(sapply(spp.names,function(x){return(which(pred.names == x))}))
    # pred.match = unique(grep(paste(spp.names,collapse='|'),pred.names))
    if(length(spp.names) == 1){
      out.mat = as.data.frame(t(pprey.mat[pred.match,]))  
    }else{
      out.mat = as.data.frame(pprey.mat[pred.match,])
    }
    colnames(out.mat)= prey.names
    out.mat$pred = pred.names[pred.match]
    out.mat = out.mat[,c(ncol(out.mat),1:(ncol(out.mat)-1))]
    # out.mat = pprey.mat[pred.match,]
    # row.names(out.mat) = pred.names[pred.match]
    # colnames(out.mat) = prey.names
    if(remove.zero){
      if(length(spp.names) == 1){
        out.mat = out.mat[,which(out.mat[1,]!=0)]
      }else{
        out.mat = out.mat[,!(apply(out.mat,2,function(x) return(all(x==0))))]  
      }
    }
    return(out.mat)
  }else{
    prey.match =unname(sapply(spp.names,function(x){return(which(prey.names == x))}))
    # prey.match = unique(grep(paste(spp.names,collapse='|'),prey.names))
    out.mat = data.frame(pred = pred.names,pprey.mat[,prey.match])
    colnames(out.mat)[2:ncol(out.mat)] = prey.names[prey.match]
    # row.names(out.mat) = pred.names
    if(remove.zero){
      if(length(spp.names) == 1){
        out.mat = out.mat[which(out.mat[,2]!=0),]
      }else{
        out.mat = out.mat[!(apply(out.mat[,2:ncol(out.mat)],1,function(x) return(all(x==0)))),]  
      }
    }
    return(out.mat)
  }
}

#Need to replace and reconstruct.
#Find way to identify pred and prey row in biol.prm
#Overwrite then save
edit_param_pprey = function(atl.dir, biol.file, fgs.file){
  
  #Full File Lines
  con = file(biol.file)
  biol.lines = readLines(con)
  close(con)
  
  #Read in pPREY vals from biology.prm
  pprey.ls = get_pprey_lines(atl.dir,biol.file)
  
  #Read in functional groups
  fgs = read.csv(fgs.file,stringsAsFactors = F)
  groups = fgs$Code
  
  #Loop through pred-prey matches
  if(length(pred.list) != length(prey.list)){
    stop('Need equal predator and prey pairs')
  }
  
  for(i in 1:length(pred.list)){
    
    #ID pred, prey, and new value
    pred = pred.list[i]
    prey = prey.list[i]
    val = pprey.vals[i]
    
    #Identify pPREY line for pred group and convert to numeric
    pred.line = grep(pred,pprey.ls$line.chars)
    pred.vals = pprey.ls$line.chars[pred.line+1]
    pred.vals = as.numeric(strsplit(pred.vals,' ')[[1]])
    
    #Identify pPREY element for prey group
    prey.el = grep(paste0("\\",prey,"\\b"),pprey.ls$prey.names)
    #Overwrite existing pprey value
    pred.vals[prey.el] = val
    #Convert back to space delimited character string
    pred.vals.new = paste(pred.vals,collapse =' ')
    
    #Overwrite biol.lines with new values
    biol.lines[pprey.ls$line.num[pred.line]+1] = pred.vals.new
    
  }
  #If overwriting overwrite existing file and save
  if(overwrite){
    writeLines(biol.lines, con = paste0(biol.file))
  }else{
    file.copy(biol.file,paste0(atl.dir,new.file.name))
    writeLines(biol.lines,con = paste0(atl.dir,new.file.name))
  }
  
}


