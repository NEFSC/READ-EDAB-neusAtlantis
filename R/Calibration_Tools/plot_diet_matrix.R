# Script to plot diet matrix w.i.p...
library(dplyr)

source(here::here('R','edit_param_pprey.R'))
source(here::here('R','compare_pprey.R'))

priority = read.csv(here::here('diagnostics','neus_atlantis_group_priority.csv'))%>%
  select(code, priority.overall)

spp.ls = pprey.combs('HER')
prey.df = dplyr::bind_rows(lapply(spp.ls,get_pprey_vals,
                                  atl.dir = here::here('currentVersion',),
                                  biol.file = here::here('currentversion','at_biology.prm'),
                                  fgs.file = here::here('currentVersion','neus_groups.csv'),
                                  is.pred = T,
                                  remove.zero = T))

pred.df =get_pprey_vals(
  atl.dir = here::here('currentVersion',),
  biol.file = here::here('currentversion','at_biology.prm'),
  fgs.file = here::here('currentVersion','neus_groups.csv'),
  spp.names = NULL,
  is.pred = F,
  remove.zero = F
)

pred.mat = pred.df 
for(i in 2:ncol(pred.mat)){
  x = as.numeric(as.character(pred.mat[,i]))
  x[which(x>0)] = 1
  pred.mat[,i] = x
  }

pred.names = as.character(pred.mat$pred.names)
for(i in 1:length(pred.names)){
  
  x = pred.names[i]
  if(substr(x,1,1) %in% c('1','2')){
    if( nchar(x) == 5){
      new.x = substr(x,2,4)
    }else{
      new.x = substr(x,2,3)
    }
  }else{
    new.x = x
  }
  pred.names[i] = new.x
}

pred.mat$code = pred.names
pred.mat = left_join(pred.mat,priority)
pred.mat = pred.mat[,c(94:95,1:93)]

write.csv(pred.mat, here::here('diagnostics','binary_diet_matrix.csv'),row.names = F)
write.csv(pred.df,  here::here('diagnostics','full_diet_matrix.csv'),row.names = F)
# pred.df = pred.df %>%
#   mutate(pred = as.character(pred),
#     pred.age = substr(pred,1,1),
#     prey.age = substr(pred,nchar(pred),nchar(pred)))
# pred.df$pred.age = as.numeric(unlist(sapply(pred.df$pred.age, function(x) return(ifelse(is.na(as.numeric(x)),NA,x)),USE.NAMES = F)))
# pred.df$prey.age = as.numeric(unlist(sapply(pred.df$prey.age, function(x) return(ifelse(is.na(as.numeric(x)),NA,x)),USE.NAMES = F)))
# pred.df$pred = sapply(pred.df$pred,function(x){
#   if(nchar(x) == 5) {
#     return(substr(x,2,4))
#   }else if(nchar(x) == 4 & substr(x,1,1) %in% c(1,2)){
#     return(substr(x,2,3))
#   }else{
#     return(x)
#   }
# },USE.NAMES = F)
# #make wide format
# pred.df2 = reshape2::dcast(pred.df,pred~prey.age,value.var = 'HER',mean)


# pprey.mat =get_pprey_vals(atl.dir = here::here('currentVersion',),
#                biol.file = here::here('currentversion','at_biology.prm'),
#                fgs.file = here::here('currentVersion','neus_groups.csv'),
#                spp.names = 'HER',
#                is.pred = F,
#                remove.zero = T,
#                export.mat = T)
# pred.names = pprey.combs('HER')
# prey.names = 'HER'
# 
# pprey.df = as.data.frame(pprey.mat)
# pprey.df$pred = row.names(pprey.mat)
# 
# pprey.df = pprey.df %>%
#   mutate(pred.age = substr(pred,1,1),
#          prey.age = substr(pred,nchar(pred),nchar(pred)))
# pprey.df$pred.age = as.numeric(unlist(sapply(pprey.df$pred.age, function(x) return(ifelse(is.na(as.numeric(x)),NA,x)),USE.NAMES = F)))
# pprey.df$prey.age = as.numeric(unlist(sapply(pprey.df$prey.age, function(x) return(ifelse(is.na(as.numeric(x)),NA,x)),USE.NAMES = F)))
#   
# pprey.df = pprey.df[,c(91:93,1:90)]
# 
# #juveniles
# juv.pprey.df =pprey.df %>%
#   filter((prey.age == 1 & HER != 0)|pred %in% pred.names[1:2]) 
# juv.pprey.df=juv.pprey.df[,-(which(colSums(juv.pprey.df[4:93])  ==0 )+3)]
# juv.pprey.df[,-(which(colSums(juv.pprey.df[which(juv.pprey.df$pred %in% pred.names[1:2]),4:ncol(juv.pprey.df)])==0)+3)]
# 
#              