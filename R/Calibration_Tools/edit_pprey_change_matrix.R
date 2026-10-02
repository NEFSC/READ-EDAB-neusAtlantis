#Function to compare pPREY parameters from multiple parameter files

bio.files = c(here::here('currentVersion','at_biology.prm'),
              here::here('currentVersion','at_biology_12282020.prm'),
              here::here('currentVersion','at_biology_1222021.prm'))
              # 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/at_biol_neus_v15_scaled_diet_20181126_3.prm',)
atl.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
source(here::here('R','edit_param_pprey.R'))


compare_param_pprey = function(atl.dir,bio.files,fgs.file,file.names, remove.similar = F,diff.code = 1){
  
  file.pprey.ls = list()
  for(i in 1:length(bio.files)){
    
    #Read in pPREY data from each bio file (done by specifying spp.names = NULL in get_pprey_vals)
    
    file.pprey = get_pprey_vals(atl.dir =atl.dir,
                   biol.file = bio.files[i],
                   fgs.file = here::here('currentVersion','neus_groups.csv'),
                   spp.names = NULL
                   )
    
    #Convert to longform interactions (pred.name,prey.name,pprey.val)
    file.pprey.long = reshape2::melt(file.pprey,id.vars = 'pred.names',
                                     variable.name = 'prey.names',
                                     value.name = paste0(file.names[i],'.pprey'))
    file.pprey.long[,3] = as.numeric(file.pprey.long[,3])
    file.pprey.ls[[i]] = file.pprey.long
  }
  
  #Join data horizontally
  # pprey.all.out = dplyr::bind_cols(file.pprey.ls)
  pprey.all.out = plyr::join_all(file.pprey.ls)
  
  if(remove.similar){
    pprey.all.out$is.similar = apply(pprey.all.out[,3:(2+length(file.names))],1,function(x) return(length(unique(as.numeric(x)))>=diff.code) )
    pprey.all.out = dplyr::filter(pprey.all.out,is.similar == T)
    pprey.all.out = dplyr::select(pprey.all.out,-is.similar)
  }
  
  return(pprey.all.out)
}

pprey.out = compare_param_pprey(atl.dir,bio.files,
                    fgs.file = here::here('currentVersion','neus_groups.csv'),
                    file.names = c('Bio_JC','Bio_Orig','Bio_RG'),
                    remove.similar=T,
                    diff.code = 3)
write.csv(pprey.out,here::here('Diagnostics','pprey_comparison_1222021.csv'),row.names = F)


#Read in merge conflict file and create new changes sheet
new.pprey.change = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Diet/merge_master_01252021.csv')
new.pprey = data.frame(pred = new.pprey.change$pred.names,prey = new.pprey.change$prey.names, value = NA)
keep.id =sapply(new.pprey.change$version,function(x){
  if(x == 'J'){
    return(1)
  }else if(x == 'O'){
    return(2)
  }else if(x == 'R'){
    return(3)
  }else{
    return(NA)
  }
})

new.pprey$value = new.pprey.change[as.matrix(data.frame(r = 1:nrow(new.pprey),c = keep.id+2))]
edit_param_pprey(
  atl.dir = atl.dir,
  biol.file = paste0(atl.dir,'at_biology.prm'),
  fgs.file = paste0(atl.dir,'neus_groups.csv'),
  pred.list = new.pprey[,1],
  prey.list = new.pprey[,2],
  pprey.vals = new.pprey[,3],
  overwrite = T,
  new.file.name = NA
)
