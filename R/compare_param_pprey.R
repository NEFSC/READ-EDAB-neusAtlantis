#Function to compare pPREY parameters from multiple parameter files

bio.files = c(here::here('currentVersion','at_biology.prm'),
              here::here('currentVersion','at_biology_10302020.prm'),
              here::here('currentVersion','at_biology_master.prm'))
              # 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/at_biol_neus_v15_scaled_diet_20181126_3.prm',)
atl.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
source(here::here('R','edit_param_pprey.R'))


compare_param_pprey = function(atl.dir,bio.files,fgs.file,file.names, remove.similar = F){
  
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
    pprey.all.out$is.similar = apply(pprey.all.out[,3:(2+length(file.names))],1,function(x) return(length(unique(as.numeric(x)))>1) )
    pprey.all.out = dplyr::filter(pprey.all.out,is.similar == T)
    pprey.all.out = dplyr::select(pprey.all.out,-is.similar)
  }
  
  return(pprey.all.out)
}

pprey.out = compare_param_pprey(atl.dir,bio.files,
                    fgs.file = here::here('currentVersion','neus_groups.csv'),
                    file.names = c('Bio_JC','Bio_Orig','Bio_RG'),
                    remove.similar=T)
write.csv(pprey.out,here::here('Diagnostics','pprey_comparison_12102020.csv'),row.names = F)

