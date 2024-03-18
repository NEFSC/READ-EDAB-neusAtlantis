# group.name = 'DOG'
# init.file = here::here('currentVersion','neus_init.nc')
# fgs.file = here::here('diagnostics','neus_groups_v2_0_1.csv')
# run.file = here::here('currentVersion','at_run.prm')

get_init_spatial_dist = function(init.file,fgs.file,run.file, group.name,print.cdf=F){
  
  library(ncdf4)
  
  init = ncdf4::nc_open(init.file)
  fgs = read.csv(fgs.file,as.is =T)
  
  n.age = fgs$NumCohorts[which(fgs$Code == group.name)]
  spp = fgs$Name[which(fgs$Code == group.name)]
  
  varnames = paste0(spp,1:n.age,'_Nums')
  
  bio.file = here::here('currentVersion','at_biology.prm')
  biolines = readLines(bio.file)
  spp.spat.ad = paste0('F',group.name,'_S',1,'\\b')
  spp.spat.juv =paste0('F',group.name,'_S',1,'juv\\b')
  
  #age at maturity
  mat.age = as.numeric(strsplit(biolines[grep(paste0(group.name,'_age_mat'),biolines)],'\t| ')[[1]][2])+1
  
  #get initial scalar
  source(here::here('R','Calibration_Tools','edit_param_init_scalar.R'))
  init.scalar = get_param_init_scalar(run.prm = run.file ,
                                      groups.file = fgs.file,
                                      write.output = F)
  # spp.scalar = as.numeric(as.character(init.scalar[which(init.scalar$group == group.name),2]))
  spp.scalar =1
  
  # out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Init/'
  box.mat = list()
  for(i in 1:length(varnames)){
    
    x = ncvar_get(init,varnames[i])[1,]
    # x.tot = sum(x,na.rm=T)
    # x.prop = x/x.tot
    # 
    # new.tot = x.tot * spp.scalar
    # 
    # if(i < mat.age){
    #   line.match = grep(spp.spat.juv,biolines)+1
    #     
    #   line.string = biolines[line.match]
    #   line.split = as.numeric(strsplit(line.string,'\t| ')[[1]])
    # }  
    
    if(print.cdf){
      new.x = x*spp.scalar
      new.x = paste0(new.x,',')
      new.x[which(new.x == 'NA,')] = '_,'
      
      out.mat = matrix('_,',nrow = 30, ncol = 5)
      out.mat[,1] = new.x
      out.mat[30,5] = '_ ;'
    }else{
     box.mat[[i]] = x
    }

    
    # write.table(out.mat,file = paste0(out.dir,varnames[i],'.txt'),quote = F, row.names = F)
    
  }
  
  if(print.cdf){
    return(out.mat)    
  }else{
    box.mat = do.call('rbind',box.mat)
    return(box.mat)
  }

  
  # curve( (13.4-127*(x+1))/-0.75 - 463, 0, 2.65)
}

