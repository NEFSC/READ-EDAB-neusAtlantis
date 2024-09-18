group.name = 'DOG'
init.file = here::here('currentVersion','neus_init.nc')
fgs.file = here::here('diagnostics','neus_groups_v2_0_1.csv')
run.file = here::here('currentVersion','at_run.prm')
bgm.file = here::here('geometry','neus_tmerc_RM2.bgm')

get_init_spatial_dist = function(init.file,fgs.file,run.file,bgm.file, group.name,convert.biomass,print.cdf=F){
  
  library(ncdf4)
  
  init = ncdf4::nc_open(init.file)
  fgs = read.csv(fgs.file,as.is =T)
  
  is.epi = fgs$GroupType[which(fgs$Code == group.name)] %in% c('SED_EP_FF','SED_EP_OTHER','MOB_EP_OTHER')
  n.age = fgs$NumCohorts[which(fgs$Code == group.name)]
  spp = fgs$Name[which(fgs$Code == group.name)]
  
  init.vars = names(init$var)
  if(n.age==1){
    var.N = paste0(spp,'_N')
  }else if (n.age == 2){
    var.N = paste0(spp,'_N',1:n.age)
  }else{
    var.Num =paste0(spp,1:n.age,'_Nums')
    var.RN =paste0(spp,1:n.age,'_ResN')
    var.SN =paste0(spp,1:n.age,'_StructN')
  }
  
  
  bio.file = here::here('currentVersion','at_biology.prm')
  biolines = readLines(bio.file)
  
  bgm = rbgm::bgmfile(bgm.file)
  box.area = bgm$boxes$area
  box.vol.z1 = -bgm$boxes$botz
  box.vol.z1[box.vol.z1>50] = 50
  box.vol.z1 = box.vol.z1 * box.area
  
  #age at maturity
  mat.age = as.numeric(strsplit(biolines[grep(paste0(group.name,'_age_mat'),biolines)],'\t| ')[[1]][2])+1
  
  #get initial scalar(not sure if this should be applied or not)
  # source(here::here('R','Calibration_Tools','edit_param_init_scalar.R'))
  # init.scalar = get_param_init_scalar(run.prm = run.file ,
  #                                     groups.file = fgs.file,
  #                                     write.output = F)
  # spp.scalar = as.numeric(as.character(init.scalar[which(init.scalar$group == group.name),2]))
  spp.scalar =1
  
  # out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Init/'
  box.mat = list()
  for(i in 1:n.age){
    
    if(n.age == 1){
      val.N = ncvar_get(init,var.N)
      
      if(convert.biomass){
        
        if(is.epi){
          val.bio = val.N * box.area  
        }else{
          val.bio = val.N * box.vol.z1
        }
        
      }
    }else if(n.age == 2){
      val.N = ncvar_get(init,var.N[i])
      
      if(convert.biomass){
        
        if(is.epi){
          val.bio = val.N * box.area  
        }else{
          val.bio = val.N * box.vol.z1
        }
      }
    }else {
      val.Num = ncvar_get(init,var.Num[i])
      val.RN = ncvar_get(init,var.RN[i])
      val.SN = ncvar_get(init,var.SN[i])
      
      if(convert.biomass){
        val.bio = val.Num * (val.RN+val.SN)
      }
    }
    
    ####
    #figure out way to export to be used by edit function
    ####
    
    
    #I think this was meant to print to the CDF then knit, but working on better workflow instead
    # if(print.cdf){
    #   new.x = x*spp.scalar
    #   new.x = paste0(new.x,',')
    #   new.x[which(new.x == 'NA,')] = '_,'
    #   
    #   out.mat = matrix('_,',nrow = 30, ncol = 5)
    #   out.mat[,1] = new.x
    #   out.mat[30,5] = '_ ;'
    # }else{
    #  box.mat[[i]] = x
    # }

    
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

edit_init_spatial_dist = function(init.file,fgs.file,)

get_init_spatial_dist(
  group.name = 'SCA',
  init.file = here::here('currentVersion','neus_init.nc'),
  fgs.file = here::here('diagnostics','neus_groups_v2_0_1.csv'),
  run.file = here::here('currentVersion','at_run.prm')
)
