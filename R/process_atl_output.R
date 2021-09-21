#' Creates post-processing output for Atlantis
#' 
#' @param.dir string. Path to location of atlantis parameter files
#' @atl.dir string. path to location of atlantis output files
#' @out.dir string. path to desired location of post-processed output
#' @run.prefix string. Prefix for atlantis run output (specified in runcommand.bat)
#' @include_catch logical. Is catch output  turned on in model?
#' @save.out logical. If TRUE, saves .R file, if F returns result object
#' @spatial.overlap logical. Should it include spatial.overlap data?
#' @param.s list generated from get_atl_paramfiles()
#' @diet.agg.time Scale to aggregate dietcheck biomass from (either 'raw','month', or 'year' )
#' @return Either saves an R object or returns a list called "result"
#' 
#' Author: Ryan Morse, modified by Joseph Caracappa

process_atl_output = function(param.dir,
                              atl.dir,
                              out.dir,
                              run.prefix,
                              param.ls,
                              spatial.overlap,
                              include_catch,
                              diet.agg.time,
                              save.out,
                              large.file = F){
  
  # memory.limit(size = 56000)
  source(here::here('R','load_nc_temp.R'))

  #Utility function
  bind.save = function(x,name){
    x2 = dplyr::bind_rows(x)
    saveRDS(x2,paste0(out.dir,name,'.rds'))
  }
  
  #Get boundary box
  bboxes =  atlantistools::get_boundary(boxinfo = atlantistools::load_box(param.ls$bgm.file)) 
  
  #Get epibenthic biopool groups
  bio.pools = atlantistools::load_bps(param.ls$groups.file,param.ls$init.file)
  
  #Get biomass conversion scalar
  bio.conv = atlantistools::get_conv_mgnbiot(param.ls$biol.prm)
  
  #All groups extracted (names, age-structured, biopools, and codes)
  group.names = atlantistools::get_groups(param.ls$groups.file)
  groups.age = atlantistools::get_age_groups(param.ls$groups.file)
  groups.bp = group.names[!group.names %in% groups.age]
  codes.age = atlantistools::get_age_acronyms(param.ls$groups.file)
  groups.data = atlantistools::load_fgs(param.ls$groups.file)
  
# Read Physics ------------------------------------------------------------

  flux = atlantistools::load_nc_physics(nc = param.ls$main.nc, select_physics = c('eflux','vflux'),
                                        prm_run = param.ls$run.prm, bboxes = bboxes)
  source.sink = atlantistools::load_nc_physics(nc = param.ls$main.nc, select_physics = c('hdsource','hdsink'),
                                               prm_run = param.ls$run.prm, bboxes = bboxes)
  phys.statevars = atlantistools::load_nc_physics(nc = param.ls$main.nc, 
                                                  select_physics = c('salt','NO3','NH3','Temp','Chl_a','Oxygen','Light'),
                                                  prm_run = param.ls$run.prm, bboxes = bboxes)  
  
  #Exclude sediment from salinity
  phys.statevars = dplyr::filter(phys.statevars, !(variable == 'salt' & layer == max(layer) & time == min(time) ))
  
  #Read in box properties
  vol.dz = atlantistools::load_nc_physics(nc = param.ls$main.nc, select_physics = c('volume','dz'),
                                          prm_run = param.ls$run.prm, bboxes = bboxes)
  dz = dplyr::filter(vol.dz, variable == 'dz')
  vol = dplyr::filter(vol.dz, variable == 'volume')
  
  #Aggregate volume vertically
  vol.ts = atlantistools::agg_data(vol, groups = c('time','polygon'), fun = sum, out = 'volume')
  
  nominal.dz = as.data.frame(atlantistools::load_init(init = param.ls$init.file, vars = 'nominal_dz') )
  nominal.dz = dplyr::filter(nominal.dz,!is.na(layer))
  
  #write and remove physics objects with no further use
  saveRDS(flux,file = paste0(out.dir,'flux.rds'))
  saveRDS(source.sink,file = paste0(out.dir,'source_sink.rds'))
  saveRDS(phys.statevars,file = paste0(out.dir,'physics_statevars.rds'))
  saveRDS(vol.ts,file = paste0(out.dir,'volume.rds'))
  saveRDS(dz,file = paste0(out.dir,'dz.rds'))
  saveRDS(nominal.dz, file = paste0(out.dir,'nominal_dz.rds'))
  
  
  rm(flux,source.sink,phys.statevars,vol.ts)
  

# Other Parameter Objects -------------------------------------------------

  #Read in age matrix
  data.age.mat = atlantistools::prm_to_df(prm_biol = param.ls$biol.prm, fgs = param.ls$groups.file, 
                                          group = codes.age, parameter = 'age_mat')
  saveRDS(data.age.mat,file = paste0(out.dir,'data_age_mat.rds'))
  
  #Read in diet matrix
  data.diet.mat = atlantistools::load_dietmatrix(prm_biol = param.ls$biol.prm,fgs = param.ls$groups.file, convert_names = T)
  # saveRDS(data.diet.mat,file = paste0(out.dir,'diet_matrix.rds'))
  
  #length.age tempmat
  biol.prm.lines = read.table(param.ls$biol.prm,col.name = 1:100, comment.char = '', fill = T, header = F)
  lia.match =biol.prm.lines[grep('li_a_',biol.prm.lines[,1]),1:20]
  
  tempmat = matrix(NA,nrow = nrow(lia.match), ncol = 3)
  for(igroup in 1:nrow(tempmat)){
    tempmat[igroup,1] = strsplit(as.character(lia.match[igroup,1]),'li_a_')[[1]][2]
  }
  tempmat[,2] = as.numeric(as.character(lia.match[,2]))
  lib.match = grep('li_b_',biol.prm.lines[,1])
  tempmat[,3] = as.numeric(as.character(biol.prm.lines[lib.match,2]))
  
  groups.data2 = groups.data[,c('Code','LongName')]
  tempmat2 = as.data.frame(tempmat[2:dim(tempmat)[1],])
  colnames(tempmat2) = c('Code','li_a','li_b')
  tempmat3 = dplyr::left_join(tempmat2, groups.data2,by = 'Code')
  
  ##Growth relative to initial conditions
  recruit.weight = atlantistools::prm_to_df(prm_biol = param.ls$biol.prm, fgs = param.ls$groups.file,
                                            group = codes.age,
                                            parameter = c('KWRR','KWSR','AgeClassSize'))
  pd = atlantistools::load_init_weight(init = param.ls$init.nofill, fgs = param.ls$groups.file,bboxes = bboxes)
  pd = dplyr::left_join(pd,recruit.weight)
  pd = split(pd,pd$species)
  
  #Calculate weight difference from one ageclass to the next
  for(i in seq_along(pd)){
    pd[[i]]$wdiff = c((pd[[i]]$rn[1] + pd[[i]]$sn[1]) - (pd[[i]]$kwrr[1] + pd[[i]]$kwsr[1]),
                      diff(pd[[i]]$rn + pd[[i]]$sn))
  }
  pd = do.call(rbind,pd)
  pd$growth_req = pd$wdiff/ (365* pd$ageclasssize)
  if( any(pd$growth_req < 0 )){
    warning("Required growth negative for some groups. Please check your initial conditions files.")
    print(unique(pd$species[which(pd$growth_req<0)]))
  }
  unique(pd$species[which(pd$growth_req<0)])
  
  growth.required = dplyr::select(pd, c(species,agecl, growth_req))
  
# Process DietCheck -------------------------------------------------------

  if(large.file==F){
    data.dietcheck = atlantistools::load_dietcheck(dietcheck = param.ls$dietcheck,
                                  fgs = param.ls$groups.file,
                                  prm_run = param.ls$run.prm,
                                  convert_names = T)
    saveRDS(data.dietcheck,file = paste0(out.dir,'data_dietcheck.rds'))
  }else{
    `%>%` = dplyr::`%>%`
    
    ##NEUS only 613 diet obs per timestep
    nsteps =  365/extract_prm(prm_biol = param.ls$run.prm, variables = "toutinc") 
    line.incr = 613 * nsteps
    nline.str = system(paste0('find /c /v "" ',param.ls$dietcheck),intern = T)[2]
    nline = as.numeric(strsplit(nline.str,' ')[[1]][3])
    line.seq = c(seq(0,nline,line.incr),nline)
    
    diet.agg = list()
    
    diet.colnames = colnames(data.table::fread(param.ls$dietcheck,nrow = 1))
    for(i in 1:(length(line.seq)-1)){
      
      #read chunk and aggregate by specified interval
      lines2read = line.seq[i+1]-line.seq[i]
      diet.slice = data.table::fread(param.ls$dietcheck,skip = line.seq[i]+1,nrow = lines2read)
      colnames(diet.slice) = diet.colnames
      
      diet.slice.dates = as.POSIXct(diet.slice$Time*86400, origin = '1964-01-01')
      if(diet.agg.time == 'month') {
        diet.slice$time.agg = as.numeric(factor(format(diet.slice.dates,format = '%m')))
      }else if(diet.agg.time == 'year'){
        diet.slice$time.agg = as.numeric(factor(format(diet.slice.dates, format = '%Y')))
      }else{
        diet.slice$time.agg = 1:nrow(diet.slice)
      }
      
      diet.slice = diet.slice %>%
        dplyr::group_by(time.agg,Predator,Cohort,Stock,Updated)%>%
        dplyr::summarise(Time = max(Time),
                         dplyr::across(MAK:DC,mean))%>%
        dplyr::ungroup()%>%
        dplyr::select(-time.agg)%>%
        dplyr::select(Time,Predator,Cohort,Stock,Updated,MAK:DC)%>%
        dplyr::arrange(Time,Predator,Cohort,Stock,Updated)
      
      pred.sum =apply(diet.slice[,6:93],1,sum,na.rm=T)
      
      diet.slice[,6:93] = diet.slice[,6:93]/pred.sum
      diet.slice = diet.slice[which(pred.sum !=0),]
      
      write.table(diet.slice,file = paste0(out.dir,'dietcheck_temp.txt'))
      diet.agg[[i]] = atlantistools::load_dietcheck(dietcheck = paste0(out.dir,'dietcheck_temp.txt'),
                                                    fgs = param.ls$groups.file,
                                                    prm_run = param.ls$run.prm,
                                                    convert_names = T)
      print(i)
    }
    
    data.dietcheck = dplyr::bind_rows(diet.agg)
    rm(diet.agg)
    saveRDS(data.dietcheck,file = paste0(out.dir,'data_dietcheck.rds'))
  }
  
  
# Main NetCDF objects -----------------------------------------------------

  #Set up biological variable groups
  group.types = dplyr::bind_rows(list(data.frame(species = groups.age,group = 'age'),
       data.frame(species = groups.bp, group = 'bp'))
  )
  age.vars = c('Nums','StructN','ResN','N')
  bp.vars = 'N'
  
  #Loop by species to save on memory

  #Setup lists for output objects
  {numbers = list()
  numbers.age = list()
  numbers.box = list()
  RN.box = list()
  SN.box = list()
  RN.age = list()
  SN.age = list()
  RN.age.mean = list()
  SN.age.mean = list()
  biomass.age = list()
  biomass.age.invert = list()
  spatial.biomass = list()
  spatial.biomass.stanza = list()
  biomass = list()
  biomass.box = list()
  sp.overlap = list()
  biomass.box.invert = list()
  length.age = list()}
  
  if(large.file==F){
    vars = list('Nums','StructN','ResN','N')
    group.types = list(groups.age,groups.age,groups.age,groups.bp)
    rawdata.main = Map(atlantistools::load_nc,
                       select_variable = vars,
                       select_groups = group.types,
                       MoreArgs = list(nc = param.ls$main.nc, bps = bio.pools,
                                       fgs = param.ls$groups.file,prm_run = param.ls$run.prm,
                                       bboxes = bboxes ))
    #numbers
    numbers = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','time'), fun = sum)
    numbers.age = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','agecl','time'), fun = sum)
    numbers.box[[i]] = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','polygon','time'), fun = sum)
    
    RN.box[[i]] = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','polygon','time'), fun = sum)
    SN.box[[i]] = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','polygon','time'), fun = sum)
    RN.age[[i]] = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','agecl','time'), fun = sum)
    SN.age[[i]] = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','agecl','time'), fun = sum)
    
    SN.age.mean[[i]] = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','time','agecl'), fun = mean)
    RN.age.mean[[i]] = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','time','agecl'), fun = mean)
    
    #make length.age
    #Use mean RN+SN per age for each species, convert to weight, get length w/Von Bert.
    RN.SN.age = dplyr::left_join(RN.age[[i]],SN.age[[i]],by = c('species','agecl','time'))
    colnames(RN.SN.age) = c('species','agecl','time','RN','SN')
    
    biomass.age2 = dplyr::left_join(RN.SN.age, tempmat3[,2:4], by = c('species'='LongName'))
    biomass.age2$grams_N_Ind = (biomass.age2$RN+biomass.age2$SN)*5.7*20/1000
    biomass.age2$length_age = (as.numeric(as.character(biomass.age2$grams_N_Ind))/as.numeric(as.character(biomass.age2$li_a)))^(1/as.numeric(as.character(biomass.age2$li_b)))
    length.age[[i]] = biomass.age2[,c('species','agecl','time','length_age')]
    colnames(length.age[[i]])[4] = 'atoutput'
    
    spatial.biomass = atlantistools::calculate_biomass_spatial(nums = rawdata.main[[1]],
                                                                    sn = rawdata.main[[2]],
                                                                    rn = rawdata.main[[3]],
                                                                    n = rawdata.main[[4]],
                                                                    vol_dz = vol.dz,
                                                                    bio_conv = bio.conv,
                                                                    bps = bio.pools)
    
    spatial.biomass.stanza = atlantistools::combine_ages(spatial.biomass,grp_col = 'species',agemat = data.age.mat)
    
    #spatial biomass
    
    rm(rawdata.spp)
    
    ##Biomass Groups and spatial overlap
    
    biomass <- atlantistools::agg_data(spatial.biomass,groups = c('species','time'),fun = sum)
    
    #Aggregate biomass by box
    biomass.box = atlantistools::agg_data(spatial.biomass, groups = c('species','polygon','time'), fun = sum)
    biomass.box.invert = dplyr::filter(biomass.box, !(species %in% data.age.mat$species))
    
    #Aggregate by ageclass
    biomass.age = dplyr::filter(spatial.biomass, species %in% data.age.mat$species)
    biomass.age = atlantistools::agg_data(biomass.age, groups = c('species','agecl','time'),fun = sum)  
    
    biomass.age.invert = dplyr::filter(spatial.biomass, !(species %in% data.age.mat$species))
    biomass.age.invert = atlantistools::agg_data(biomass.age.invert,groups = c('species','time'),fun = sum)
    
    
    
  }else{
    for(i in 1:nrow(group.types)){
      
      blank.df = data.frame(species = group.types$species[i], polygon =NA, agecl = NA,layer = NA, time = 0, atoutput = NA)
      if(group.types$group[i] == 'age'){
        main.vars = age.vars
      }else{
        main.vars = bp.vars
      }
      
      ## (1) Process Raw Data
      #Read in raw untransformed data from main.nc file
      if(load_fgs(param.ls$groups.file)$IsTurnedOn[i] == 0){
        rawdata.spp = list(data.frame(species = group.types$species[i], polygon =NA, agecl = NA,layer = NA, time = 0, atoutput = NA))
      }else{
        rawdata.spp =Map(load_nc_temp,
                         select_variable = main.vars,
                         select_groups = group.types$species[i],
                         MoreArgs = list(nc = param.ls$main.nc, bps = bio.pools,
                                         fgs = param.ls$groups.file,prm_run = param.ls$run.prm,
                                         bboxes = bboxes ))
      }
      
      if(group.types$group[i] == 'bp'){
        rawdata.spp[[1]]$agecl =1
      }
      
      ## (2) create 1st level objects( Spatial Biomass, Numbers, RN/SN,)
      
      
      #SN/RN
      if(group.types$group[i] == 'age'){
        
        #numbers
        numbers[[i]] = atlantistools::agg_data(data = rawdata.spp[[1]], groups = c('species','time'), fun = sum)
        numbers.age[[i]] = atlantistools::agg_data(data = rawdata.spp[[1]], groups = c('species','agecl','time'), fun = sum)
        numbers.box[[i]] = atlantistools::agg_data(data = rawdata.spp[[1]], groups = c('species','polygon','time'), fun = sum)
        
        RN.box[[i]] = atlantistools::agg_data(data = rawdata.spp[[3]], groups = c('species','polygon','time'), fun = sum)
        SN.box[[i]] = atlantistools::agg_data(data = rawdata.spp[[2]], groups = c('species','polygon','time'), fun = sum)
        RN.age[[i]] = atlantistools::agg_data(data = rawdata.spp[[3]], groups = c('species','agecl','time'), fun = sum)
        SN.age[[i]] = atlantistools::agg_data(data = rawdata.spp[[2]], groups = c('species','agecl','time'), fun = sum)
        
        SN.age.mean[[i]] = atlantistools::agg_data(data = rawdata.spp[[2]], groups = c('species','time','agecl'), fun = mean)
        RN.age.mean[[i]] = atlantistools::agg_data(data = rawdata.spp[[3]], groups = c('species','time','agecl'), fun = mean)
        
        #make length.age
        #Use mean RN+SN per age for each species, convert to weight, get length w/Von Bert.
        RN.SN.age = dplyr::left_join(RN.age[[i]],SN.age[[i]],by = c('species','agecl','time'))
        colnames(RN.SN.age) = c('species','agecl','time','RN','SN')
        
        biomass.age2 = dplyr::left_join(RN.SN.age, tempmat3[,2:4], by = c('species'='LongName'))
        biomass.age2$grams_N_Ind = (biomass.age2$RN+biomass.age2$SN)*5.7*20/1000
        biomass.age2$length_age = (as.numeric(as.character(biomass.age2$grams_N_Ind))/as.numeric(as.character(biomass.age2$li_a)))^(1/as.numeric(as.character(biomass.age2$li_b)))
        length.age[[i]] = biomass.age2[,c('species','agecl','time','length_age')]
        colnames(length.age[[i]])[4] = 'atoutput'
        
        spatial.biomass[[i]] = atlantistools::calculate_biomass_spatial(nums = rawdata.spp[[1]],
                                                                        sn = rawdata.spp[[2]],
                                                                        rn = rawdata.spp[[3]],
                                                                        n = rawdata.spp[[4]],
                                                                        vol_dz = vol.dz,
                                                                        bio_conv = bio.conv,
                                                                        bps = bio.pools)
      }else{
        spatial.biomass[[i]] = atlantistools::calculate_biomass_spatial(nums = blank.df,
                                                                        sn = blank.df,
                                                                        rn = blank.df,
                                                                        n = rawdata.spp[[1]],
                                                                        vol_dz = vol.dz,
                                                                        bio_conv = bio.conv,
                                                                        bps = bio.pools)
      }
      
      
      
      #Aggregate spatial biomass based on stanzas
      spatial.biomass.stanza[[i]] = atlantistools::combine_ages(spatial.biomass[[i]],grp_col = 'species',agemat = data.age.mat)
      
      print(group.types$species[i])
      
      # if(spatial.overlap == F){
      #   rm(spatial.biomass)
      # }
      
      #spatial biomass
      
      rm(rawdata.spp)
      
      ##Biomass Groups and spatial overlap
      
      biomass[[i]] <- atlantistools::agg_data(spatial.biomass[[i]],groups = c('species','time'),fun = sum)
      
      #Aggregate biomass by box
      biomass.box[[i]] = atlantistools::agg_data(spatial.biomass[[i]], groups = c('species','polygon','time'), fun = sum)
      biomass.box.invert[[i]] = dplyr::filter(biomass.box[[i]], !(species %in% data.age.mat$species))
      
      #Aggregate by ageclass
      biomass.age[[i]] = dplyr::filter(spatial.biomass[[i]], species %in% data.age.mat$species)
      biomass.age[[i]] = atlantistools::agg_data(biomass.age[[i]], groups = c('species','agecl','time'),fun = sum)  
      
      biomass.age.invert[[i]] = dplyr::filter(spatial.biomass[[i]], !(species %in% data.age.mat$species))
      biomass.age.invert[[i]] = atlantistools::agg_data(biomass.age.invert[[i]],groups = c('species','time'),fun = sum)
      
      gc()
    }
  }
 

  #Calculate spatial overlap
  # if(spatial.overlap){
  #   spatial.biomass = dplyr::bind_rows(spatial.biomass)
  #   sp.overlap = atlantistools::calculate_spatial_overlap(biomass_spatial = spatial.biomass,dietmatrix = data.diet.mat, agemat = data.age.mat )  
  #   saveRDS(sp.overlap, paste0(out.dir,'spatial_overlap.rds'))
  #   rm(spatial.biomass,sp.overlap)
  # }
  
  #bind, save and delete objects
  bind.save(numbers,'numbers')
  bind.save(numbers.age,'numbers_age')
  bind.save(numbers.box,'numbers_box')
  
  bind.save(RN.box,'RN_box')
  bind.save(SN.box,'SN_box')
  bind.save(RN.age,'RN_age')
  bind.save(SN.age,'SN_age')

  bind.save(SN.age.mean,'SN_age_mean')
  bind.save(RN.age.mean,'RN_age_mean')
  
  bind.save(length.age,'length_age')
  
  bind.save(biomass,'biomass')
  bind.save(biomass.box,'biomass_box')
  bind.save(biomass.box.invert,'biomass_box_invert')
  bind.save(biomass.age,'biomass_age')
  bind.save(biomass.age.invert,'biomass_age_invert')
  bind.save(spatial.biomass.stanza,'biomass_spatial_stanza')
  
  rm(numbers,numbers.age,numbers.box,
     RN.box,SN.box,RN.age,SN.age,
     SN.age.mean,RN.age.mean,length.age,
     biomass,biomass.box,biomass.box.invert,
     biomass.age,biomass.age.invert,
     spatial.biomass.stanza)
  
# PROD netCDF objects -----------------------------------------------------
  
  #Read in raw untransformed data from prod.nc file
  
  #Set up biological variable groups
  age.vars = c('Eat','Growth')
  bp.vars = 'Grazing'
  
  #Setup spaces for objects
  eat.age = list()
  grazing = list()
  growth.age = list()
  growth.rel.init = list()
  bio.consumed = list()
  
  if(large.file == F){
    vars = list('Eat','Grazing','Growth')
    group.types = list(groups.age,groups.bp,groups.age)
    rawdata.prod = Map(atlantistools::load_nc,
                       select_variable = vars,
                       select_groups = group.types,
                       MoreArgs = list(nc = param.ls$prod.nc, bps = bio.pools,
                                       fgs = param.ls$groups.file, prm_run = param.ls$run.prm,
                                       bboxes = bboxes))
    
    bio.consumed = atlantistools::calculate_consumed_biomass(eat = rawdata.prod[[1]],
                                                             grazing = rawdata.prod[[2]],
                                                             dm = data.dietcheck,
                                                             vol = vol,
                                                             bio_conv = bio.conv)
    
    eat.age = atlantistools::agg_data(data = rawdata.prod[[1]], groups = c('species','time','agecl'),fun = mean)
    grazing = atlantistools::agg_data(data = rawdata.prod[[2]], groups = c('species','time'),fun = mean)
    growth.age =  atlantistools::agg_data(data = rawdata.prod[[3]], groups = c('species','time','agecl'),fun = mean)
    
    
  }else{
    for(i in 1:nrow(group.types)){
      
      if(group.types$group[i] == 'age'){
        prod.vars = age.vars
      }else{
        prod.vars = bp.vars
      }
      
      ## Process PROD Data by spp
      if(load_fgs(param.ls$groups.file)$IsTurnedOn[i] == 0){
        proddata.spp = list(data.frame(species = group.types$species[i], polygon =NA, agecl = NA,layer = NA, time = 0, atoutput = NA))
      }else{
        proddata.spp = Map(load_nc_temp,
                           select_variable = prod.vars,
                           select_groups = group.types$species[i],
                           MoreArgs = list(nc = param.ls$prod.nc, bps = bio.pools,
                                           fgs = param.ls$groups.file, prm_run = param.ls$run.prm,
                                           bboxes = bboxes))
      }
      if(is.null(nrow(proddata.spp[[1]]))){
        print(i)
        next()
      }else if(group.types$group[i] != 'age'){
        grazing[[i]] = proddata.spp[[1]] %>%
          filter(time %in% unique(data.dietcheck$time))
      }else{
        eat.age[[i]] = proddata.spp[[1]] %>%
          filter(time %in% unique(data.dietcheck$time))
        growth.age[[i]] = proddata.spp[[2]] %>%
          filter(time %in% unique(data.dietcheck$time))
      }
      
      print(i)
    }
    
    grazing = dplyr::bind_rows(grazing)
    eat.age = dplyr::bind_rows(eat.age)
    
    vol.temp = dplyr::filter(vol,time %in% unique(data.dietcheck$time))
    bio.consumed = atlantistools::calculate_consumed_biomass(eat = eat.age,
                                                             grazing = grazing,
                                                             dm = data.dietcheck,
                                                             vol = vol.temp,
                                                             bio_conv = bio.conv)
  }


  growth.age = dplyr::bind_rows(growth.age)
  growth.age =  atlantistools::agg_data(data = growth.age, groups = c('species','time','agecl'),fun = mean)
  #make growth.rel.init
  growth.rel.init = dplyr::left_join(growth.age,growth.required)  
  growth.rel.init = dplyr::mutate(growth.rel.init, gr_rel = (atoutput - growth_req) / growth_req)
  
  which.inf = which(growth.rel.init$gr_rel == 'Inf')
  growth.rel.init$gr_rel[which.inf] = 1
  
  saveRDS(growth.rel.init,paste0(out.dir,'growth_rel_init.rds'))
  saveRDS(growth.age,paste0(out.dir,'growth_age.rds'))
  
  rm(growth.age,growth.rel.init)


    #aggregate other prod objects
  grazing = atlantistools::agg_data(data = grazing, groups = c('species','time'),fun = mean)
  eat.age = atlantistools::agg_data(data = eat.age, groups = c('species','time','agecl'),fun = mean)
  

  #write to file and remove
  saveRDS(grazing,paste0(out.dir,'grazing.rds'))
  saveRDS(eat.age,paste0(out.dir,'eat_age.rds'))
  
  saveRDS(bio.consumed,paste0(out.dir,'biomass_consumed.rds'))

  rm(grazing,eat.age,growth.age,bio.consumed,growth.rel.init)

# Do Recruitment ----------------------------------------------------------
  ssb.recruits = atlantistools::load_rec(yoy = param.ls$yoy, ssb = param.ls$ssb,prm_biol = param.ls$biol.prm )
  saveRDS(ssb.recruits,paste0(out.dir,'ssb_recruits.RDS'))
  rm(ssb.recruits)

# Do catch -------------------------------------------------------------------

  if(include_catch){
    catch = atlantistools::load_nc(param.ls$catch,
                                   fgs = param.ls$groups.file,
                                   bps = bio.pools,
                                   select_groups = group.names,
                                   select_variable = 'Catch',
                                   prm_run = param.ls$run.prm,
                                   bboxes = bboxes,check_acronyms = F)  
    totcatch = atlantistools::agg_data(catch, groups = c('species','time','agecl'), fun = sum)
    catchmt <- atlantisom::load_catch(dir=atl.dir,file_catch=paste0(run.prefix,"Catch.txt"),fgs = groups.data) %>%
      dplyr::select(species,time,atoutput) %>%
      dplyr::mutate(time = time/365)
    
    # saveRDS(catch,paste0(out.dir,'catch.RDS'))
    saveRDS(totcatch,paste0(out.dir,'totcatch.RDS'))
    saveRDS(catchmt,paste0(out.dir,'catchmt.RDS'))
    
    rm(catch,totcatch,catchmt)
  }
  
  # Do mortality -------------------------------------------------------------------
   mortality <- atlantistools::load_mort(param.ls$mort,prm_run=param.ls$run.prm,fgs=param.ls$groups.file,convert_names = T) #%>%
  #   dplyr::group_by(species,time) %>% 
  #   dplyr::summarise(atoutput = atoutput[source == "F"]/atoutput[source == "M"],.groups="drop") %>% 
  #   dplyr::mutate(atoutput = ifelse(is.infinite(atoutput),NA,atoutput))
  saveRDS(mortality,paste0(out.dir,'mort.RDS'))
  
  specificMortality <- atlantistools::load_spec_mort(param.ls$specificmort,prm_run=param.ls$run.prm,fgs=param.ls$groups.file,convert_names = T,removeZeros = F)
  saveRDS(specificMortality,paste0(out.dir,'specificmort.RDS'))
}
