#' Creates post-processing output for Atlantis
#' 
#' @param.dir string. Path to location of atlantis parameter files
#' @atl.dir string. path to location of atlantis output files
#' @out.dir string. path to desired location of post-processed output
#' @run.prefix string. Prefix for atlantis run output (specified in runcommand.bat)
#' @include_catch logical. Is catch output  turned on in model?
#' @save.out logical. If TRUE, saves .R file, if F returns result object
#' @spatial.overlap logical. Should it include spatial.overlap data?
#' @bgm.file string. Full file name for bgm file
#' @groups.file string. Full file name for functional groups file
#' @init.file string. Full file name for initial conditions netCDF (this should be the version without fillvalues)
#' @biol.prm string. Full file name for biology parameter file
#' @run.prm string. Full file name for run parameter file
#' @main.nc string. Full file name for the main atlantis output netCDF
#' @prod.nc string. Full file name for the PROD output netCDF
#' @dietcheck string. Full file name for the dietcheck.txt file
#' @yoy string. Full file name for the YOT.txt file
#' @catch.file string. Full file name for the CATCH.nc file
#' @totcatch.file string. Full file name for the TOTCATCH.nc file 
#' 
#' @return Either saves an R object or returns a list called "result"
#' 
#' Author: Ryan Morse, modified by Joseph Caracappa

process_atl_output = function(param.dir,atl.dir,out.dir,bgm.file,groups.file,init.file,biol.prm,
                              main.nc,prod.nc,dietcheck,ssb,yoy,
                              spatial.overlap,include_catch,catch.file,
                              totcatch.file,save.out,run.prefix,run.prm){
  
  #Get boundary box
  bboxes =  atlantistools::get_boundary(boxinfo = atlantistools::load_box(bgm.file)) 
  
  #Get epibenthic biopool groups
  bio.pools = atlantistools::load_bps(groups.file,init.file)
  
  #Get biomass conversion scalar
  bio.conv = atlantistools::get_conv_mgnbiot(biol.prm)
  
  #All groups extracted (names, age-structured, biopools, and codes)
  group.names = atlantistools::get_groups(groups.file)
  groups.age = atlantistools::get_age_groups(groups.file)
  groups.bp = group.names[!group.names %in% groups.age]
  codes.age = atlantistools::get_age_acronyms(groups.file)
  groups.data = atlantistools::load_fgs(groups.file)
  
  #Read in raw untransformed data from main.nc file
  vars = list('Nums','StructN','ResN','N')
  group.types = list(groups.age,groups.age,groups.age,groups.bp)
  rawdata.main = Map(atlantistools::load_nc,
                 select_variable = vars,
                 select_groups = group.types,
                 MoreArgs = list(nc = main.nc, bps = bio.pools,
                                 fgs = groups.file,prm_run = run.prm,
                                 bboxes = bboxes ))
  
  #Read in raw untransformed data from prod.nc file
  vars = list('Eat','Grazing','Growth')
  group.types = list(groups.age,groups.bp,groups.age)
  rawdata.prod = Map(atlantistools::load_nc,
                      select_variable = vars,
                      select_groups = group.types,
                      MoreArgs = list(nc = prod.nc, bps = bio.pools,
                                      fgs = groups.file, prm_run = run.prm,
                                      bboxes = bboxes)) 
  
  #Read in production (no predators)
  # groups.nopred = groups.data$Name[groups.data[,names(groups.data)[names(groups.data) %in% c('isPredator','IsPredator')]] == 0][4:6]
  groups.nopred = c('Diatom','PicoPhytopl')
  # groups.nopred = 'Pelag_Bact'
  vars = list('Prodn')
  rawdata.nopred.prod = Map(atlantistools::load_nc,
                            select_variable = vars,
                            select_groups = groups.nopred,
                            MoreArgs = list(nc = prod.nc, bps = bio.pools,
                                            fgs = groups.file,prm_run = run.prm,
                                            bboxes = bboxes))
  
  #Read in physics
  flux = atlantistools::load_nc_physics(nc = main.nc, select_physics = c('eflux','vflux'),
                                        prm_run = run.prm, bboxes = bboxes)
  source.sink = atlantistools::load_nc_physics(nc = main.nc, select_physics = c('hdsource','hdsink'),
                                               prm_run = run.prm, bboxes = bboxes)
  phys.statevars = atlantistools::load_nc_physics(nc = main.nc, 
                                                  select_physics = c('salt','NO3','NH3','Temp','Chl_a','Oxygen','Light'),
                                                  prm_run = run.prm, bboxes = bboxes)  
  
  #Exclude sediment from salinity
  phys.statevars = dplyr::filter(phys.statevars, !(variable == 'salt' & layer == max(layer) & time == min(time) ))
  
  #Read in box properties
  vol.dz = atlantistools::load_nc_physics(nc = main.nc, select_physics = c('volume','dz'),
                                          prm_run = run.prm, bboxes = bboxes)
  dz = dplyr::filter(vol.dz, variable == 'dz')
  vol = dplyr::filter(vol.dz, variable == 'volume')
  
  nominal.dz = as.data.frame(atlantistools::load_init(init = init.file, vars = 'nominal_dz') )
  nominal.dz = dplyr::filter(nominal.dz,!is.na(layer))
  
  #Read in Dietcheck
  data.dietcheck = atlantistools::load_dietcheck(dietcheck = dietcheck,fgs = groups.file,
                                                 prm_run = run.prm, convert_names = T)
  
  #Read in SSB 
  data.ssb.recruit = atlantistools::load_rec(yoy = yoy, ssb = ssb,prm_biol = biol.prm )
  
  #Read in age matrix
  data.age.mat = atlantistools::prm_to_df(prm_biol = biol.prm, fgs = groups.file, 
                                          group = codes.age, parameter = 'age_mat')
  
  #Read in diet matrix
  data.diet.mat = atlantistools::load_dietmatrix(prm_biol = biol.prm,fgs = groups.file, convert_names = T)
  
  #Calculate biomass spatially
  spatial.biomass = atlantistools::calculate_biomass_spatial(nums = rawdata.main[[1]],
                                                             sn = rawdata.main[[2]],
                                                             rn = rawdata.main[[3]],
                                                             n = rawdata.main[[4]],
                                                             vol_dz = vol.dz,
                                                             bio_conv = bio.conv,
                                                             bps = bio.pools)
  
  #Aggregate spatial biomass based on stanzas
  spatial.biomass.stanza = atlantistools::combine_ages(spatial.biomass,grp_col = 'species',agemat = data.age.mat)
  
  #Aggregate biomass
  biomass <- atlantistools::agg_data(spatial.biomass,groups = c('species','time'),fun = sum)
  
  #Aggregate biomass by box
  biomass.box = atlantistools::agg_data(spatial.biomass, groups = c('species','polygon','time'), fun = sum)
  biomass.box.invert = dplyr::filter(biomass.box, !(species %in% data.age.mat$species))
  biomass.box.epibenthic = dplyr::filter(biomass.box,species %in% bio.pools)
  
  #Aggregate by ageclass
  biomass.age = dplyr::filter(spatial.biomass, species %in% data.age.mat$species)
  biomass.age = atlantistools::agg_data(biomass.age, groups = c('species','agecl','time'),fun = sum)  
  
  biomass.age.invert = dplyr::filter(spatial.biomass, !(species %in% data.age.mat$species))
  biomass.age.invert = atlantistools::agg_data(biomass.age.invert,groups = c('species','time'),fun = sum)
  
  #Aggregate Numbers (not biomass) 
  numbers = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','time'), fun = sum)
  numbers.age = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','agecl','time'), fun = sum)
  numbers.box = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','polygon','time'), fun = sum)
  
  RN.box = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','polygon','time'), fun = sum)
  SN.box = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','polygon','time'), fun = sum)
  RN.age = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','agecl','time'), fun = sum)
  SN.age = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','agecl','time'), fun = sum)
  
  #Use mean RN+SN per age for each species, convert to weight, get length w/Von Bert.
  RN.SN.age = dplyr::left_join(RN.age,SN.age,by = c('species','agecl','time'))
  colnames(RN.SN.age) = c('species','agecl','time','RN','SN')
  
  biol.prm.lines = read.table(biol.prm,col.name = 1:100, comment.char = '', fill = T, header = F)
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
  
  biomass.age2 = dplyr::left_join(RN.SN.age, tempmat3[,2:4], by = c('species'='LongName'))
  biomass.age2$grams_N_Ind = (biomass.age2$RN+biomass.age2$SN)*5.7*20/1000
  biomass.age2$length_age = (as.numeric(as.character(biomass.age2$grams_N_Ind))/as.numeric(as.character(biomass.age2$li_a)))^(1/as.numeric(as.character(biomass.age2$li_b)))
  length.age = biomass.age2[,c('species','agecl','time','length_age')]
  colnames(length.age)[4] = 'atoutput'
  
  #Aggregate the rest of the dataframes by mean
  SN.age.mean = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','time','agecl'), fun = mean)
  RN.age.mean = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','time','agecl'), fun = mean)
  eat.age = atlantistools::agg_data(data = rawdata.prod[[1]], groups = c('species','time','agecl'),fun = mean)
  grazing = atlantistools::agg_data(data = rawdata.prod[[2]], groups = c('species','time'),fun = mean)
  growth.age =  atlantistools::agg_data(data = rawdata.prod[[3]], groups = c('species','time','agecl'),fun = mean)
  growth.PL = atlantistools::agg_data(data = rawdata.nopred.prod[[1]],groups = c('species','time'),fun = mean )
  growth.PS = atlantistools::agg_data(data = rawdata.nopred.prod[[2]],groups = c('species','time'),fun = mean )
  # growth.PB = atlantistools::agg_data(data = rawdata.nopred.prod[[3]],groups = c('species','time'),fun = mean )
  
  #Calculate consumed biomasss
  safe.diet = purrr::possibly(atlantistools::calculate_consumed_biomass, otherwise = NA)
  bio.consumed = safe.diet(eat = rawdata.prod[[1]], grazing = rawdata.prod[[2]], dm = data.dietcheck, vol = vol, bio_conv = bio.conv)
  if(any(!is.na(bio.consumed))){
    bio.consumed = atlantistools::agg_data(bio.consumed, groups = c('pred','agecl','time','prey'),fun = sum)
  }
  
  #Calculate spatial overlap
  if(spatial.overlap){
    sp.overlap = atlantistools::calculate_spatial_overlap(biomass_spatial = spatial.biomass,dietmatrix = data.diet.mat, agemat = data.age.mat )  
  }else{
    sp.overlap = NA
  }
  
  
  #Growth relative to initial conditions
  recruit.weight = atlantistools::prm_to_df(prm_biol = biol.prm, fgs = groups.file,
                                            group = codes.age,
                                            parameter = c('KWRR','KWSR','AgeClassSize'))
  pd = atlantistools::load_init_weight(init = init.file, fgs = groups.file,bboxes = bboxes)
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
  
  growth.rel.init = dplyr::left_join(growth.age,growth.required)  
  growth.rel.init = dplyr::mutate(growth.rel.init, gr_rel = (atoutput - growth_req) / growth_req)
  
  which.inf = which(growth.rel.init$gr_rel == 'Inf')
  growth.rel.init$gr_rel[which.inf] = 1
  
  #Add catch and total catch
  if(include_catch){
    catch = atlantistools::load_nc(catch.file, fgs = groups.file, bps = bio.pools,
                                   select_groups = group.names,
                                   select_variable = 'Catch',
                                   prm_run = run.prm, bboxes = bboxes)  
    totcatch = atlantistools::agg_data(catch, groups = c('species','time','agecl'), fun = sum)
    catchmt <- atlantisom::load_catch(dir=atl.dir,file_catch=paste0(run.prefix,"Catch.txt"),fgs = groups.data) %>%
      dplyr::select(species,time,atoutput) %>%
      dplyr::mutate(time = time/365)
  }
  
  #Aggregate volume vertically
  vol.ts = atlantistools::agg_data(vol, groups = c('time','polygon'), fun = sum, out = 'volume')
  
  #create output list (conditional on include_catch)
  if(include_catch){
    result = list(
      'biomass' = biomass,
      'biomass.age' = biomass.age,
      'biomass.consumed' = bio.consumed,
      'biomass.spatial.stanza' = spatial.biomass.stanza,
      'biomass.age.invert' = biomass.age.invert,
      'biomass.box.invert' = biomass.box.invert,
      'diet.matrix' = data.diet.mat,
      'dz' = dz,
      'eat_age' = eat.age,
      'flux' = flux,
      'grazing' = grazing,
      'growth.age' = growth.age,
      'growth.rel.init' = growth.rel.init,
      'length.age' = length.age,
      'nominal.dz' = nominal.dz,
      'numbers' = numbers,
      'numbers.age' = numbers.age,
      'numbers.box' = numbers.box,
      'biomass.box' = biomass.box,
      'physics.statevars' = phys.statevars,
      'RN.box' = RN.box,
      'RN.age' = RN.age,
      'SN.box' = SN.box,
      'SN.age' = SN.age,
      'source.sink' = source.sink,
      'spatial.overlap' = spatial.overlap,
      'ssb.recruits' = data.ssb.recruit,
      'volume' = vol.ts,
      'catch' = catch,
      'totcatch' = totcatch,
      'catchmt' = catchmt
      )
  } else{
    result = list(
      'biomass' = biomass,
      'biomass.age' = biomass.age,
      'biomass.consumed' = bio.consumed,
      'biomass.spatial.stanza' = spatial.biomass.stanza,
      'biomass.age.invert' = biomass.age.invert,
      'biomass.box.invert' = biomass.box.invert,
      'diet.matrix' = data.diet.mat,
      'dz' = dz,
      'eat_age' = eat.age,
      'flux' = flux,
      'grazing' = grazing,
      'growth.age' = growth.age,
      'growth.rel.init' = growth.rel.init,
      'length.age' = length.age,
      'nominal.dz' = nominal.dz,
      'numbers' = numbers,
      'numbers.age' = numbers.age,
      'numbers.box' = numbers.box,
      'biomass.box' = biomass.box,
      'physics.statevars' = phys.statevars,
      'RN.box' = RN.box,
      'RN.age' = RN.age,
      'SN.box' = SN.box,
      'SN.age' = SN.age,
      'source.sink' = source.sink,
      'spatial.overlap' = spatial.overlap,
      'ssb.recruits' = data.ssb.recruit,
      'volume' = vol.ts
    )
  }
  
  if(save.out){
    save(result, file = paste0(out.dir,run.prefix,'_postprocessed.rdata'))    
  }else{
    return(result)
  }

  
}

# atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/Post_Processed/'
# param.dir = here::here('CurrentVersion')
# run.prefix = 'neus_output_test'
# 
# source(here::here('R','get_atl_paramfiles.R'))
# param.ls= get_atl_paramfiles(param.dir,atl.dir,include_catch=T)

# process_atl_output( param.dir = here::here('CurrentVersion'),
#   atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/',
#   out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/Post_Processed/',
#   run.prefix = 'neus_output_test',
#   include_catch = T,
#   save.out = T,
#   bgm.file = param.ls$bgm,
#   groups.file = param.ls$func.groups,
#   init.file = param.ls$init.nofill,
#   biol.prm = param.ls$biol.prm,
#   run.prm = param.ls$run.prm,
#   main.nc = param.ls$main.nc,
#   prod.nc = param.ls$prod.nc,
#   dietcheck = param.ls$dietcheck,
#   ssb = param.ls$ssb,
#   yoy = param.ls$yoy,
#   catch.file = param.ls$catch,
#   totcatch.file = param.ls$catchtot,
#   spatial.overlap = F  
# )

  # atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/'
  # out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/Post_Processed/'
  # run.prefix = 'neus_output_test'
  # include_catch = T
  # save.out = T
  # param.dir = here::here('CurrentVersion')
  # bgm.file = param.ls$bgm
  # groups.file = param.ls$func.groups
  # init.file = param.ls$init.nofill
  # biol.prm = param.ls$biol.prm
  # run.prm = param.ls$run.prm
  # main.nc = param.ls$main.nc
  # prod.nc = param.ls$prod.nc
  # dietcheck = param.ls$dietcheck
  # ssb = param.ls$ssb
  # yoy = param.ls$yoy
  # catch.file = param.ls$catch
  # totcatch.file = param.ls$catchtot
  # spatial.overlap = F