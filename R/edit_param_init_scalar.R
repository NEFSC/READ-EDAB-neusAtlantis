#' Functions to:
#' 1) Retrieve init_scalar params
#' 2) Retrieve total initial condition biomass (tonnes)
#' 3) edit init_scalar params
#' 4) Calculate_init_scalar

# get_param_init_scalar ---------------------------------------------------

get_param_init_scalar = function(run.prm,groups.file,write.output = F, output.dir){
  
  #Read in data
  run.lines = readLines(run.prm)
  fgs = read.csv(groups.file,header = T, stringsAsFactors = F)
  
  #Get group names (order is important)
  group.code = fgs$Code
  
  #get init_scalar lines
  init.scalar.string = run.lines[grep('init_scalar',run.lines)+1]
  init.scalar.vals =strsplit(init.scalar.string,'\t')[[1]]
  
  #output dataframe
  out.df = data.frame(group = group.code, init.scalar = init.scalar.vals)
  
  if(write.output==F){
    return(out.df)  
  }else{
    write.csv(out.df, paste0(output.dir,'init_scalar.csv'),row.names = F)
  }
}


# get_init_cond_biomass ---------------------------------------------------

get_init_biomass = function(bio.age.file, groups.file,write.output =F, output.dir){
  library(dplyr)
  
  bio.age = read.table(bio.age.file,header = T,stringsAsFactors = F)
  
  bio.age.init = dplyr::filter(bio.age,Time == 0)
  bio.age.colnames = colnames(bio.age.init)[-1]
  bio.age.df = data.frame(
    code = sapply(bio.age.colnames,function(x) strsplit(x,'\\.')[[1]][1]),
    age = sapply(bio.age.colnames,function(x) strsplit(x,'\\.')[[1]][2]),
    biomass.tonnes = c(t(unname(bio.age.init[1,-1]))),
    row.names =NULL
  ) %>%
    group_by(code) %>%
    summarize(biomass.tonnes = sum(biomass.tonnes,na.rm=T))
  
  if(write.output){
    write.csv(bio.age.df,paste0(output.dir,'initial_biomass_tonnes.csv'),row.names = F)
  }else{
    return(bio.age.df)
  }
}


# edit_param_init_scalar --------------------------------------------------

edit_param_init_scalar = function(run.prm,groups.file,new.init.scalar,overwrite = F, new.file.name){
  
  library(dplyr)
  #Read in groups file and run.prm
  fgs = read.csv(groups.file,stringsAsFactors = F)
  run.prm.lines = readLines(run.prm)
  
  #Read in init_scalar .csv (edited)
  # new.init.scalar = read.csv(init.scalar.file,stringsAsFactors = F)
  new.init.scalar$init.scalar = sapply(new.init.scalar$init.scalar,function(x){
    if(x<1){
      return(signif(x,3))
    }else{
      return(round(x,3))
    }
  })
  new.init.scalar$init.scalar = as.character(new.init.scalar$init.scalar)
  
  
  #Sort by Group Code Index and join with new data
  init.scalar.edit = fgs %>% 
    select(Code, Index) %>%
    arrange(Index) %>%
    left_join(new.init.scalar, by = c('Code'= 'group'))
  
  #overwrite line
  run.prm.lines[grep('init_scalar',run.prm.lines)+1] = paste(init.scalar.edit$init.scalar,collapse ='\t')
  
  #overwrite or make copy of run file
  if(overwrite){
    writeLines(run.prm.lines, con = run.prm)
  }else{
    file.copy(run.prm, new.file.name, overwrite = T)
    writeLines(run.prm.lines, con = new.file.name )
  }
}


# Calculate_init_scalar ---------------------------------------------------

#Use survdat swept area to calculate initial biomass and init_scalar

calculate_init_scalar = function(run.prm, init.file, groups.file, bio.age.file,exclude.invert = T, missing.cohorts, write.output, output.dir){
  library(dplyr)
    #Read in functional groups
  fgs = read.csv(groups.file,stringsAsFactors = F)
  groups.inverts = fgs$Code[which(fgs$NumCohorts != 10)]
  fgs.fullname = select(fgs, Code, Name, LongName)
  
  #Read in swept area data
  survdat =readRDS(here::here('data-raw','sweptAreaBiomass.RDS'))
  survdat$YEAR = as.numeric(survdat$YEAR)
  
  #Read in initial conditions file
  init.nc = ncdf4::nc_open(init.file)
  init.varnames = names(init.nc$var)
  
  #Read in inital biomass from output file
  init.biomass = get_init_biomass(bio.age.file,groups.file,write.output = F)
  colnames(init.biomass) = c('Code','init.biomass.tonnes')
  
  #Filter to first 5 years of each group's data
  survdat.agg=survdat %>%
    group_by(Code, YEAR) %>%
    summarize(tot.biomass = sum(tot.biomass,na.rm=T),
              tot.bio.SE = mean(tot.bio.SE,na.rm=T),
              tot.abundance = sum(tot.abundance,na.rm=T),
              tot.abund.SE = mean(tot.abund.SE,na.rm=T)) %>%
    group_by(Code) %>%
    mutate(year.id = YEAR-YEAR[1]) %>%
    # mutate(year.id = YEAR-1964)
    filter(year.id <= 10) %>%
    summarize(tot.biomass = mean(tot.biomass,na.rm=T),
              tot.bio.SE = mean(tot.bio.SE,na.rm=T),
              tot.abundance = mean(tot.abundance,na.rm=T),
              tot.abund.SE = mean(tot.abund.SE,na.rm=T)) %>%
    filter(!Code %in% groups.inverts) %>%
    left_join(missing.cohorts) %>%
    left_join(fgs.fullname)  %>%
    left_join(init.biomass) %>% 
    select(Code, Name, LongName,missing_cohorts,tot.biomass,tot.bio.SE, tot.abundance, tot.abund.SE, init.biomass.tonnes)
  
  #Get numbers at age, total numbers, and fraction for cohorts missing from survdat from init.nc
  survdat.agg$init.numbers = NA
  survdat.agg$group.scale.fract = NA
  for( i in 1:nrow(survdat.agg)){
    
    #Find matching init.nc varnames for each group in survdat.agg
    var.match = init.varnames[grep(paste0(survdat.agg$Name[i],'.*_Nums'), init.varnames)]
    
    #Loop through all cohorts and retreive domain-wide numbers in init.nc
    group.nums.df = data.frame(varname = var.match, Name = survdat.agg$Name[i], cohort= NA, nums = NA)
    for(j in 1:length(var.match)){
      group.nums.df$cohort[j] = as.numeric(strsplit(var.match[j],paste0(survdat.agg$Name[i],'|_Nums'))[[1]][2])
      group.nums.df$nums[j] = sum(ncdf4::ncvar_get(init.nc,var.match[j]),na.rm=T)
    }
    group.nums.tot = sum(group.nums.df$nums)
    
    
    group.nums.df = group.nums.df %>%
      mutate(num.tot = group.nums.tot,
             cohort.prop = nums/num.tot) %>%
      arrange(cohort) %>% 
      filter(cohort <= survdat.agg$missing_cohorts[i])
    
    survdat.agg$init.numbers[i] = group.nums.tot
    survdat.agg$group.scale.fract[i] = 1-sum(group.nums.df$cohort.prop)
    
    }

  survdat.agg$tot.abundance.adj = survdat.agg$tot.abundance/survdat.agg$group.scale.fract 
  survdat.agg$new.init.scalar = survdat.agg$tot.abundance.adj/survdat.agg$init.numbers

  if(write.output){
    write.csv(survdat.agg, file = paste0(output.dir,'survdat_initial_conditions.csv'),row.names = F)
  }else{
    return(survdat.agg)
  }
  
}

# Example -----------------------------------------------------------------
# 
# run.prm = here::here('currentVersion','at_run.prm')
# init.file = here::here('currentVersion','neus_init.nc')
# init.file.nofill = here::here('currentVersion','neus_init_nofill.nc')
# groups.file = here::here('currentVersion','neus_groups.csv')
# bgm.file = here::here('currentVersion','neus_tmerc_RM2.bgm')
# # bio.age.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_10202020/neus_outputAgeBiomIndx.txt'
# bio.age.file = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Master_NoInitScalar/neus_outputAgeBiomIndx.txt'
# init.scalar.file ='C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/init_scalar_new.csv'
# output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/'
# missing.cohorts = read.csv(paste0(output.dir,'missing_survdat_cohorts.csv'),stringsAsFactors = F)
# 
# get_param_init_scalar(run.prm,groups.file,write.output = T,
#                       output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/')
# get_init_biomass(bio.age.file,groups.file,write.output = T,
#                  output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/')
# edit_param_init_scalar(run.prm,groups.file,
#                        new.init.scalar = read.csv(init.scalar.file,stringsAsFactors = F),
#                        overwrite = F,
#                        new.file.name = here::here('currentVersion','at_run_test.prm'))
# calculate_init_scalar(run.prm = run.prm,
#                       groups.file = groups.file,
#                       bio.age.file = bio.age.file,
#                       init.file = init.file,
#                       missing.cohorts = missing.cohorts,
#                       exclude.invert = T,
#                       write.output = T,
#                       output.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/')
# 
# edit_param_init_scalar(run.prm,groups.file,
#                        new.init.scalar = read.csv(init.scalar.file,stringsAsFactors = F),
#                        overwrite = T)
