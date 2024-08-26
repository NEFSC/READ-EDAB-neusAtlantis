edit_param_env_thresholds = function(bio.file,spp.names,threshold.dt,group_locations.dt,new.file.name,overwrite){
  
  con = file(bio.file)
  bio.lines = readLines(con)
  close.connection(con)
  
  env_thresholds.suffixes = c('min_move_temp','max_move_temp','min_move_salt','max_move_salt')
  env_thresholds.df.ls = list()
  numSuffixes <- length(env_thresholds.suffixes)
  for(i in 1:numSuffixes) {
    
    env_thresholds.params = paste0(spp.names, '_', env_thresholds.suffixes[i])
    numParams <- length(env_thresholds.params)

    env_thresholds.lines = sapply(env_thresholds.params,function(x) grep(paste0('^\\b',x,'\\b'),bio.lines),USE.NAMES = F)
    env_thresholds.vals = bio.lines[env_thresholds.lines]
    env_thresholds.df.ls[[i]] = data.frame(spp = spp.names,
                                  param = env_thresholds.suffixes[i],
                                  value = sapply(env_thresholds.vals,function(x) strsplit(x,' |\t')[[1]][2],USE.NAMES = F),
                                  stringsAsFactors = F)
  }
  env_params.df = dplyr::bind_rows(env_thresholds.df.ls)
  
  numEnvParams <- nrow(env_params.df)
  for (n in 1:numEnvParams) {
    groupCode <- env_params.df$spp[n]
    param <- env_params.df$param[n]
    env_record <- filter(threshold.dt, Code == groupCode)
    location_record <-filter(group_locations.dt,Code == groupCode)
    if (nrow(env_record > 0)) {
      if (param == 'min_move_temp') {
        if (location_record$Location == 'Surface') {
          newValue <- env_record$min_surface_temp
        } else {
          newValue <- env_record$min_bottom_temp
        }
      } else if (param == 'max_move_temp') {
        if (location_record$Location == 'Surface') {
          newValue <- env_record$max_surface_temp
        } else {
          newValue <- env_record$max_bottom_temp
        }
      } else if (param == 'min_move_salt') {
        if (location_record$Location == 'Surface') {
          newValue <- env_record$min_surface_salt
        } else {
          newValue <- env_record$min_bottom_salt
        }
      } else if (param == 'max_move_salt') {
        if (location_record$Location == 'Surface') {
          newValue <- env_record$max_surface_salt
        } else {
          newValue <- env_record$max_bottom_salt
        }
      } 
      param.str <- paste0(groupCode, '_', param)
      line.match = grep(param.str,bio.lines)
      new.line <- paste0(param.str, ' ', newValue)
      lm <- length(line.match)
      bio.lines[line.match[lm]] = new.line
    }
  }
  if(overwrite){
    writeLines(bio.lines, con = bio.file)
  }else{
    file.copy(bio.file, new.file.name, overwrite = T)
    writeLines(bio.lines, con = new.file.name )
  }
}

# Sample code
#bio.file  = here::here('currentVersion','at_biology.prm')
#fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is = T)
#threshold.dt <- read.csv(here::here('data','atlantis_group_thresholds_NRHA.csv'))
#group_locations.dt <- read.csv(here::here('data','group_locations.csv'))
#new.file.name <- here::here('currentVersion','at_biology_env_thresholds.prm')
#edit_param_env_thresholds(bio.file,fgs$Code[1:86],threshold.dt,group_locations.dt,new.file.name,overwrite=F)
