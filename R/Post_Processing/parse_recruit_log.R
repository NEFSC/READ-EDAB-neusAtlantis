#Function to parse log file

# run.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/BH_NEUSv1_Spawn_Debug/"

parse_recruit_log = function(run.dir,spp= NULL){
  
  log.files = list.files(run.dir,'^log*',full.names = T)
  dat.df.ls = list()
  
  for(i in 1:length(log.files)){
    log.lines = readLines(log.files[i])
    
    recruit.lines = grep("BulkRecruits",log.lines)
    dat.df = read.table(text = gsub(':| ',',',log.lines[recruit.lines]),sep = ',')[,c(3,6,9,12,16,21,25,29,33,36)]
    colnames(dat.df) = c('time','box','layer','code','ngene','recruit.case','recruits','temp.recruits','spawn','qid')
    
    dat.df %>%filter(code == 'MAK' & box == 1)
    
    #Old Version
    
    #recruit.lines = grep('recSTOCK',log.lines)
    # dat.df = read.table(text = gsub(':| ',',',log.lines[recruit.lines]),sep = ',')[,c(3,5,8,12,16,20,24,28,32,36,40,44)]
    # colnames(dat.df) = c('Time','Location','Code','ngene','stock','temprec','recSTOCK','BHalpha','Larvae','BHbeta','totfish','stock_prop')
    # dat.df$box = sapply(as.character(dat.df$Location),function(x) return(strsplit(x,'box|-')[[1]][2]),USE.NAMES = F)
    # dat.df$layer = sapply(as.character(dat.df$Location),function(x) return(strsplit(x,'box|-')[[1]][3]),USE.NAMES = F)
    
    if(!is.null(spp)){
      dat.df= subset(dat.df,code %in% spp)
    }
    dat.df.ls[[i]] = dat.df
  }
  return(dplyr::bind_rows(dat.df.ls))
}