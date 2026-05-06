library(dplyr)
source(here::here('R','Calibration_Tools','edit_param_pprey.R'))

# new.pprey = read.csv('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Diet/DOG_cannibal.csv',as.is = T)

atl.dir  = here::here('currentVersion','/')

# diet.change = read.csv('C:/Users/joe92/Documents/Atlantis/Diet Changes/Diet_Adjust_07082022.csv')
fgs = read.csv(here::here('currentVersion','neus_groups.csv'))

out.df = data.frame(Code= fgs$Code, nprey = NA, npred = NA, tot.conn = NA)

for(i in 1:nrow(fgs)){
  
  prey.vals = get_pprey_vals(atl.dir = atl.dir,
                             biol.file = here::here('currentVersion','at_biology.prm'),
                             fgs.file = here::here('currentVersion','neus_groups.csv'),
                             spp.names = out.df$Code[i],
                             is.pred = F,
                             remove.zero = T)
  
  if(fgs$NumCohorts[which(fgs$Code == out.df$Code[i])]>1){
    pred.name = paste0(c(1,1,2,2),out.df$Code[i],c(1,2,1,2))
  }else{
    pred.name = out.df$Code[i]
  }
  
  out.df$npred[i] = gsub("\\d", "", prey.vals$pred) |> unique() |> length()
  
  out.df$nprey[i] <- tryCatch({
    
    # 1. Attempt to run your function
    pred.vals <- get_pprey_vals(
      atl.dir = atl.dir,
      biol.file = here::here('currentVersion','at_biology.prm'),
      fgs.file = here::here('currentVersion','neus_groups.csv'),
      spp.names = pred.name,
      is.pred = T,
      remove.zero = T
    )
    
    # 2. If successful, this evaluates and is passed to out.df$nprey[i]
    length(colSums(pred.vals[, -1]))
    
  }, error = function(e) {
    
    # 3. If an error is caught, return 0 instead
    0
    
  })
  
  out.df$tot.conn[i] = out.df$npred[i] + out.df$nprey[i]
  
}

plot.df = out.df |> 
  arrange(desc(tot.conn))
