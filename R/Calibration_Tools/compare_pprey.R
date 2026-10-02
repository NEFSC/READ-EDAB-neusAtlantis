#' Function that compares pprey values for a specific group between two model runs
#' @model.1.dir = string. Directory for first model parameter files
#' @model.2.dir = string. Directory for second model parameter files
#' @bio.1 string. Name of first biology.prm file
#' @bio.2 string. Name of second model biology.prm file
#' @fgs.file string. Name of functional groups file in first model
#' @model.1.name string. Name of first model
#' @model.2.name string. Name of second model
#' @spp.name string. Functional group code for comparison
#' @is.pred logical. If TRUE shows all prey groups, if FALSE shows all predator groups
#' @rm.same logical. If TRUE, removes pred/prey groups that don't change between models

# model.1.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
# model.2.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Parameter_Files/'
# bio.1 = 'at_biology.prm'
# bio.2 = 'at_biol_neus_v15_scaled_diet_20181126_3.prm'
# fgs.file = 'neus_groups.csv'
# spp.name = '1MEN1'
# spp.name = 'MEN'
# is.pred = F
# rm.same = TRUE
# model.1.name = 'Obs'
# model.2.name = 'Old'

#Utility function to expand possible pPREY combinations for age-structured groups
#AXXXB where A is predator group and B is prey group (e.g. 1MEN2 is juvenile MEN consuming adult prey)
pprey.combs = function(spp.name){
  return( c(paste0(1,spp.name,1),
            paste0(1,spp.name,2),
            paste0(2,spp.name,1),
            paste0(2,spp.name,2)))
}

compare_pprey = function(model.1.dir,model.2.dir,
                         bio.1, bio.2,
                         fgs.file,
                         model.1.name, model.2.name,
                         spp.name, is.pred, rm.same){
  library(dplyr)
  #Load edit_param_pprey functions 
  source(here::here('R','edit_param_pprey.R'))
  
  #Retreive pprey vals for model 1
  model.1.pprey = get_pprey_vals(atl.dir = model.1.dir,
                                 biol.file = paste0(model.1.dir,bio.1),
                                 fgs.file = paste0(model.1.dir,fgs.file),
                                 spp.names = spp.name,
                                 is.pred = is.pred,
                                 remove.zero = F)
  
  #Retreive pprey vals for model 2
  model.2.pprey = get_pprey_vals(atl.dir = model.2.dir,
                                 biol.file = paste0(model.2.dir,bio.2),
                                 fgs.file = paste0(model.1.dir,fgs.file),
                                 spp.names = spp.name,
                                 is.pred = is.pred,
                                 remove.zero = F)
  
  #Reshape pprey vals into long dataframe
  if(is.pred){
    model.1.pprey.long = reshape2::melt(model.1.pprey,id.vars = 'pred',variable.name = 'prey',value.name = 'pprey.1')
    model.1.pprey.long$model = model.1.name
    model.2.pprey.long = reshape2::melt(model.2.pprey,id.vars = 'pred',variable.name = 'prey',value.name = 'pprey.2')
    model.2.pprey.long$model = model.2.name
    
    #Bind dataframes
    models.pprey.df = model.1.pprey.long %>%
      left_join(model.2.pprey.long, by = c('pred','prey')) %>%
      mutate(diff = pprey.1 - pprey.2) %>%
      select(pred,prey,pprey.1,pprey.2,diff)
    
  }else{
    colnames(model.1.pprey)[2] = 'pprey.1'
    colnames(model.2.pprey)[2] = 'pprey.2'
    
    model.1.pprey$prey = spp.name
    model.2.pprey$prey = spp.name
    
    #Bind dataframes
    models.pprey.df = model.1.pprey %>%
      left_join(model.2.pprey, by = c('prey','pred')) %>%
      mutate(diff= pprey.1 - pprey.2) %>%
      select(prey,pred,pprey.1,pprey.2,diff)
  }
  
  if(rm.same){
    models.pprey.df = models.pprey.df %>% filter(diff != 0)
  }
  
  return(models.pprey.df)
}