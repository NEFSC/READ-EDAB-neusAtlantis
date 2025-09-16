#'Script to generate revenue of catch using price reference table
#'
#'@param run.name character. name of run
#'@param run.dir  character. directory of atlantis runs
#'@param price.reference dataframe. Code x Year x Price used for calculation
#'@param plot logical. whether to create plots
#'@param out.dir character. path to write output
#'@param write logical. whether to write out (F = only return)



make_catch_revenue = function(run.name, run.dir, price.reference, start.year = 1964, plot = F, write = F, out.dir = NA){
  
  #get catch by fishery
  catch = read.table(paste0(run.dir, 'neus_outputCatchPerFishery.txt'),header =T) %>% 
    tidyr::gather(Code, Catch, -Time, -Fishery) %>% 
    dplyr::mutate(Year = floor(Time/365)+ start.year)
  
  catch.price = catch %>%
    dplyr::left_join(price.reference) %>% 
    dplyr::filter(!is.na(Price) & Catch > 0) %>% 
    dplyr::mutate(Revenue = Price * Catch * 2204.62)
  
  revenue.fleet = catch.price %>% 
    dplyr::group_by(Fishery, Year) %>% 
    dplyr::summarise(Revenue = sum(Revenue,na.rm=T))
  
  if(plot){
    
    ggplot2::ggplot(data = revenue.fleet, ggplot2::aes(x = Year, y = Revenue))+
      ggplot2::geom_line()+
      ggplot2::facet_wrap(~Fishery, scale = 'free_y')
  }
  

  if(write){
    if(!dir.exists(out.dir)){
      dir.create(out.dir)
    }
    
    saveRDS(revenue.fleet, paste0(out.dir, run.name, '_fleet_revenue.rds'))  
  }else{
    return(revenue.fleet)  
  }
    
}