#' Generates timeseries plot comparing geochemical variables of 2 or more models
#' 
#' Used for diagnostic comparisons between model runs. While does not work
#' exclusively for geochemical variables, other functions exist that are 
#' more tailored specifically for those comparisons
#' 
#' @output.files character vector. character vector of full path to main output.nc file of each model
#' @model.names chacter vector. Vector descibing names of each model (used for plotting)
#' @var.name string. Name of the variable used for plotting
#' @plot.dir string. path to save resulting figure
#' @plot.name string. Name to save plot as
#' 
#' @return timeseries plot for each model
#' 
#' Author: J.Caracappa
#' 

# output.files = c(
#   'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/atneus_v15_01272020/atneus_v15_01272020.nc',
#   'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_LTLForce_1980Fill/neus_output_test.nc'
# )
# model.names = c('original','LTL_Forced')
# var.name = 'NO3'
# plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Diagnostic_Figures/Run_Comparisons/'
# plot.name = 'NO3_original_v_LTL_Forced'

plot.geochem.comps = function(output.files,model.names,var.name,plot.dir,plot.name){
  
  dumm.nc = ncdf4::nc_open(output.files[1])
  var.units = ncdf4::ncatt_get(dumm.nc,var.name)$units
  var.longname = ncdf4::ncatt_get(dumm.nc,var.name)$long_name
  ncdf4::nc_close(dumm.nc);rm(dumm.nc)
  
  if(length(model.names) != length(output.files)){
    stop('Length of model names and output files must match')
  }
  source(here::here('R','atl_var_to_longform.R'))
  `%>%` = dplyr::`%>%`
  
  var.dat.ls = list()
  for(f in 1:length(output.files)){
    var.dat.ls[[f]] = atl_var_to_longform(output.files[f],var.name = var.name,origin = '1964-01-01')
    var.dat.ls[[f]]$model = model.names[f]
    var.dat.ls[[f]]$model.id = f
  }
  var.dat.all = dplyr::bind_rows(var.dat.ls)
  rm(var.dat.ls)
  
  var.dat.summ = var.dat.all %>%
    dplyr::group_by(model,model.id,time,time.date,level) %>%
    dplyr::summarize(value.mu = mean(value))
  rm(var.dat.all)
  
  ggplot2::ggplot(var.dat.summ,
                  ggplot2::aes(x=time.date,y=value.mu,color = as.factor(model.id)))+
    ggplot2::geom_line()+
    ggplot2::facet_wrap(~level, nrow = 5,labeller = ggplot2::label_both)+
    ggplot2::xlab('')+
    ggplot2::ylab(paste0(var.longname,'( ',var.units,' )'))+
    ggplot2::scale_color_manual(name = 'Model',labels = model.names, values = RColorBrewer::brewer.pal(length(model.names),'Set1'))+
    ggplot2::ggsave(paste0(plot.dir,plot.name,'.png'),width = 14, height = 8, units = 'in', dpi = 300)
}