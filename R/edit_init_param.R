#' Function to edit initial conditions .nc attributes
#' 
#' @init.file string. Filename for initial conditions
#' @var.name string. Variable name whose attribute you want to change
#' @att.name string. Attribute name you want to change
#' @new.val string. New attribute value
#' @overwrite logical. If TRUE overwrite, if FALSE save as new
#' @new.file.name string. New file name if saving new
#' 

# init.file = here::here('currentVersion','neus_init.nc')
# var.name = 'Diatom_N'
# att.name = 'svel'
# new.val = 4.629E-5
# overwrite = F
# new.file.name =paste0(here::here('currentVersion'),'/neus_init_test.nc')

edit_init_param = function(init.file,var.name,print.param,att.name,new.val,overwrite=F,new.file.name){
  
  if(overwrite | print.param){
    init.nc = ncdf4::nc_open(init.file,write = T)
    if(print.param){
      return(ncdf4::ncatt_get(init.nc,var.name,att.name))
    }
  }else{
    file.copy(init.file,new.file.name)
    init.nc = ncdf4::nc_open(new.file.name,write = T)
  }
  
  
 
  ncdf4::ncatt_put(init.nc,var.name,att.name,new.val)
  
  ncdf4::nc_close(init.nc)  
}

edit_init_param(
  # init.file = here::here('currentVersion','neus_init.nc'),
  init.file = 'C:/Users/joseph.caracappa/Downloads/nordic_biol_v23.nc',
  var.name = 'Large_phytop_N',
  att.name = 'svel',
  print.param = T
)

edit_init_param(
  init.file = here::here('currentVersion','neus_init.nc'),
  var.name = 'DinoFlag_N',
  att.name = 'svel',
  new.val = -2.89E-5,
  overwrite = T,
  print.param = F,
  new.file.name = here::here('currentVersion','neus_init_FastSink.nc')
)

edit_init_param(
  init.file = here::here('currentVersion','neus_init.nc'),
  var.name = 'Diatom_S',
  att.name = 'svel',
  new.val = -5.78E-6,
  overwrite = T,
  print.param = F,
  new.file.name = here::here('currentVersion','neus_init_FastSink.nc')
)
