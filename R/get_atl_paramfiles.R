#' Creates a dataframe of atlantis parameter files based on standard outut
#' 
#' @param.dir string. Path to location of atlantis parameter files
#' @atl.dir string. Path to location of atlantis output files
#' @include_catch logical. Whether to include catch output
#' 
#' @return list containing parameter file name and full path to file
#' 

get_atl_paramfiles = function(param.dir,atl.dir,include_catch){
  
  #Create vector of netCDF output files
  output.files = list.files(path = atl.dir, pattern = '*.nc')
  
  #Identify file names and their character length
  nc.str = strsplit(output.files, '.nc')
  lng.str = nchar(nc.str)
  
  #Generate paths to main, PROD, DietCheck, YOY, and SSB output files
  main.nc = paste0(atl.dir,run.prefix,'.nc')
  prod.nc = paste0(atl.dir,run.prefix,'PROD.nc')
  dietcheck = paste0(atl.dir,run.prefix,'DietCheck.txt')
  yoy = paste0(atl.dir,run.prefix,'YOY.txt')
  ssb = paste0(atl.dir,run.prefix,'SSB.txt')
  mort <- paste0(atl.dir,run.prefix,'Mort.txt')
  specificmort <- paste0(atl.dir,run.prefix,'SpecificMort.txt')
  
  #If catch is turned on all generate paths for CATCH and TOTCATCH
  if(include_catch){
    catch = paste0(atl.dir, run.prefix,'CATCH.nc')
    catchtot = paste0(atl.dir, run.prefix,'TOTCATCH.nc')
  }
  
  #Identify run_command file to get additional
  run.bat = list.files(param.dir,'*.bat')
  run.con = file(file.path(param.dir,run.bat),'r')
  run.cmd = readLines(run.con,n = 1,warn = F)
  close(run.con)
  
  #Generate paths of param files from run command 
  run.filename = function(run.cmd,code){
    return(strsplit(strsplit(run.cmd,paste0(code,' '))[[1]][2],' ')[[1]][1])
  }
  
  run.prm = file.path(param.dir,run.filename(run.cmd, '-r'))
  biol.prm = file.path(param.dir,run.filename(run.cmd, '-b'))
  force.prm = file.path(param.dir,run.filename(run.cmd, '-f'))
  phys.prm = file.path(param.dir,run.filename(run.cmd, '-p'))
  harvest.prm = file.path(param.dir,run.filename(run.cmd, '-h'))
  economics.prm = file.path(param.dir,run.filename(run.cmd, '-e'))
  fishery.prm = file.path(param.dir,run.filename(run.cmd, '-q'))
  func.groups = file.path(param.dir,run.filename(run.cmd, '-s'))
  init = file.path(param.dir,run.filename(run.cmd, '-i'))
  
  
  #Get bgm file name from initial conditions file
  init.nc = ncdf4::nc_open(init)
  bgm.str = ncdf4::ncatt_get(init.nc,0,'geometry')$value
  ncdf4::nc_close(init.nc)
  bgm = file.path(param.dir,bgm.str)
  
  #Get nofill version of initial conditions
  init.nofill = list.files(param.dir,'*nofill*',full.name = T)
  
  #Organize parameter files
  param.list = list(run.cmd = run.cmd,
                    run.prm = run.prm,
                    biol.prm = biol.prm,
                    force.prm = force.prm,
                    phys.prm = phys.prm,
                    harvest.prm = harvest.prm,
                    economics.prm = economics.prm,
                    fishery.prm = fishery.prm,
                    groups.file = func.groups,
                    init.file = init,
                    bgm.file = bgm,
                    init.nofill = init.nofill,
                    main.nc = main.nc,
                    prod.nc = prod.nc,
                    dietcheck = dietcheck,
                    yoy =yoy,
                    ssb = ssb,
                    catch = ifelse(include_catch,catch,NA),
                    catchtot = ifelse(include_catch,catchtot,NA),
                    mort = mort,
                    specificmort = specificmort
                    )
  
  return(param.list)
}
