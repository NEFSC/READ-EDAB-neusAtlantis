#' @description Takes annual .csv output from satellite primary production model output, converts it to biomass units (mgN/m-3)
#' and transforms it to atlantis-structured array. Then array is converted to netCDF output with appropriate 
#'  attritbutes.
#'  
#'  @in.dir string. Input file directory
#'  @in.prefix string. Input file prefix (all read in)
#'  @in.file string. Input file name
#'  @out.dir string. Output directory
#'  @out.prefix string. Output annual file prefix
#'  @stat.var string. Name of summary statitic column used for aggregation
#'  @bio.vars Character vector of primary producer variable names
#'  @atl.varname Character vector length atl.groups with full Atlantis name (for netCDF variables). Should match initial conditions file
#'  @atl.longname Character vector length atl.groups with full descriptive name for each atlantis variable.
#'  @atl.units character vector of units from initial conditions file
#'  @phtyo.fract matrix (box x time) that contains the Diatom:Dinoflagellate ratio for the microphytoplankton
#'  @chl.conv numeric vector same length as bio.vars that has the Cholorphyll to biomass conversion factor
#'  
#'Author: J. Caracappa
#'

# in.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Data/'
# in.prefix = 'D8-OCCCI-ATLANTIS_*'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/'
# out.prefix = 'Phyto_Forcing_'
# # out.file = 'Phyto_2000'
# stat.var = 'MED'
# bio.vars = c('MICRO','NANO','PICO')
# atl.varname = c('Diatom_N','DinoFlag_N','PicoPhytopl_N','Diatom_S')
# atl.longname = c('Diatom Nitrogen','Dinoflagellate Nitrogen','PicoPhytoplankton Nitrogen','Diatom Silicon')
# atl.units = c(rep('mg N m-3',3),'mg Si m-3')
# # phyto.fract = data.frame(PL = c(0.5,0,0),DF = c(0.5,0.5,0), PS = c(0,0.5,1))
# phyto.fract = matrix(0.75,nrow = 30, ncol = 366)
# phyto.fract.ls = lapply(1,function(x) return(phyto.fract))
# chl.conv = rep(7,3)


make_SatPhyto_files = function(in.dir,
                                     in.prefix,
                                     out.dir,
                                     out.prefix,
                                     stat.var,
                                     bio.vars,
                                     atl.varname,
                                     atl.longname,
                                     atl.units,
                                    dynamic.mid,
                                    dynamic.bot,
                                     phyto.fract.ls,
                                     chl.conv){
  
  
  source(here::here('R','Satellite_Phytoplankton','fill_satphyto_gaps.R'))
  
  `%>%` = dplyr::`%>%`
  
  #Define input files
  in.files = list.files(in.dir,in.prefix)
  
  years = as.numeric(unname(sapply(in.files,function(x) return(strsplit(x,"D8-OCCCI-ATLANTIS_|.csv")[[1]][2]))))
  
  #Read in data from CSV and put int one data.frame
  year.df.ls = list()
  for(i in 1:length(in.files)){
  # for(i in 1:2){
    #Read in data
    data = read.csv(paste0(in.dir,in.files[i]),header = T,as.is = T)  
    
    data$mid = as.Date(data$mid)
    
    #Remove unused columns
    data = data %>% 
      dplyr::select(PROD,PERIOD,mid,mid.year,UNITS,SUBAREA,N_SUBAREA,all_of(stat.var)) %>% 
      dplyr::rename( 'STAT' =all_of(stat.var))
    
    #Format all dates
    ref.year = unique(data$mid.year)
    ref.year.dates = seq.Date(as.Date(paste0(years[i],'-01-01')),as.Date(paste0(years[i],'-12-31')),by = 1)
    
    #Format Data into array
    prod.ls = list()
    boxes = 0:29
    
    v=1
    for(v in 1:length(bio.vars)){
      
      var.mat = matrix(NA,nrow = 30, ncol = length(ref.year.dates))
      
      #Loop over boxes and put int array (by day)
      for(b in 1:length(boxes)){
        var.box = data %>% 
          dplyr::filter(PROD == bio.vars[v] & SUBAREA == boxes[b]) %>%
          dplyr::arrange(mid)
        date.match = which(ref.year.dates %in% var.box$mid)
        var.mat[b,date.match] = var.box$STAT
      }
      
      #Convert Chl to Nitrogen
      var.mat = var.mat / chl.conv[v]
      
      #Put into Prod.ls
      prod.ls[[v]] = var.mat
    }
    
    #Assign Producer groups to Atlantis groups using phyto.fract
    atl.var.ls = lapply(1:4,function(x) matrix(NA,nrow = 30,ncol = length(ref.year.dates)))
    names(atl.var.ls) = atl.varname
    
    #Extract appropriate fractions and assign Atlantis variables
    for(d in 1:length(ref.year.dates)){
      #Diatom calculation
      atl.var.ls[[1]][,d] = prod.ls[[1]][,d]*phyto.fract.ls[[i]][,d]
      
      #Dinoflagellate Calculation
      atl.var.ls[[2]][,d] = prod.ls[[1]][,d]*(1-phyto.fract.ls[[i]][,d])
      
      #Small Phytoplankton Calculation
      atl.var.ls[[3]][,d] = prod.ls[[2]][,d]+prod.ls[[3]][,d]
  
    }
    
    #Diatom_S Using Si:N = 1.1
    # From Brzezinski 1985
    atl.var.ls[[4]] = atl.var.ls[[1]] * 1.1
    
    data.df.ls = list()
    #Format into data.frame
    for(v in 1:length(atl.var.ls)){
      dat = atl.var.ls[[v]]
      DF = data.frame(date = rep(ref.year.dates,each = 30),
                      doy = rep(1:length(ref.year.dates),each = 30),
                      ref.year = ref.year,
                      box = rep(0:29,length(ref.year.dates)),
                      variable = names(atl.var.ls)[v],
                      values = c(atl.var.ls[[v]]),
                      stringsAsFactors = F)
      data.df.ls[[v]] = DF
      # print(nrow(DF))
    }
    #add into yearly data list
    year.df.ls[[i]] = dplyr::bind_rows(data.df.ls)
    
  }
  
  #combine years into single dataframe
  year.df = dplyr::bind_rows(year.df.ls)
  rm(year.df.ls,data.df.ls)
  
  #Split Data back into list split by variable
  year.var.ls = list()
  for(v in 1:length(atl.varname)){
    year.var.ls[[v]] = year.df %>%
      dplyr::filter(variable == atl.varname[v]) %>%
      dplyr::arrange(date,box)
  }
  
  
  #Put full data through gap filling routine
  
  for(i in 1:length(atl.var.ls)){
    if(names(atl.var.ls)[i] == 'Diatom_S'){
      var.name = 'Diatom_N'
    }else{
      var.name = atl.varname[i]
    }
    atl.var.ls[[i]] = fill_satphyto_gaps(input.mat = year.var.ls[[i]],
                       var.name = var.name,
                       doy.file = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/Phyto_Climatology.nc',
                       max.interp = 100,
                       write.gaps = T,
                       gaps.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Diagnostics/Gap_Analysis/'
                       )
  }
  
  #  y = atl.var.ls[[3]] %>% filter(box == 1)
  # plot(values~date,y,type='l')
  
  # save(atl.var.ls,file= paste0(out.dir,'var_test.R'))
  #Format as netCDF
  
  #Read in dz.csv file
  dz = read.csv(here::here('Geometry','dz.csv'))
  box.lev = apply(dz[,2:5],1,function(x) return(sum(!is.na(x))))
  #Dimension values
  levels = 1:5
  
  for(y in 1:length(years)){
    
    ref.year.dates = seq.Date(as.Date(paste0(years[y],'-01-01')),as.Date(paste0(years[y],'-12-31')),by = 1)
    
    
    #Format each year as a df
    atl.year.ls = list()
    for(v in 1:length(atl.var.ls)){
      dat = atl.var.ls[[v]] %>%
        dplyr::filter(ref.year == years[y]) %>%
        dplyr::arrange(date,box)
      dat2 = reshape2::dcast(dat, box ~date) %>% dplyr::select(-box)
      if(dynamic.mid){
        dat.array = array(NA,dim = c(5,30,length(ref.year.dates)))  
      }else{
        dat.array = array(0,dim = c(5,30,length(ref.year.dates)))
      }
      if(dynamic.bot){
        dat.array[5,,] = NA  
      }else{
        dat.array[5,,] = 0
      }
      
      for(b in 1:length(boxes)){
        dat.array[box.lev[b],b,] = as.numeric(dat2[b,])
      }
      atl.year.ls[[v]] = dat.array
      names(atl.year.ls)[v] = atl.varname[v]
    }
    
    t_tot = as.numeric(difftime(as.POSIXct(ref.year.dates,tz='UTC'),as.POSIXct('1964-01-01 00:00:00',tz='UTC'),units = 'secs'))
    
    #Test t_tot dates
    # as.POSIXct(t_tot,origin = '1964-01-01 00:00:00',tz = 'UTC')
    
    #call new netCDF file
    filename = paste0(out.dir,out.prefix,years[y],'.nc')
    
    nc.file = RNetCDF::create.nc(filename)
    
    RNetCDF::dim.def.nc(nc.file, "t", unlim=TRUE)
    RNetCDF::dim.def.nc(nc.file, "b", 30)
    RNetCDF::dim.def.nc(nc.file, "z", 5)
    
    RNetCDF::var.def.nc(nc.file, "t", "NC_DOUBLE", "t")
    for(v in 1:length(atl.year.ls)){
      var.name = atl.varname[v]
      #Define Variables
      RNetCDF::var.def.nc(nc.file, atl.varname[v], 'NC_DOUBLE', c('z','b','t'))
      #Assign Fill Value
      RNetCDF::att.put.nc(nc.file, atl.varname[v], '_FillValue', "NC_DOUBLE", -999)
      #Assign 
      RNetCDF::att.put.nc(nc.file, atl.varname[v], 'missing_value', 'NC_DOUBLE',-999)
      #Assign valid_min
      RNetCDF::att.put.nc(nc.file, atl.varname[v], 'valid_min', 'NC_DOUBLE', -999)
      #Assing valid_max
      RNetCDF::att.put.nc(nc.file, atl.varname[v], 'valid_max', 'NC_DOUBLE', 99999)
      #Assign units
      RNetCDF::att.put.nc(nc.file, atl.varname[v], 'units','NC_CHAR', atl.units[v])  
      #Assign long_name
      RNetCDF::att.put.nc(nc.file,atl.varname[v],'long_name','NC_CHAR',atl.longname[v])
      
      #Put variable values
      RNetCDF::var.put.nc(nc.file,atl.varname[v],atl.year.ls[[v]])
    }
    
    RNetCDF::att.put.nc(nc.file, "t", "units", "NC_CHAR", 'seconds since 1964-01-01 00:00:00 UTC')
    RNetCDF::att.put.nc(nc.file, "t", "dt", "NC_DOUBLE", 86400)
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "title", "NC_CHAR", 'NEUS_Atlantis_Obs_Hindcast')
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "geometry", "NC_CHAR", 'neus_tmerc_RM2.bgm')
    RNetCDF::att.put.nc(nc.file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
    
    RNetCDF::var.put.nc(nc.file, "t", t_tot)
    
    
    RNetCDF::close.nc(nc.file)
    
    print(years[y])
  }
  
}
