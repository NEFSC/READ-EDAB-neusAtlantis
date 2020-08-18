#' Function that fills in temporal gaps in box-level satellite phytoplankton biomass
#' 
#' @description Takes input as 2D array (box x day) and uses either interpolation or climatology to fill in 
#' gaps in data based on duration and location of the gap in the timeseries. 
#' 
#' @input.mat numeric matrix (box x day) of one atlantis variable (preferably after transformation to mg N m-3)
#' @var.name string. Name of atlantis variable to be filled
#' @ref.year numeric. Current data year
#' @ref.year.dates Dates. Dates for current year
#' @doy.file string. full path for DOY climatology Atlantis-Formatted file
#' @max.interp numeric. Maximum gap length where interpolation method is valid
#' @write.gaps logical. Should box gap information be written to file
#' @gaps.dir string. Output director for box gap information
#' 
#' @author J. Caracappa
#' 

# load('C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/var_test.R')
# input.mat = atl.var.ls[[1]]
# var.name = 'Diatom_N'
# doy.file = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Atlantis_Format/Phyto_Climatology.nc'
# max.interp = 3
# write.gaps = T
# ref.year = 2000
# ref.year.dates = seq.Date(as.Date(paste0(ref.year,'-01-01')),as.Date(paste0(ref.year,'-12-31')),by = 1)
# gaps.dir = 'C:/Users/joseph.caracappa/Documents/Satellite_Phyto/Diagnostics/Gap_Analysis/'

fill_satphyto_gaps = function(input.mat,
                              var.name,
                              doy.file,
                              max.interp,
                              write.gaps,
                              gaps.dir,
                              ref.year,
                              ref.year.dates){
  
  `%>%` = dplyr::`%>%`
  #Read in climatology data
  sat.doy = ncdf4::nc_open(doy.file)
  var.doy = ncdf4::ncvar_get(sat.doy,var.name)
  
  #Loop over all boxes, find, and fill gaps
  boxes = 0:29
  
  b=1
  if(write.gaps){
    var.gaps.ls = list()
  }
  for( b in 1:length(boxes)){
    #Generate box timeseries
    var.box = input.mat[1,b,]
    
    #Identify gaps in data
    n.na = sum(is.na(var.box))
    # print(n.na)
    
    ##if no NA values then skip
    if(n.na == 0){
      var.gaps.ls[[b]] = NULL
      next()
    }
    
    #Use rle() to identify gaps and duration, with start and end dates
    var.box.na = !is.finite(var.box)
    var.box.rle = rle(var.box.na)
    var.box.gaps = data.frame(box = boxes[b],
                              lengths = var.box.rle$lengths,
                              values = var.box.rle$values) %>%
      dplyr::mutate(stop = cumsum(lengths),
             start = c(1,dplyr::lag(stop)[-1]+1)) %>%
      dplyr::select(box,lengths,values,start,stop) %>%
      dplyr::filter(values == T) %>%
      dplyr::mutate(start.date = ref.year.dates[start],
             stop.date = ref.year.dates[stop])
    var.gaps.ls[[b]] = var.box.gaps
    
    #Extract DOY Climatology for box
    var.box.doy = var.doy[1,b,]
    
    #Loop through each gap iteration
    for(g in 1:nrow(var.box.gaps)){
      gap.length = var.box.gaps$lengths[g]
      doy.range = var.box.gaps$start[g]:var.box.gaps$stop[g]
      
      #First year no data vs gap
      if(ref.year == 1997){
        #Start date '1997-09-07'
        before.dates = which(ref.year.dates < as.Date('1997-09-07'))
        before.dates.gap = which(!(doy.range %in% before.dates))
        doy.range = doy.range[before.dates.gap]

        #Exception if missing dates in beginning of 1997 start
        start.doy = which(ref.year.dates == as.Date('1997-09-07'))
        if(start.doy %in% doy.range){
          input.mat[1,b,doy.range] = var.box.doy[doy.range]
          next()
        }
      }
      
      if(366 %in% doy.range| 365 %in% doy.range){
        #replace day 366 with climatology doy 365
        if(length(ref.year.dates) == 366){
          input.mat[1,b,366] = var.box.doy[365]
        }
        #replace rest with DOY
        not.leap.day = doy.range[which(doy.range != 366)]
        input.mat[1,b,not.leap.day] = var.box.doy[not.leap.day]
      }else if(1 %in% doy.range){
        input.mat[1,b,doy.range] = var.box.doy[doy.range]
      }else if(gap.length <= max.interp | any(is.na(var.box.doy[doy.range]))){
        pre.gap = doy.range[1]-1
        post.gap = doy.range[gap.length]+1
        m = (var.box[post.gap]-var.box[pre.gap])/(post.gap-pre.gap)
        input.mat[1,b,doy.range] = var.box[pre.gap]+m*(1:gap.length)
      }else{
        input.mat[1,b,doy.range] = var.box.doy[doy.range]
      }

   }
  }
  
  #combine gaps data to single DF
  var.gaps = dplyr::bind_rows(var.gaps.ls)
  
  if(write.gaps){
    #write gaps data to file
    write.csv(var.gaps, file = paste0(gaps.dir,'satellite_phyto_gaps_',ref.year,'.csv'),row.names = F)
  }
  
  return(input.mat)
}

# b=24
# plot(atl.var.ls[[1]][1,b,],type='l')
# plot(input.mat[1,b,],type='l')
# lines(var.doy[1,b,],col='red')
