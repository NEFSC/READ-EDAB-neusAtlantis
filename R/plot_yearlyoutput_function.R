#' @title Function to plot daily output and migration dates 
#' @author Hem Nalini Morzaria-Luna hmorzaria@hotmail.com
#' @description Function to extract Number, weight-at-age, SN and RN and plot numbers from run with daily output  
#function cannot be parallelized because of how R holds NC in memory, read only one file at a time

get.nc.data <- function(eachgroup,nc.file,fg.file,thistimestep){
  
  print(paste("Analyzing this group",eachgroup))
  
  this.sprow <- read.csv(fg.file) %>% 
    filter(Code==eachgroup) 
  
  group.name = this.sprow$Name
  
  print(this.sprow)
  
  group.ages <- paste(group.name,1:this.sprow$NumCohorts,sep="")
  
  print(group.ages)
  
  #make names for nc variables
  
  varlist <- c("_ResN","_Nums","_StructN","_Wage")
  
  var.listdata <- list()
  
  nc.dat = open.nc(nc.file)
  
  for(eachvar in 1:length(varlist)){
    
    eachvarlist <- varlist[eachvar]
    
    print(eachvarlist)
    
    name.var <- paste(group.ages,eachvarlist,sep="")
    
    variable.type <- gsub("_","",eachvarlist)
    
    if(eachvarlist == "_ResN" | eachvarlist == "_StructN" | eachvarlist == "_Wage") {
      
      for(eachage in 1:length(name.var)) {
        
        if(eachvarlist == "_Wage"){
          
          eachvarlist = "_ResN"
          name.var <- paste(group.ages,eachvarlist,sep="")
          variable.type <- gsub("_","",eachvarlist)
          eachvarlist = "_Wage"
        }
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- var.get.nc(nc.dat, eachvariable)
        
        if(eachvarlist == "_Wage") {
          
          thisData<-thisData*20*5.7*(3.65/2.65)/1000000
          
          variable.type = "Wage"
        }
        
        thisData[thisData==0]<-NA  # Replace 0's with NA
        thisData <- thisData[1:4,2:30,1:thistimestep]
        thisDataMeanMg <-apply(thisData,3,mean,na.rm = TRUE) #Get mean size over time, averaging over depth and location 
        
        thisY <-tibble(variable=thisDataMeanMg/thisDataMeanMg[1]) %>%  # Normalize by initial value
          mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
        
        if(eachvarlist == "_Wage") {
          
          thisY <-tibble(variable=thisDataMeanMg) %>%  # Normalize by initial value
            mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
          
        }
        
        listname <- paste(thisY$group[1],"_",thisY$variable_type[1],sep="")
        var.listdata[[listname]] <- thisY  
        
      } 
      
    } else if (eachvarlist == "_Nums") {
      
      for(eachage in 1:length(name.var)) {
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- var.get.nc(nc.dat, eachvariable)
        thisData[thisData==0]<-NA  # Replace 0's with NA
        print(dim(thisData))
        thisData <- thisData[1:4,2:30,1:thistimestep]
        #thisData <- thisData[1:7,2:89,1:51] #use this for 10 year runs
        thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location  
        thisY <-tibble(variable=thisDataNums) %>%  # Normalize by initial value
          mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
        
        var.listdata[[eachvariable]] <- thisY  
        
      }
    }
  }
  
  thissp.data <- var.listdata %>% 
    bind_rows() %>% 
    mutate(code = this.sprow$Code, longname = this.sprow$longname) %>% 
    dplyr::rename(atlantis_group = group) %>% 
    mutate(Year=(time*output_freqyrs)/365)
  
  
  print(paste("Done with group",eachgroup))
  
  
  return(thissp.data)
}

# thisdataset = get.nc.data(eachgroup = 'BLF',nc.file,fg.file,thistimestep)

year_plot <- function(run.dir, out.dir, yearsrun, output_freqyrs, thisncfile, fg.list, mig.file, thisvariabletype){
  
  #read functional group file
  fg.list <- read_csv(fg.file) %>% 
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, Name, LongName) %>% 
    filter(!Code %in% c("DIN","DL"))
  
  #get vertebrate names for nc file
  vert.groups <- fg.list %>% 
    filter(IsTurnedOn==1) %>% 
    filter(GroupType == "FISH" | GroupType == "SHARK" | GroupType == "BIRD"| GroupType == "MAMMAL") %>% 
    dplyr::select(Code) %>% .$Code
  
  #open nc file
  # nc <- open.nc(here(run.dir,out.dir, this.output.nc))
  # nc.data <- read.nc(nc)
  
  group.atlantis.data <- lapply(vert.groups, get.nc.data, nc.file = nc.file , fg.file = fg.file, thistimestep = thistimestep ) %>% 
    bind_rows()
  
  #this can be changed, right now just plotting numbers
  
  thisdataset <- group.atlantis.data %>% 
    filter(!code %in% c("DIN","DL")) %>% 
    filter(variable_type==thisvariabletype) %>% 
    mutate(age = as.factor(age))
  
  write_csv(thisdataset, file=paste0(out.dir,'/',thisvariabletype,".csv"))

  mig.species <- read.csv(mig.file) %>% 
    distinct(GroupCode) %>% 
    pull(GroupCode)
  
  mig.species = as.character(mig.species)
  mig.species = mig.species[mig.species %in% vert.groups]
  
  lapply(mig.species, plot_migrations, thisdataset = thisdataset, mig.file = mig.file, fg.file = fg.file)
    
}

plot_migrations <- function(eachgroup, thisdataset, mig.file, fg.file){
  
  group.nums <- thisdataset %>% 
    filter(code==eachgroup,variable_type == 'Nums')
  
  mig.params <- read.csv(mig.file) %>% 
    filter(GroupCode==eachgroup)
  
  group.params <- fg.list %>% 
    filter(Code==eachgroup)
  
  group.name <- fg.list %>% 
    filter(Code==eachgroup) %>% 
    pull(LongName)
  
  if(nrow(mig.params)==2){
    
    sp.nums.plot <- group.nums %>% 
      mutate(year_sim = rep(1:yearsrun,each = 365, times = group.params$NumCohorts),
             day_year = rep(1:365,each = 1, times = (yearsrun*group.params$NumCohorts)),
             age = as.factor(age)) %>% 
      ggplot(aes(x= day_year, y = variable, colour = age)) +
      geom_line()+
      facet_wrap(year_sim ~ age, scales = "free_y") +
      labs(title=group.name, subtitle = "Rows are simulation years,columns are age classes \n 
       Lines: red = migration, green = return") +
      scale_y_continuous(limits = c(0,NA)) +
      geom_vline(xintercept = mig.params$StartTofY[1],  
                 color = "coral4", size=0.7) +
      geom_vline(xintercept = (mig.params$StartTofY[1] + mig.params$Return_Period[1]),  
                 color = "coral4",  size=0.7) +
      geom_vline(xintercept = mig.params$EndTofY[1],  
                 color = "forestgreen",  size=0.7, alpha = 0.5) +
      geom_vline(xintercept = mig.params$EndTofY[1]+mig.params$Return_Period[1],  
                 color = "forestgreen",  size=0.7, alpha = 0.5) 
    #can be used for spawning, need table
    # geom_vline(xintercept = 197,  
    #            color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
    # geom_vline(xintercept = 243,  
    #            color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
    # geom_vline(xintercept = 152,  
    #            color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
    # geom_vline(xintercept = 198,  
    #            color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)
    # 
    ggsave(paste0(out.dir, '/',eachgroup,"_daily_numbers.png"),sp.nums.plot, dpi= 300, width = 12, height = 10)
    
  }
  
  if(nrow(mig.params)==4){
    
    
    sp.nums.plot <- group.nums %>% 
      mutate(year_sim = rep(1:yearsrun,each = 365, times = group.params$NumCohorts),
             day_year = rep(1:365,each = 1, times = (yearsrun*group.params$NumCohorts)),
             age = as.factor(age)) %>% 
      ggplot(aes(x= day_year, y = variable, colour = age)) +
      geom_line()+
      facet_wrap(year_sim ~ age, scales = "free_y") +
      labs(title=group.name, subtitle = "Rows are simulation years,columns are age classes \n 
       Lines: red = migration, green = return, blue = second migration, orange = second return") +
      scale_y_continuous(limits = c(0,NA)) +
      geom_vline(xintercept = mig.params$StartTofY[1],  
                 color = "coral4", size=0.7) +
      geom_vline(xintercept = (mig.params$StartTofY[1] + mig.params$Return_Period[1]),  
                 color = "coral4",  size=0.7) +
      geom_vline(xintercept = mig.params$EndTofY[1],  
                 color = "forestgreen",  size=0.7, alpha = 0.5) +
      geom_vline(xintercept = mig.params$EndTofY[1]+mig.params$Return_Period[1],  
                 color = "forestgreen",  size=0.7, alpha = 0.5) +
      geom_vline(xintercept = mig.params$StartTofY[3],  
                 color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
      geom_vline(xintercept = mig.params$StartTofY[3] + mig.params$Return_Period[3],  
                 color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) #+
    # geom_vline(xintercept = 152,  
    #            color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
    # geom_vline(xintercept = 198,  
    #           color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)
    # 
    ggsave(here(paste0(eachgroup,"_daily_numbers.png")),sp.nums.plot, dpi= 300, width = 12, height = 10)
  }
  
}




