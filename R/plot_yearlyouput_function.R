
#function cannot be parallelized because of how R holds NC in memory, read only one file at a time


# List of packages for session
.packages = c("devtools","dtplyr","stringi","data.table","tidyverse","stringr","R.utils","magrittr",
              "scales","RNetCDF","ggforce","pdftools",
              "rgdal","gridExtra", "esquisse","sf", "here")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

foldername <- "Atlantis_mv_1_6645_5yr_fprintfsp34_out1dy"
output_foldername  <- "outputFolder"
yearsrun  <- 5
output_freqyrs <- 1
this.output.nc <- "AMPS_OUT.nc"
func_grouplist <- "PugetSoundAtlantisFunctionalGroups.csv"
mig.file <- "PugetMigrations.csv"


get.nc.data <- function(eachgroup,thisncfile){
  
  print(paste("Analyzing this group",eachgroup))
  
  this.sprow <- fg.list %>% 
    filter(name==eachgroup) 
  
  print(this.sprow)
  
  group.ages <- paste(eachgroup,1:this.sprow$NumCohorts,sep="")
  
  print(group.ages)
  
  #make names for nc variables
  
  varlist <- c("_ResN","_Nums","_StructN","_Wage")
  
  var.listdata <- list()
  
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
        
        thisData <- var.get.nc(thisncfile, eachvariable)
        
        if(eachvarlist == "_Wage") {
          
          thisData<-thisData*20*5.7*(3.65/2.65)/1000000
          
          variable.type = "Wage"
        }
        
        thisData[thisData==0]<-NA  # Replace 0's with NA
        thisData <- thisData[1:7,2:89,1:thistimestep]
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
        
        thisData <- var.get.nc(thisncfile, eachvariable)
        thisData[thisData==0]<-NA  # Replace 0's with NA
        print(dim(thisData))
        thisData <- thisData[1:7,2:89,1:thistimestep]
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
    mutate(Year=(time*73)/365)
  
  
  print(paste("Done with group",eachgroup))
  
  
  return(thissp.data)
}


year_plot <- function(foldername, output_foldername, yearsrun, output_freqyrs, this.output.nc, func_grouplist, mig.file){
  
  #read functional group file
  fg.list <- read_csv(here(func_grouplist)) %>% 
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>% 
    filter(!Code %in% c("DIN","DL"))
  
  #get vertebrate names for nc file
  vert.groups <- fg.list %>% 
    filter(IsTurnedOn==1) %>% 
    filter(GroupType == "FISH" | GroupType == "SHARK" | GroupType == "BIRD"| GroupType == "MAMMAL") %>% 
    dplyr::select(name) %>% .$name
  
  #open nc file
  nc <- open.nc(this.output.nc)
  nc.data <- read.nc(nc)
  
  group.atlantis.data <- lapply(vert.groups, get.nc.data, thisncfile = nc) %>% 
    bind_rows()
  
  #this can be changed, right now just plotting numbers
  
  thisvariabletype <- "Nums"
  thisdataset <- group.atlantis.data %>% 
    filter(!code %in% c("DIN","DL")) %>% 
    filter(variable_type==thisvariabletype) %>% 
    mutate(age = as.factor(age))
  
  write_csv(thisdataset, file=paste0(thisvariabletype,".csv"))

  folder.paths <- here(paste0(c(foldername)),output_foldername)
  
  model.ver <- 1:length(folder.paths)
  
  theseyears <- 1:yearsrun
  
  thistimestep <- (yearsrun*365)/output_freqyrs #this is max number of timesteps/73 output frequency
  
  maxtimestep <- yearsrun*365
  
  mig.table <- read_csv(mig.file)
  
  mig.species <- mig.table %>% 
    distinct(GroupCode) %>% 
    pull(GroupCode)
  
  lapply(mig.species, plot_migrations, thisdataset, mig.table, fg.list)
    
}

plot_migrations <- function(eachgroup, thisdataset, mig.table, fg.list){
  
  group.nums <- thisdataset %>% 
    filter(code==eachgroup)
  
 mig.params <- mig.table %>% 
   filter(GroupCode==eachgroup)
 
 group.params <- fg.list %>% 
   filter(GroupCode==eachgroup)
 
 group.name <- fg.list %>% 
   filter(GroupCode==eachgroup) %>% 
   pull(longname)
    
  sp.nums.plot <- group.nums %>% 
    mutate(year_sim = rep(1:yearsrun,each = 365, times = group.params$NumCohorts),
           day_year = rep(1:365,each = 1, times = (yearsrun*group.params$NumCohorts)),
           age = as.factor(age)) %>% 
    ggplot(aes(x= day_year, y = variable, colour = age)) +
    geom_line()+
    facet_wrap(year_sim ~ age, scales = "free_y") +
    labs(title=group.name, subtitle = "Rows are simulation years,columns are age classes \n 
       Lines: red = migration, green = return, orange = spawning") +
    scale_y_continuous(limits = c(0,NA)) +
    geom_vline(xintercept = mig.params$StartTofY,  
               color = "coral4", size=0.7) +
    geom_vline(xintercept = (mig.params$startTofY + mig.params$Return_Period),  
               color = "coral4",  size=0.7) +
    geom_vline(xintercept = mig.params$EndTofY,  
               color = "forestgreen",  size=0.7, alpha = 0.5) +
    geom_vline(xintercept = mig.params$EndTofY+ReturnPeriod,  
               color = "forestgreen",  size=0.7, alpha = 0.5) +
    geom_vline(xintercept = 197,  
               color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = 243,  
               color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = 152,  
               color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
    geom_vline(xintercept = 198,  
               color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)
  
  ggsave(here("chc_daily_numbers.png"),chc.nums.plot, dpi= 300, width = 12, height = 10)
  
  
}


