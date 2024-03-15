#Script to check the range in relative change for data-rich groups in last N years of data
library(atlantisdiagnostics)
library(dplyr)
library(ggplot2)

#Read in Survdat biomass data
realBiomass <- readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)

groups = unique(realBiomass$Code)

nyrs = 20
yr.start = 1964
yr.stop = 2018

relChange.df = data.frame(Code = groups, relChange.lm = NA)
plot.ls = list()

for(i in 1:length(groups)){
  
  dat = realBiomass %>%
    filter(Code == groups[i] & variable == 'tot.biomass' & YEAR >= yr.start & YEAR <= yr.stop) %>%
    select(YEAR, value) %>%
    group_by(YEAR) %>%
    summarize(value = sum(value,na.rm=T))
    
  year.min = min(dat$YEAR)
  year.max = max(dat$YEAR)
  
  print(paste(groups[i],nrow(dat)))
  if(nrow(dat)>=nyrs){
    dat = dat %>% filter(YEAR >= year.max-nyrs+1)
    
    # plot(value~YEAR,dat,type='l')
    
    model = lm(value~YEAR,dat)
    
    if(dat$value[1] == 0){
      ref.value = dat$value[which(dat$value != 0)[1]]
    }else{
      ref.value = dat$value[1]
    }
    relChange.df$relChange.lm[i] = model$coefficients[2]/ref.value
    
    dat$value = dat$value/dat$value[1]
    dat$Code = groups[i]
    plot.ls[[i]] = dat
  }else{
    relChange.df$relChange.lm[i] = NA
    next()
  }
}

hist(relChange.df$relChange.lm)
mean(abs(relChange.df$relChange.lm),na.rm=T)
median(abs(relChange.df$relChange.lm),na.rm=T)

plot.df = bind_rows(plot.ls)

ggplot(plot.df,aes(x=YEAR,y = value))+
  geom_line()+
  facet_wrap(~Code,scale = 'free_y')+
  ggsave('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/Survday_last20yr.png',width = 12, height = 12, units = 'in', dpi = 300)

