#Script to get abundance estimates from StockSmart data

library(dplyr)

#Read in StockSmart Data
data = readRDS(here::here('data-raw','stockData.Rds'))

convert.number = function(x){
  if(x %in% c('Number','Adult Fish')){
    return(1)
  }else if(x %in%  c('Number x 1,000')){
    return(1E6)
  }else if(x == 'Number x 10,000,000,000'){
    return(1E10)
  }else {
    return(NA)
  }
}

#Filter by abundance
abund = data %>%
  filter(Metric == 'Abundance') %>%
  select(Species,Year,Value,Metric,Units,Code) %>%
  group_by(Code,Units) %>%
  summarize(Abundance = median(Value)) 

#Calculate unit conversion
abund$conv.scalar = sapply(abund$Units,convert.number)
abund$abundance.numbers = abund$Abundance * abund$conv.scalar

#Aggregate over multiple units, wights are as NAs
abund.final = abund %>%
  group_by(Code) %>%
  summarize(abundance.numbers = sum(abundance.numbers))

