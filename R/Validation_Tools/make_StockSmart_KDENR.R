#Script to pull StockSmart recruitment data by species
#Use median of earliest 5 years of data

library(dplyr)

#Read in StockSmart Data
data = readRDS(here::here('data-raw','stockData.Rds'))

convert.number = function(x){
  if(x %in% c('Thousand Recruits','Number x 1,000','Number x 1000')){
    return(1E3)
  }else if(x %in%  c('Million Recruits','Number x 1,000,000')){
    return(1E6)
  }else if(x == 'Number'){
    return(1)
  }else {
    return(NA)
  }
}

#Filter by recruitment
recruit = data %>%
  filter(Metric == 'Recruitment') %>%
  select(Species,Year,Value,Metric,Units,Code) %>%
  group_by(Code,Units) %>%
  summarize(Recruits = median(Value)) 

#Calculate unit conversion
recruit$conv.scalar = sapply(recruit$Units,convert.number)
recruit$recruits.numbers = recruit$Recruits * recruit$conv.scalar

#Aggregate over multiple units, wights are as NAs
recruit.final = recruit %>%
  group_by(Code) %>%
  summarize(recruit.numbers = sum(recruits.numbers))

#Write output
saveRDS(recruit.final,here::here('data-raw','StockSmart_Median_Recruitment.Rds'))
write.csv(recruit.final,here::here('data-raw','SockSmart_Median_Recruitment.csv'),row.names = F)

# #Find First year of each group
# recruit.first.year = recruit %>%
#   group_by(Code) %>%
#   summarize(Year.First = min(Year))
# 
# #Merge back
# recruit2 = recruit %>%
#   left_join(recruit.first.year) %>%
#   filter(Year < Year.First+5)
