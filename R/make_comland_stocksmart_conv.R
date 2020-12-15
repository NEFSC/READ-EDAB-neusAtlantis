# Script to generate conversion factor between 
# A) comland_meatwt_deflated_EPU.Rds and
# B) stockData.Rds
# Compare biomass by year for groups with catch in both datasets
# How consistent is data within a group
# between groups

library(dplyr)
library(ggplot2)

#Read in yearly-aggregated data (made in "R/compare_catch_sources.R")
data.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Catch_Data/"
fig.dir = "C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Figures/"
load(paste0(data.dir,'Catch_Comparison.RData'))

#Rename columns and join
new.comland.df = new.comland.df %>%
  rename(Catch.comland = "Catch") %>%
  select(-source)

stocksmart.df = stocksmart.df %>%
  rename(Catch.stocksmart = 'Catch') %>%
  select(-source)

catch.all  = new.comland.df %>%
  left_join(stocksmart.df, by = c('Group','Year')) %>%
  mutate(both.present = !is.na(Catch.comland) & !is.na(Catch.stocksmart)) %>%
  filter(both.present == T) %>%
  select(-both.present) %>%
  mutate(ratio = Catch.stocksmart/Catch.comland)

ratio.spp = catch.all %>%
  group_by(Group) %>%
  #really skewed use median
  summarize(ss.comland.conv = median(ratio,na.rm=T))

# saveRDS(ratio.spp,paste0(data.dir,'stocksmart_comland_ratio.Rds'))
saveRDS(ratio.spp,here::here('data-raw','stocksmart_comland_ratio.Rds'))
# ratio.overall = median(ratio.spp$ratio,na.rm=T)
# 
# png(paste0(fig.dir,'Catch/Stocksmart_Comland_Ratio.png'),width = 12, height = 5, units = 'in', res = 150)
# hist(ratio.spp$ratio, xlab = 'Stocksmart:Comland_EPU',main = '')
# abline(v=ratio.overall)
# dev.off()

# png(paste0(fig.dir,'Catch/Stocksmart_Comland_Ratio_Zoom.png'),width = 12, height = 5, units = 'in', res = 150)
# hist(ratio.spp$ratio,breaks = seq(0,800,2), xlab = 'Stocksmart:Comland_EPU',main = '',xlim = c(0,60))
# text(53,3,'YTF')
# text(21,3,'LSK')
# text(c(15,15),c(3,4), c('BIL','SK'))
# text(c(11,13),c(3,3),c('WSK','BLF'))
# dev.off()

