#Script to create the catch length distribution reference from vessel trip reports
library(dplyr)

#Read in original trip data
catch.len.raw = readRDS(here::here('data-raw','gfTripLengths.rds'))

spp.names = sort(unique(catch.len.raw$Code))

bin.width = 5

out.ls = list()
i =1

for(i in 1:length(spp.names)){
  catch.len.spp =catch.len.raw %>%
    filter(Code == spp.names[i])
  
  spp.len.min = floor(min(catch.len.spp$LENGTH)/bin.width)*bin.width
  spp.len.max = ceiling(max(catch.len.spp$LENGTH)/bin.width)*bin.width
  spp.len.seq = seq(spp.len.min, spp.len.max,bin.width)
  
  spp.len.freq = as.data.frame(table(cut(catch.len.spp$LENGTH,breaks = spp.len.seq)))%>%
    mutate(len.lower = spp.len.seq[-length(spp.len.seq)],
           len.upper = spp.len.seq[-1],
           rel.freq = Freq/sum(Freq),
           cum.prob = cumsum(rel.freq),
           Code = spp.names[i])%>%
    rename(freq = 'Freq')%>%
    select(Code,len.lower,len.upper,freq,rel.freq,cum.prob)
  
  # plot(cum.prob~len.lower,spp.len.freq,type = 'b')
  
  out.ls[[i]] = spp.len.freq
}

out.df = bind_rows(out.ls)

saveRDS(out.df,here::here('data','groundfish_catch_length_reference.RDS'))
