# Create a biomass length distribution reference from survdat
library(dplyr)

#get rawdata from existing survdat pull
sd.len.raw = readRDS(here::here('data','survey_lenagewgt.rds'))

spp.names = sort(unique(sd.len.raw$Code))

bin.width = 5

out.ls = list()
for(i in 1:length(spp.names)){
  sd.len.spp =sd.len.raw %>%
    filter(Code == spp.names[i])
    
  
  spp.len.min = floor(min(sd.len.spp$LENGTH)/bin.width)*bin.width
  spp.len.max = ceiling(max(sd.len.spp$LENGTH)/bin.width)*bin.width
  spp.len.seq = seq(spp.len.min, spp.len.max,bin.width)
  
  spp.len.freq = as.data.frame(table(cut(sd.len.spp$LENGTH,breaks = spp.len.seq)))%>%
    mutate(len.lower = spp.len.seq[-length(spp.len.seq)],
           len.upper = spp.len.seq[-1],
           rel.freq = Freq/sum(Freq),
           cum.prob = cumsum(rel.freq),
           Code = spp.names[i],
           min.year = min(sd.len.spp$YEAR),
           max.year = max(sd.len.spp$YEAR))%>%
    rename(freq = 'Freq')%>%
    select(Code,min.year,max.year,len.lower,len.upper,freq,rel.freq,cum.prob)
  
  # plot(cum.prob~len.lower,spp.len.freq,type = 'b')
  
  out.ls[[i]] = spp.len.freq
}

out.df = bind_rows(out.ls)

saveRDS(out.df,here::here('data','survdat_length_reference.RDS'))
