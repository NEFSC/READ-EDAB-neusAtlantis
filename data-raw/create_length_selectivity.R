#Script to calculate the selectivity curves for each groundfish species

#logistic fit
source(here::here('R','Calibration_Tools','fit_length_selectivity.R'))
source(here::here('R','Calibration_Tools','edit_param_fleet.R'))

fleets.file = here::here('currentVersion','neus_fisheries.csv')
fleets = read.csv(fleets.file)
gf.fleets = fleets$Code[grepl('^gf',fleets$Code)]
#1) Define selectivity based on the smallest sized groundfish species
len.ref = readRDS(here::here('data-raw','gfTripLengths.rds'))
gf.spp = sort(unique(len.ref$Code))

len.spp = len.ref %>%
  group_by(Code)%>%
  summarise(max.l = quantile(LENGTH,0.99,na.rm=T))
smallest.spp = len.spp$Code[which(len.spp$max.l == min(len.spp$max.l))]

length.smallest = len.ref %>%
  filter(Code == smallest.spp)%>%
  select(Code,LENGTH)

fit.smallest = fit_length_selectivity(data = length.smallest,Code = smallest.spp)

for(f in 1:length(gf.fleets)){
  edit_param_fleet(harvest.file = here::here('currentVersion','at_harvest.prm'),  
                   Fleet = gf.fleets[f],
                   VarName = 'sel_b',
                   Value = fit.smallest$b[1],
                   Unit = 'Value',
                   overwrite =T
  )
  edit_param_fleet(harvest.file = here::here('currentVersion','at_harvest.prm'),  
                   Fleet = gf.fleets[f],
                   VarName = 'sel_lsm',
                   Value = fit.smallest$lsm[1],
                   Unit = 'Value',
                   overwrite =T
  )
}

                 
#2) Define age-based escapement for the others to mirror their empirical selectivity curves

source(here::here('R','Calibration_Tools','edit_param_escape.R'))

fgs = read.csv(here::here('currentVersion','neus_groups.csv'))
s=1
for(s in 1:length(gf.spp)){
  
  
  #Set flagescapement to 2 for all groundfish fleets
  edit_param_escape(harvest.file = here::here('currentVersion','at_harvest.prm'),  
                    Code = gf.spp[s],
                   Fleet = gf.fleets,
                   fleets.file = here::here('currentVersion','neus_fisheries.csv'),
                   VarName = 'flagescapement',
                   Value = 2,
                   overwrite =T)
  
  #Fit lm to length selectivity
  len.spp.ref = len.ref  %>%
    filter(Code == gf.spp[s])%>%
    select(Code,LENGTH)
  
}



