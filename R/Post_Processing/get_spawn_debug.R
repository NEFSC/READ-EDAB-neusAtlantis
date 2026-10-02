#get spawning data from log.txt debug lines

#So given a fixed StructN amount, to get positive spawning biomass per age class you need to satisfy: (KSPA-SN(A+1))/(FSP-1) - 3.65*SN >= 0 , where A is the RN:SN. 

library(dplyr)

run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/ReduceHerStart_7c/'

log.lines = readLines(paste0(run.dir,'log.txt'))

spawn.lines = log.lines[grep('Spawn: ',log.lines)]

parse.spawn = function(x){
  x2 = strsplit(x,split = ' |\t|,')[[1]]
  out.df = data.frame(time = as.numeric(x2[2]),
                      group = x2[4],
                      age = as.numeric(x2[8]),
                      box = as.numeric(x2[11]),
                      layer = as.numeric(x2[14]),
                      tot.spawn = as.numeric(x2[17]),
                      ind.spawn = as.numeric(x2[20]),
                      den = as.numeric(x2[23]),
                      fspb = as.numeric(x2[26]),
                      SN = as.numeric(x2[29]),
                      RN = as.numeric(x2[32]),
                      FSP = as.numeric(x2[35]),
                      KSPA = as.numeric(x2[38]),
                      step1 = as.numeric(x2[40]),
                      RSprop = as.numeric(x2[42]))
  return(out.df)
}

nlines = 100000
spawn.ls = lapply(spawn.lines[1:nlines], parse.spawn)
spawn.df = dplyr::bind_rows(spawn.ls)
rm(spawn.ls)

write.csv(spawn.df,paste0(run.dir,'Post_Processed/spawn_debug.csv'))


her.spawn = spawn.df %>%
  filter(group == 'HER') %>%
  group_by(time,group,age) %>%
  summarize(tot.spawn = sum(tot.spawn,na.rm=T),
            ind.spawn = sum(ind.spawn,na.rm=T),
            den = sum(den,na.rm=T),
            FSPB = mean(fspb,na.rm=T),
            SN = sum(SN,na.rm=T),
            RN = sum(RN,na.rm=T),
            FSP = mean(FSP),
            KSPA = mean(KSPA),
            step1 = sum(step1),
            RSprop = mean(RSprop)
  ) %>%
  mutate(wgt = SN + RN,
         wgt.opt = 3.65 * SN,
         spwn = (FSP * wgt.opt)- KSPA,
         wgt.check = FSPB * SN,
         wgt.flag = wgt < wgt.opt,
         ind.spawn.flag = (FSPB * step1) == ind.spawn,
         tot.spawn.flag = (ind.spawn * den) == tot.spawn)

spawn.check = her.spawn %>%
  select(RN, SN, FSPB, den, FSP, KSPA, FSPB) %>%
  mutate(rn.sn = RN/SN,
         w.opt = 3.65*SN,
         w = RN + SN,
         ind.spawn = ((FSP*w.opt)-KSPA) - (w.opt - w),
         tot.spawn = ind.spawn * den * FSPB
         )

mak.spawn = spawn.df %>%
  filter(group == 'SUF') %>%
  group_by(time,group,age) %>%
  summarize(tot.spawn = sum(tot.spawn,na.rm=T),
            ind.spawn = sum(ind.spawn,na.rm=T),
            den = sum(den,na.rm=T),
            FSPB = mean(fspb,na.rm=T),
            SN = sum(SN,na.rm=T),
            RN = sum(RN,na.rm=T),
            FSP = mean(FSP),
            KSPA = mean(KSPA),
            step1 = sum(step1)
  ) %>%
  mutate(wgt = SN + RN,
         wgt.opt = 3.65 * SN,
         spwn = (FSP * wgt.opt)- KSPA,
         wgt.check = FSPB * SN,
         wgt.flag = wgt < wgt.opt,
         ind.spawn.flag = (FSPB * step1) == ind.spawn,
         tot.spawn.flag = (ind.spawn * den) == tot.spawn)
#Ecology_Age_Structured_Spawn            
  