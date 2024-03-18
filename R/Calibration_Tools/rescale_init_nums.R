#Script to rescale initial abundance based on discrepency to a reference run
library(dplyr)
library(tidync)
library(ncdf4)

source(here::here('R','Calibration_Tools','edit_param_init_scalar.R'))

ref.run.dir = '/home/jcaracappa/atlantis/Shared_Data/Dev_Runs/Dev_20231101/'

new.run.dir = here::here('Atlantis_Runs','6536_new_age_param_init_rescale','')

run.prm = here::here('currentVersion','at_run.prm')
fgs.file.ref =here::here('diagnostics','neus_groups_v2_0_1.csv')
fgs.file.new = here::here('currentVersion','neus_groups.csv')

fgs.ref = read.csv(fgs.file.ref,as.is = T)%>%
  filter(IsTurnedOn == T)
fgs.new = read.csv(fgs.file.new,as.is = T)%>%
  filter(IsTurnedOn == T)

# bio.old = read.table(paste0(ref.run.dir,'neus_outputBiomIndx.txt'),as.is = T,header = T)%>%
#   select(Time,all_of(fgs$Code))%>%
#   filter(Time == 0)%>%
#   tidyr::gather(Code,biomass.init,-Time)

main.nc.ref = nc_open(paste0(ref.run.dir,'neus_output.nc'))
main.nc.new = nc_open(paste0(new.run.dir,'neus_output.nc'))

bio.ref = read.table(paste0(ref.run.dir,'neus_outputBiomIndx.txt'),header =T)
bio.new = read.table(paste0(new.run.dir,'neus_outputBiomIndx.txt'),header =T)

vert.names = fgs.ref$Name[which(fgs.ref$NumCohorts >2)]

i=1
init.compare = data.frame(Name = vert.names,numbers.ref = NA,numbers.new = NA)

for(i in 1:length(vert.names)){
  
  ncohort.ref = fgs.ref$NumCohorts[which(fgs.ref$Name == vert.names[i])]
  ncohort.new = fgs.new$NumCohorts[which(fgs.new$Name == vert.names[i])]
  
  # out.ref = lapply(1:ncohort.ref,function(x){
  #   return(sum(ncvar_get(main.nc.ref,paste0(vert.names[i],x,'_Nums'))[,,1],na.rm=T))
  # })
  # out.new = lapply(1:ncohort.new,function(x){
  #   return(sum(ncvar_get(main.nc.new,paste0(vert.names[i],x,'_Nums'))[,,1],na.rm=T))
  # })
  # out.ref = sum(unlist(out.ref))
  # out.new = sum(unlist(out.new))
  
  code.ref = fgs.ref$Code[which(fgs.ref$Name == vert.names[i])]
  
  out.ref = bio.ref[1,code.ref]
  out.new = bio.new[1,code.ref]
  
  init.compare$numbers.ref[i] = out.ref
  init.compare$numbers.new[i] = out.new
  # init.compare$numbers.ratio[i] = out.ref/out.new
  
}

init.compare = init.compare %>%
  mutate(numbers.scalar = numbers.ref/numbers.new)%>%
  left_join(select(fgs.new,Code,Name))%>%
  select(Code,numbers.scalar)

init.scalar.old = get_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                      groups.file = fgs.file.new,
                      write.output = F)  

init.scalar.new = init.scalar.old %>%
  left_join(init.compare, by = c(group = 'Code'))%>%
  mutate(numbers.scalar = ifelse(is.na(numbers.scalar),1,numbers.scalar),
         init.scalar = as.numeric(init.scalar),
         new.scalar = numbers.scalar * init.scalar)%>%
  select(group,new.scalar)%>%
  rename(init.scalar = 'new.scalar')
  
edit_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                       groups.file = fgs.file.new,
                       new.init.scalar = init.scalar.new,
                       overwrite = T)


