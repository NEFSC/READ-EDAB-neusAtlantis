


run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/'
run.name = 'DOG_WSK_Test_3'
group.name = 'DOG'
long.name = 'Spiny_Dogfish'
full.name = 'Spiny Dogfish'

num.names = paste0(long.name,1:10,'_Nums')
sn.names = paste0(long.name,1:10,'_StructN')
rn.names = paste0(long.name,1:10,'_ResN')

var.names = length(num.names)

init.nc = nc_open(here::here('currentVersion','neus_init.nc'))
out.df = data.frame(age = 1:10,nums = NA, SN = NA, RN = NA, totN = NA,biomass = NA)
for(i in 1:var.names){
  nums = ncvar_get(init.nc,num.names[i])
  nums = sum(nums,na.rm=T)
  
  sn = ncatt_get(init.nc,sn.names[i],'_FillValue')$value
  
  rn = ncatt_get(init.nc,rn.names[i],'_FillValue')$value
  
  totN = sn+rn
  
  biomass = nums*totN
  
  out.df$nums[i] = nums
  out.df$SN[i] = sn
  out.df$RN[i] = rn
  out.df$totN[i] = totN
  out.df$biomass[i] = biomass
}
nc_close(init.nc)

# numbers = readRDS(paste0(run.dir,run.name,'/Post_Processed/Data/numbers_age.RDS')) %>%
#   filter(biom)




bio = read.table(paste0(run.dir,run.name,'/neus_outputAgeBiomIndx.txt'),header = T)
bio = bio %>% select('Time',starts_with(group.name))
bio.long = reshape2::melt(bio,id.vars = 'Time') %>% filter(Time == max(Time))

neus.nc = nc_open(paste0(run.dir,run.name,'/neus_output.nc'))
neus.df = data.frame(age = 1:10, nums = NA, SN = NA, RN = NA, TotN = NA)
for(i in 1:var.names){
  
  nums = ncvar_get(neus.nc,num.names[i])
  nt= dim(nums)
  nums = nums[,,nt]
  nums = sum(nums,na.rm=T)
  
  sn = ncvar_get(neus.nc,sn.names[i])[,,nt]
  sn = sum(sn,na.rm=T)
  
  rn = ncvar_get(neus.nc,rn.names[i])[,,nt]
  rn = sum(rn,na.rm=T)
  
  totN = sn + rn
  
  neus.df$nums[i] = nums
  neus.df$SN[i] = sn
  neus.df$RN[i] = rn
  neus.df$TotN[i] = totN
}
nc_close(neus.nc)

png(paste0(run.dir,run.name,'/Post_Processed/Initial Biomass.png'),width = 10, height = 10, units = 'in',res = 300)
par(mfrow = c(6,1))
barplot(out.df$nums, names.arg = 1:10,main = 'Init Numbers')
barplot(neus.df$nums,names.arg = 1:10, main ='End Numbers')
barplot(out.df$totN, names.arg = 1:10,main = 'Init TotN')
barplot(neus.df$TotN,names.arg = 1:10, main ='End TotN')
barplot(out.df$biomass, names.arg = 1:10,main = 'Init Biomass')
barplot(bio.long$value,names.arg = 1:10, main ='End Biomass')
dev.off()
