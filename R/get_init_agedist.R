init.nc = nc_open(here::here('currentVersion','neus_init.nc'))

num.names = paste0('Herring',1:10,'_Nums')
sn.names = paste0('Herring',1:10,'_StructN')
rn.names = paste0('Herring',1:10,'_ResN')

out.df = data.frame(age = 1:10,nums = NA, SN = NA, RN = NA, totN = NA)
for(i in 1:length(var.names)){
  nums = ncvar_get(init.nc,num.names[i])
  nums = sum(nums,na.rm=T)
  
  sn = ncvar_get(init.nc,sn.names[i])
  sn = sum(sn,na.rm=T)
  
  rn = ncvar_get(init.nc,rn.names[i])
  rn = sum(rn,na.rm=T)
  
  totN = sn+rn
  
  out.df$nums[i] = nums
  out.df$SN[i] = sn
  out.df$RN[i] = rn
  out.df$totN[i] = totN
}
nc_close(init.nc)

bio = read.table('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HER_Fix_1/neus_outputAgeBiomIndx.txt',header = T)
bio = bio %>% select('Time',starts_with('HER'))
bio.long = reshape2::melt(bio,id.vars = 'Time') %>% filter(Time == 0)

neus.nc = nc_open('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HER_Fix_1/neus_output.nc')
neus.df = data.frame(age = 1:10, nums = NA, SN = NA, RN = NA, TotN = NA)
for(i in 1:length(var.names)){
  nums = ncvar_get(neus.nc,num.names[i])[,,1]
  nums = sum(nums,na.rm=T)
  
  sn = ncvar_get(neus.nc,sn.names[i])[,,1]
  sn = sum(sn,na.rm=T)
  
  rn = ncvar_get(neus.nc,rn.names[i])[,,1]
  rn = sum(rn,na.rm=T)
  
  totN = sn + rn
  
  neus.df$nums[i] = nums
  neus.df$SN[i] = sn
  neus.df$RN[i] = rn
  neus.df$TotN[i] = totN
}
nc_close(neus.nc)

png('C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/HER_Fix_1/Post_Processed/Initial Biomass.png',width = 10, height = 10, units = 'in',res = 300)
par(mfrow = c(6,1))
barplot(out.df$nums, names.arg = 1:10,main = 'Init Numbers')
barplot(bio.long$value,names.arg = 1:10, main ='Output Biomass')
barplot(neus.df$nums,names.arg = 1:10, main ='Output Numbers')
barplot(neus.df$SN,names.arg = 1:10,main = 'Output SN')
barplot(neus.df$RN,names.arg = 1:10, main ='Output RN')
barplot(neus.df$TotN,names.arg = 1:10, main ='Output TotN')
dev.off()
