#Script to:
# (1) Pull all fishing sensitivity biomass output
# (2) Join with dev-branch baseline output
# (3) Calculate difference to baseline
# (4) Plot difference as function of fishing manipulation per spp per guild manipulation


# (1) Pull all fishing sensitivity biomass output




f.mort.guild = filter(f.mort.all, in.guild == T)

fished.spp.lm = data.frame(Code = fished.spp, slope = NA)
  
for(i in 1:length(fished.spp)){
  
  f.mort.spp = filter(f.mort.guild, Code == fished.spp[i] & Catch !=0)
  
  if(any(!is.finite(f.mort.spp$F.mort))){
    fished.spp.lm$slope[i] = NA  
  } else{
    f.mort.model =  lm(F.mort~fishing.scalar,data = f.mort.spp)
    
    fished.spp.lm$slope[i] = coef(f.mort.model)[2]
    
  }
  
}
  


