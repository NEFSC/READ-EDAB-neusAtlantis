#Functions used to edit migration parameters in migrations.csv (after v6665 migration updates)

mig.file = here::here('currentVersion','neus_migrations.csv')
group.name = 'BFT'
StartStage = 1 # 0 = juv, 1 = adult
unit = 'value'
value = 0.5
overwrite = F
VarName = 'MigPropSizeInc'
new.file.name = here::here('currentVersion','neus_migrations_test.csv')


edit_param_mig_csv = function(mig.file,group.name,StartStage,VarName,unit,value,overwrite = F, new.file.name){
  
  mig.orig = read.csv(mig.file,as.is = T)  
  
  if(!(group.name %in% mig.orig$GroupCode)){ stop('Species not in migration file!')}
  
  val.id = which(mig.orig$GroupCode == group.name & mig.orig$StartStage == StartStage)
  var.id = which(colnames(mig.orig) == VarName)
  
  orig.val = mig.orig[val.id,var.id]
  
  if(unit == 'value'){
      new.val = value
  }else{
      new.val = orig.val * value
  }
  
  mig.new = mig.orig
  mig.new[val.id,var.id] = new.val
  
  if(overwrite == T){
    write.csv(mig.new,mig.file,row.names = F)
  }else{
    write.csv(mig.new,new.file.name,row.names =F)
  }
  
  
}
