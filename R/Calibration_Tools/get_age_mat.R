#Script to generate a table of functional groups and age at maturity

get_age_mat = function(bio.file,write =F){
    
  bio.lines = readLines(bio.file)
  age.mat.line.num = grep('_age_mat',bio.lines)
  age.mat.line.val = bio.lines[age.mat.line.num]
  #remove comments
  which.comment = grep("#",age.mat.line.val)
  age.mat.line.num = age.mat.line.num[-which.comment]
  age.mat.line.val = age.mat.line.val[-which.comment]
  #pull group names
  grp.names = unname(sapply(age.mat.line.val,function(x)return(strsplit(x,'_')[[1]][1])))
  #pull mature ages
  mat.age = unname(sapply(age.mat.line.val,function(x)return(strsplit(x,' |  |]t')[[1]][2])))
  #format to table and write
  age.mat.df = data.frame(spp = grp.names,age.mat = mat.age)
  return(age.mat.df)
  if(write){
    write.csv(age.mat.df,here::here('diagnostics','group_mature_age.csv'),row.names = F)  
  }

}
