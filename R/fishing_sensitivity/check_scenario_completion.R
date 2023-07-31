scenario.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/Atlantis_Runs/fspike2/'

run.dirs = list.dirs(scenario.dir,recursive = F)
run.names = list.dirs(scenario.dir,full.names = F,recursive = F)

expected.end.time = 28105
out.df= data.frame(run.name = run.names, max.time = NA, complete = NA)

for(i in 1:length(run.names)){
  
  bio.file = paste0(run.dirs[i],'/neus_outputBiomIndx.txt')
  if(file.exists(bio.file)){
    data = data.table::fread(bio.file)
    out.df$max.time[i] = data$Time[nrow(data)]
    out.df$complete[i] = abs(out.df$max.time[i]-expected.end.time)<5  
  }else{
    out.df$max.time[i] = NA
    out.df$complete[i] = F
  }
  
}
