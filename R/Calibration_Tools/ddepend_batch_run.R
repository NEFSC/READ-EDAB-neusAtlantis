library(dplyr)
proj.dir = '/net/work3/EDAB/atlantis/Joe_Proj/'

target.spp = 'GOO'

set.name = 'GOO_ddepend_'

bio.file = paste0(proj.dir,'/currentVersion/at_biology.prm')
run.sh.file = paste0(proj.dir,'/currentVersion/RunAtlantis.sh')

ddepend.val.v = c(0,2,3)
min.temp = 0
max.temp.v = c(5,10)
roc.wgt.v = c(1E-6,1E6)
k.roc.food.v = c(1E-6,1E6)
enviro.displace.v = c(0,1)
enviro.kill.v = c(0,1)


combs = expand.grid(ddepend = ddepend.val.v,max.temp = max.temp.v,roc.wgt = roc.wgt.v,k.roc.food = k.roc.food.v) %>%
# combs = expand.grid(ddepend = ddepend.val.v,max.temp = max.temp.v,enviro.displace = enviro.displace.v,enviro.kill = enviro.kill.v) %>%
  mutate(run.id = 1:n())

i=1

for(i in 1:nrow(combs)){
  
  new.bio.file = paste0(proj.dir,'/currentVersion/',paste0('at_biology_',combs$run.id[i],'.prm'))
  
  file.copy(bio.file,new.bio.file,overwrite = T)
  
  bio.lines = readLines(new.bio.file)
  
  #change ddepend
  which.ddepend = grep(paste0(target.spp,'_ddepend_move'),bio.lines)
  bio.lines[which.ddepend]  = paste0(target.spp,'_ddepend_move ',combs$ddepend[i])
  
  #change min.temp
  which.min.temp = grep(paste0(target.spp,'_min_move_temp'),bio.lines)
  bio.lines[which.min.temp] = paste0(target.spp,'_min_move_temp ',min.temp)
  
  #change max.temp
  which.max.temp = grep(paste0(target.spp,'_max_move_temp'),bio.lines)
  bio.lines[which.max.temp] = paste0(target.spp,'_max_move_temp ',combs$max.temp[i])
  
  #change roc.wgt
  which.roc.wgt = grep('roc_wgt',bio.lines)
  bio.lines[which.roc.wgt] = paste0('roc_wgt ',combs$roc.wgt[i])


  #change k.roc.food
  which.k.roc.food = grep('k_roc_food',bio.lines)
  bio.lines[which.k.roc.food] = paste0('k_roc_food ',combs$k.roc.food[i])

  # #change envior displace
  # which.enviro.displace = grep('^flagenviro_displace',bio.lines)
  # bio.lines[which.enviro.displace] = paste0('flagenviro_displace ',combs$enviro.displace[i])
  # 
  # #change enviro kill
  # which.enviro.kill = grep('^flagenviro_kill',bio.lines)
  # bio.lines[which.enviro.kill] = paste0('flagenviro_kill ',combs$enviro.kill[i])
  # 
  writeLines(bio.lines,new.bio.file)
  
  #Edit RunAtlantis.sh
  
  sh.lines = readLines(run.sh.file)
  run.line = paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX.prm -p at_physics.prm -b at_biology_',combs$run.id[i],'.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -m neus_migrations.csv -q neus_fisheries.csv -t . -d output')
  
  writeLines(run.line,run.sh.file)
  
  run.name = paste0(set.name,'_',combs$run.id[i])
  run.dir =paste0(proj.dir,'/Atlantis_Runs/',run.name,'/')
  
  if(!dir.exists(run.dir)){
    dir.create(run.dir)
  }
  
  podman.cmd = paste0('podman run -d --rm --mount "type=bind,src=/net/work3/EDAB/atlantis/Joe_Proj/currentVersion,dst=/app/model" --mount "type=bind,src=/net/work3/EDAB/atlantis/Joe_Proj/Atlantis_Runs/',run.name,',dst=/app/model/output" atlantis_6681')
  
  print(run.name)
  
  system(podman.cmd)
  
  Sys.sleep(100)
  
  
}