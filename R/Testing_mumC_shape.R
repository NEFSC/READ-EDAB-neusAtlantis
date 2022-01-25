#' Bracket parameters
#'
#'

#Source in function to edit biology.prm file
source(file = 'R/edit_param_mum_age_Rob.R')
source(file = 'R/edit_param_C_age.R')

# read in csv and parse/process it
# to get output folder, index, param value etc.
#Read in run index as dataframe
run.index = read.csv('mum_c_testing/HER_3.csv',as.is = T)

# create folder structure
folders = run.index$dir.name
master.dir = 'mum_c_testing/Herring/'


# Generate mum and C dataframe and apply to edit_param_C_age/edit_param_mum_age

i <- 1
for (i in 1:length(folders)){
  group_name <- run.index$Group[i]
  # create sensible folder name has parameter in name (or index)
  system(paste0("mkdir -p mum_c_testing/Herring/",folders[i]))
  
  new.bio.name = paste0('at_biology_',run.index$ID[i],'.prm')
  file.copy('robVersion/at_biology.prm',paste0('robVersion/',new.bio.name),overwrite = T)
  # create new biology file and save 
  mum_by_age <- get_param_mum_age(bio.prm = paste0('robVersion/',new.bio.name), group.name = group_name)
  mum_by_age[,2:11] <- as.numeric(mum_by_age[,2:11])
  mum_by_age$mean_mum <- rowMeans(mum_by_age[,c(2:11)])
  mean_mum <- mum_by_age$mean_mum * run.index$mum_scalar[i]
  
  mum_shape <- c(run.index$mum_1[i], run.index$mum_2[i], run.index$mum_3[i], run.index$mum_4[i], run.index$mum_5[i], run.index$mum_6[i], run.index$mum_7[i], run.index$mum_8[i], run.index$mum_9[i], run.index$mum_10[i])

  new_mum <- mum_shape * mean_mum  
  ##New mum/C vectors and covert into dataframe
  new_C = new_mum * run.index$c_mum_ratio[i]
  # Create cumulative sum shape for C
  new_C[2] <- new_C[1] + new_C[2]
  new_C[3] <- new_C[2] + new_C[3]
  new_C[4] <- new_C[3] + new_C[4]
  new_C[5] <- new_C[4] + new_C[5]
  new_C[6] <- new_C[5] + new_C[6]
  new_C[7] <- new_C[6] + new_C[7]
  new_C[8] <- new_C[7] + new_C[8]
  new_C[9] <- new_C[8] + new_C[9]
  new_C[10] <- new_C[9] + new_C[10]
  
  print(new_mum)
  print(new_C)
  ##Overwrite Biology file
  edit_param_mum_age(bio.prm = paste0('robVersion/',new.bio.name),
                     overwrite = T,
                     new.mum = new_mum,
                     single.group = T,
                     group.name = 'HER'
#                     group.name = run.index$Group[afolder]
  )
  edit_param_C_age(bio.prm = paste0('robVersion/',new.bio.name),
                     overwrite = T,
                     new.C = new_C,
                     single.group = T,
                     group.name = 'HER'
#                     group.name = run.index$Group[afolder]
  )

  # run docker
  run.atlantis.sh = readLines('robVersion/RunAtlantis.sh')
  new.line = paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX.prm -p at_physics.prm -b ',new.bio.name,' -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output ')
  run.atlantis.sh[3] = new.line
  writeLines(run.atlantis.sh, con = 'robVersion/RunAtlantis.sh')
  print("After writeines")
  
  # run docker
  
  #build run command string
 # run <- paste0("docker run -d --mount \"type=bind,src=$PWD/robVersion,dst=/app/model\" --mount \"type=bind,src=$PWD/mum_c_testing/Herring/",folders[afolder],"/,","dst=/app/model/output/\" atlantis_6536")
#  run <- paste0("docker run -d --mount \"type=bind,src=$PWD/robVersion,dst=/app/model\" --mount \"type=bind,src=$PWD/mum_c_testing/Herring/",folders[afolder],"/,","dst=/app/model/output/\" atlantis_6536")
#  run <- paste0("docker run -d --mount \"type=bind,src=$PWD/robVersion,dst=/app/model\" --mount \"type=bind,src=$PWD/",master.dir,folders[afolder],"/,","dst=/app/model/output/\" atlantis_6536")
  run <- paste0("docker run -d --mount \"type=bind,src=$PWD/robVersion,dst=/app/model\" --mount \"type=bind,src=$PWD/",master.dir,folders[i],"/,","dst=/app/model/output/\" atlantis_6536_spawn_debug")
  
  print(run)
  system(run)
  
  # pause for 2 minute in case overwriting biolo.prm cause issues reading
  Sys.sleep(120)          

}
