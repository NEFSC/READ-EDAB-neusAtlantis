#script to generate catch forcing by a scalar of the base ts file and run the output
library(dplyr)

experiment.id = 'catch_thresholds_eof_2'

write.out =F

# proj.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
proj.dir = here::here('','')

#utitlity scripts & packages
source(paste0(proj.dir,'R/Forcing_Fishing/get_forcing_ts.r'))
source(paste0(proj.dir,'R/Forcing_Fishing/scale_forcing_ts.r'))

#Define threshold range
thresh.v = seq(0,1e7,1E5)

#Get base catch
base.catch = get_forcing_ts(code = NULL, filenm = 'total_catch',time = 'annual')
##aggregate by year
base.catch.yr = base.catch %>%
  group_by(Time) %>%
  summarise(Value = sum(Value,na.rm=T))
  
# Create at_force_LINUX.prm and runAtlantis.sh and put into a new directory
dir.create(paste0(proj.dir,'currentVersion/',experiment.id))

#specify original run.sh
run.sh.orig = paste0(proj.dir,'currentVersion/RunAtlantis_cloud.sh')
run.sh.lines = readLines(run.sh.orig)
run.command.line = grep('atlantisMerged',run.sh.lines)

#Specify original force.prm
force.file.orig = paste0(proj.dir,'currentVersion/at_force_LINUX.prm')
force.lines = readLines(force.file.orig)
catch.file.line = grep('Catchts0.data',force.lines)

i=1
setup.df = data.frame(run = 1:length(thresh.v),catch.threshold = thresh.v, catch.scalar = NA, catch.force= NA)
#Loop over all thresholds
for(i in 1:length(thresh.v)){
  
  #set scaling factor
  if(thresh.v[i] == 0){
    catch.scale = 0
    } else {
      a = thresh.v[i]/base.catch.yr$Value
     catch.scale = mean(a[is.finite(a)],na.rm=T)
    }
  setup.df$catch.scalar[i] = catch.scale
  # print(catch.scale)

  #Create new catch TS files
  new.catch.ts = paste0('total_catch_',i)
  setup.df$catch.force[i] = new.catch.ts
  
  if(write.out){
    temp.scaled = scale_forcing_ts(code = unique(base.catch$Variable),
                                   tstype = 'catch',
                                   value = catch.scale,
                                   operation = 'multiply',filename = new.catch.ts,overwrite =T)
  

  #update at_force.prm
  force.file.new.short = paste0('at_force_LINUX_',i,'.prm')
  force.file.new = paste0(proj.dir,'currentVersion/',force.file.new.short)
  
  file.copy(force.file.orig, force.file.new,overwrite = T)
  
  force.file.new.lines = readLines(force.file.new)
  catch.file.line.new = paste0('Catchts0.data CatchFiles/',new.catch.ts,'.ts')
  
  force.file.new.lines[catch.file.line] = catch.file.line.new
    
  writeLines(force.file.new.lines, con = force.file.new )
  
  #Do run.sh duplication
  run.file.new = paste0(proj.dir,'currentVersion/',paste0('runAtlantis_',i,'.sh'))
    
  file.copy(run.sh.orig, run.file.new,overwrite=T)
    
    
  run.file.new.lines = readLines(run.file.new)
  run.command.new =  paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f ',force.file.new.short,' -p at_physics.prm -b at_biology.prm -m neus_migrations.csv -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output')
  run.file.new.lines[run.command.line] = run.command.new
    
  writeLines(run.file.new.lines,con = run.file.new)
  }
  print(i)
}
  
system('sudo chmod -R 775 *')

out.df = setup.df
write.csv(out.df, paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv'),row.names = F)


base.sbatch.array = paste0(proj.dir,'currentVersion/sbatch_scenario_array_base.sh')
new.sbatch.array =  paste0(proj.dir,'currentVersion/sbatch_',experiment.id,'.sh')
file.copy(base.sbatch.array,new.sbatch.array,overwrite = T)

#replace max array number
sbatch.lines = readLines(new.sbatch.array)
new.array.line = paste0('#SBATCH --array=1-',nrow(setup.df))
sbatch.lines[grep('--array',sbatch.lines)] = new.array.line

#replace directories
new.mkdir = paste0("sudo mkdir -p /atlantisdisk/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID")
sbatch.lines[grep('mkdir',sbatch.lines)] = new.mkdir

new.singularity = paste0( "sudo singularity exec --bind ",proj.dir,"currentVersion:/app/model,/atlantisdisk/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/runAtlantis_$SLURM_ARRAY_TASK_ID.sh")
sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity

writeLines(sbatch.lines,new.sbatch.array)

# system("find . -name "*.sh" -exec chmod +x {} \;")
batch.string = paste0("sbatch ",new.sbatch.array)
system(batch.string)
  