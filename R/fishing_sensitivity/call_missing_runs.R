library(dplyr)
proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'

experiment.id = 'fscale3'

missing.run.id = c(130,140,167,184,198,199,206,212,220,222)
missing.run.txt = paste(missing.run.id,collapse = ',')
# system('sudo chmod -R 775 *')

setup.df = read.csv(paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv'))
  # filter(ID %in% missing.run.id)

base.sbatch.array = paste0(proj.dir,'currentVersion/sbatch_scenario_array_base.sh')
new.sbatch.array =  paste0(proj.dir,'currentVersion/sbatch_',experiment.id,'_revise.sh')
file.copy(base.sbatch.array,new.sbatch.array,overwrite = T)

#replace max array number
sbatch.lines = readLines(new.sbatch.array)
new.array.line = paste0('#SBATCH --array=',missing.run.txt)
sbatch.lines[grep('--array',sbatch.lines)] = new.array.line

#replace directories
new.mkdir = paste0("sudo mkdir -p ",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID")
sbatch.lines[grep('mkdir',sbatch.lines)] = new.mkdir

new.singularity = paste0( "sudo singularity exec --bind ",proj.dir,"currentVersion:/app/model,",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6536.sif /app/model/runAtlantis_",experiment.id,"_$SLURM_ARRAY_TASK_ID.sh")
sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity

writeLines(sbatch.lines,new.sbatch.array)

# system("find . -name "*.sh" -exec chmod +x {} \;")
batch.string = paste0("sbatch ",new.sbatch.array)
system(batch.string)
