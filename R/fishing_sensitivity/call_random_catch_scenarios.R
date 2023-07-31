#Reads in setup file and executes batcher

proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
# proj.dir = here::here('/')
experiment.id = 'random_catch1'

source(paste0(proj.dir,'R/fishing_sensitivity/make_random_catch_scenarios.R'))

make_random_catch_scenarios(experiment.id = experiment.id,
                                         seed = 13,
                                         N = 250,
                                         proj.dir = proj.dir,
                                         proj.duration.yr = 5,
                                         event.start.d = 20805,
                                         shuffle.start.yr = 21,
                                         shuffle.stop.yr = 57
)

system('sudo chmod -R 775 *')

setup.df = read.csv(paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv'))

base.sbatch.array = paste0(proj.dir,'currentVersion/sbatch_scenario_array_base.sh')
new.sbatch.array =  paste0(proj.dir,'currentVersion/sbatch_',experiment.id,'.sh')
file.copy(base.sbatch.array,new.sbatch.array,overwrite = T)

#replace max array number
sbatch.lines = readLines(new.sbatch.array)
new.array.line = paste0('#SBATCH --array=1-',nrow(setup.df))
sbatch.lines[grep('--array',sbatch.lines)] = new.array.line

#replace directories
new.mkdir = paste0("sudo mkdir -p ",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID")
sbatch.lines[grep('mkdir',sbatch.lines)] = new.mkdir

new.singularity = paste0( "sudo singularity exec --bind ",proj.dir,"currentVersion:/app/model,",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6536.sif /app/model/RunAtlantis_",experiment.id,"_$SLURM_ARRAY_TASK_ID.sh")
sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity

writeLines(sbatch.lines,new.sbatch.array)

# system("find . -name "*.sh" -exec chmod +x {} \;")
batch.string = paste0("sbatch ",new.sbatch.array)
system(batch.string)
