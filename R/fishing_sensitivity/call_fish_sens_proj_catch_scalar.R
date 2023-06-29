#Reads in setup file and executes batcher

proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
# proj.dir = here::here('/')
experiment.id = 'fscale2'

source(paste0(proj.dir,'R/fishing_sensitivity/make_fish_sens_proj_catch_scalar_species.R'))

make_fish_sens_proj_catch_scalar_species(proj.dir = proj.dir,
                                         experiment.id = experiment.id,
                                         proj.length.d = 365*20,
                                         run.length.d = 28105,
                                         event.start.d = 20805,
                                         event.end.d = 28105,
                                         fishing.levels = c(0,2,5,10,25,50,100),
                                         fishing.levels.text = c('0','1','5','10','25','50','100'),
                                         make.catch.files = T
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

new.singularity = paste0( "sudo singularity exec --bind ",proj.dir,"currentVersion:/app/model,",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6536.sif /app/model/runAtlantis_$SLURM_ARRAY_TASK_ID.sh")
sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity

writeLines(sbatch.lines,new.sbatch.array)

# system("find . -name "*.sh" -exec chmod +x {} \;")
batch.string = paste0("sbatch ",new.sbatch.array)
# system(batch.string)
