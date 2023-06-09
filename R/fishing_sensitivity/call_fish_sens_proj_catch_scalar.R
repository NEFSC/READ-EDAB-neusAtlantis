#Reads in setup file and executes batcher

# proj.dir = '/contrib/Joseph.Caracappa/fishing_sensitivity/neus-atlantis/'
proj.dir = here::here('/')
batch.prefix = 'fish_sens_catch_scalar_species_1'

source(paste0(proj.dir,'R/fishing_sensitivity/make_fish_sens_proj_catch_scalar_species.R'))

make_fish_sens_proj_catch_scalar_species(proj.dir = proj.dir,
                                         batch.prefix = batch.prefix,
                                         proj.duration.yr = 20,
                                         fishing.levels = c(0,2,5,10,25,50,100),
                                         fishing.levels.text = c('0','1','5','10','25','50','100'),
                                         make.catch.files = F
)

setup.df = read.csv(paste0(proj.dir,'Setup_Files/',batch.prefix,'.csv'))

#make scenario batch directory
batch.dir = paste0(proj.dir,'Atlantis_Runs/',batch.prefix)
if(!dir.exists(batch.dir)){
  dir.create(batch.dir)
}


i=1
for(i in 1:nrow(setup.df)){
  
  out.dir = paste0(batch.dir,'/',setup.df$Run[i])
  
  if(!dir.exists(out.dir)){
    dir.create(out.dir)  
  }
  
  #Create job shell script
  file.job = paste0(out.dir,'/',setup.df$Run[i],'_job.sh')
  jobConn = file(file.job,open = 'w')
  cat("#!/bin/bash\n",file=jobConn,append=T)
  runString <- paste0("sudo singularity exec --bind ",proj.dir,"currentVersion",":/app/model,",out.dir,":/app/model/output /contrib/atlantisCode/atlantis6536.sif /app/model/",setup.df$sh.script[i]) 
  cat(runString,file=jobConn,append=T)
  close(jobConn)
  
  sbatchString = paste0('sbatch -N 1 ',file.job)
  system(sbatchString)
}


