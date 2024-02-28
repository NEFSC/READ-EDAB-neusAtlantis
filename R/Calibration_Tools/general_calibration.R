#'Master wrapper for calibration routines
#'Requires a setup file in the form of /diagnostics/cloud_calibration_setup_example.csv
#'Run.ID: Numberic code used to group runs together
#'Run.Group: User-defined grouping of runs for sensible run comparisons
#'Code: Atlantis functional group code, or for diet changes the formatted predator-prey pair separated by semicolon (e.g. 1MAK1:ZL)
#'Type: Parameter names; mQ, mL, mum, C, BHalpha, BHbeta, KDENR, diet
#'Unit: supplied value type: value or scalar
#'Value: Value to be written to biology.prm
library(dplyr)

#Read in setup file
experiment.id = 'cloud_v6681_calib_3'
setup.df = read.csv(here::here('Setup_Files','cloud_v6681_calib_3.csv'),as.is=T)
# setup.df = read.csv(here::here('diagnostics','cloud_calibration_setup_example.csv'))
proj.dir = '/contrib/Joseph.Caracappa/calibration/'

#Define base files
bio.file.orig = here::here('currentVersion','at_biology.prm')
run.sh.orig = here::here('currentVersion','RunAtlantis_cloud.sh')
sbatch.orig = here::here('currentVersion','sbatch_scenario_array_base.sh')
fgs.file = here::here('currentVersion','neus_groups.csv')

#Read in functional groups and define inverts
fgs = read.csv(fgs.file,as.is = T)
invert.groups = fgs$Code[which(fgs$NumCohorts==1)]

#identify run numbers
run.id = sort(unique(setup.df$Run.ID))

#Read in necessary functions from setup.df
if(any(c('mQj','mQa','mQ','mLj','mLa','mL') %in% setup.df$Type )){
  source(here::here('R','Calibration_Tools','edit_param_mortality_age.R'))
  source(here::here('R','Calibration_Tools','edit_param_invert_mortality.R'))
  mort.age.orig = get_param_mort_age(bio.file.orig,fgs.file )
  mort.invert.orig = get_param_invert_mort(bio.file.orig,fgs.file)
}
if('mum' %in% setup.df$Type | 'C' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_mum_age.R'))
  source(here::here('R','Calibration_Tools','edit_param_C_age.R'))
  source(here::here('R','Calibration_Tools','edit_param_invert_c_mum.R'))
  c.age.orig = get_param_C_age(bio.file.orig)
  mum.age.orig = get_param_mum_age(bio.file.orig)
  mum.c.invert.orig = get_param_invert_c_mum(bio.file.orig,invert.groups)
}
if('BHalpha' %in% setup.df$Type | 'BHbeta' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_BH.R'))
  bh.orig = get_param_BH(bio.file.orig)
}
if('KDENR' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_KDENR.R'))
  kdenr.orig = get_param_KDENR(bio.file.orig)
}
if('diet' %in% setup.df$Type){
  source(here::here('R','Calibration_Tools','edit_param_pprey.R'))
  diet.orig = get_pprey_vals(atl.dir = here::here('currentVersion','/'),
                             biol.file = bio.file.orig,
                             fgs.file = fgs.file)
  prey.names = colnames(diet.orig)[-1]
}
if(any(c('E','EPlant','EDR','EDL') %in% setup.df$Type)){
  source(here::here('R','Calibration_Tools','edit_param_assim_eff.R'))
}
if(any('FSP' %in% setup.df$Type)){
  source(here::here('R','Calibration_Tools','edit_param_FSP.R'))
}

if(any('FSPB' %in% setup.df$Type)){
  source(here::here('R','Calibration_Tools','edit_param_FSPB.R'))
  fspb.orig = get_param_FSPB(bio.prm = bio.file.orig)
}

possible.types = unique(read.csv(here::here('diagnostics','cloud_calibration_setup_example.csv'),as.is=T)$Type)

#Loop through run id's
i=1
out.df = list()
for(i in 1:length(run.id)){
  
  #copy biology.prm with run.id prefix
  bio.file.short = paste0('at_biology_',run.id[i],'.prm')
  bio.file.new = here::here('currentVersion',bio.file.short)
  file.copy(bio.file.orig, bio.file.new,overwrite = T)
  
  #Separate task for run.id
  setup.run = filter(setup.df, Run.ID == run.id[i])
  
  j=1
  for(j in 1:nrow(setup.run)){
    
    if(!(setup.run$Type[j] %in% possible.types)){
      stop(paste0("Type: ", setup.run$Type[j]," Run.ID: ",setup.run$Run.ID[j]," is not compatible with this script"))
    }
    
    is.invert = setup.run$Code[j] %in% invert.groups
    
    #Do mQ or mL tasks
    if(setup.run$Type[j] %in% c('mQa','mQj','mLa','mLj','mL','mQ')){

      if(is.invert){
        
        if(setup.run$Type[j] %in% c('mLa','mLj','mL')){
          mort.group.old = mort.invert.orig$mL[which(mort.invert.orig$group == setup.run$Code[j])]
        }else{
          mort.group.old = mort.invert.orig$mQ[which(mort.invert.orig$group == setup.run$Code[j])] 
        }
        
        if(setup.run$Type[j] == 'value'){
          mort.group.new = setup.run$Value[j]
        }else{
          mort.group.new = as.numeric(mort.group.old) * setup.run$Value[j]
        }
        edit_param_invert_mort(bio.file = bio.file.new,
                               group = setup.run$Code[j],
                               type = setup.run$Type[j],
                               value = mort.group.new,
                               new.file = F)
      
      }else{
        
        group.age = substr(setup.run$Type[j],nchar(setup.run$Type[j]),nchar(setup.run$Type[j]))
        group.type = substr(setup.run$Type[j],1,2)
        
        if(group.type == 'mL'){
          mort.group.old = mort.age.orig %>%
                  filter(group == setup.run$Code[j])%>%
                  select(mL.j,mL.a)%>%
                  as.numeric()
        }else{
              mort.group.old = mort.age.orig %>%
                filter(group == setup.run$Code[j])%>%
                select(mQ.j,mQ.a)%>%
                as.numeric()
        }
        
        if(group.age == 'j'){
          mort.group.old = mort.group.old[1]
        }else{
          mort.group.old = mort.group.old[2]
        }
        
        if(setup.run$Unit[j] == 'value'){
          group.value =  setup.run$Value[j]
        }else{
          group.value = mort.group.old * setup.run$Value[j]
        }
        
        edit_param_mort_age(bio.prm = bio.file.new,
                            group.name = setup.run$Code[j],
                            age = group.age,
                            type = group.type,
                            value = group.value,
                            overwrite = T
        )
      }
    }
  
    #Do mum/C tasks
    if(setup.run$Type[j] %in% c('mum','C')){
      
      #invert mum/c
      if(is.invert == T){
        
        mum.c.invert.match = which(mum.c.invert.orig$Code == setup.run$Code[j])
        old.c.val = as.numeric(mum.c.invert.orig$c[mum.c.invert.match])
        old.mum.val = as.numeric(mum.c.invert.orig$mum[mum.c.invert.match])
        
        if(setup.run$Type[j] == 'mum'){
          new.mum.val = ifelse(setup.run$Unit[j] == 'scalar', old.mum.val * setup.run$Value[j], setup.run$Value[j])
          
          edit_param_invert_c_mum(bio.file = bio.file.new,
                                  group =setup.run$Code[j],
                                  type = 'mum',
                                  value = new.mum.val,
                                  new.file = F)
          
        }else{
          new.c.val = ifelse(setup.run$Unit[j] == 'scalar', old.c.val * setup.run$Value[j], setup.run$Value[j])
          
          edit_param_invert_c_mum(bio.file = bio.file.new,
                                  group =setup.run$Code[j],
                                  type = 'C',
                                  value = new.c.val,
                                  new.file = F)
          
        }
      #age mum/c                       
      }else{
        
        mum.age.match = which(mum.age.orig$group == setup.run$Code[j])
        c.age.match  = which(c.age.orig$group == setup.run$Code[j])
        
        if(setup.run$Type[j] == 'mum'){
          
          if(setup.run$Unit[j] == 'scalar'){
            new.mum.val = as.numeric(mum.age.orig[mum.age.match,2:11]) * setup.run$Value[j]
          }else{
              stop("workflow doesn't support values for mum/C")
          }
          edit_param_mum_age(bio.prm = bio.file.new,
                             new.mum = new.mum.val,
                             overwrite = T,
                             single.group =T,
                             group.name = setup.run$Code[j]
                            )
        }else{
          if(setup.run$Unit[j] == 'scalar'){
            new.c.val = as.numeric(c.age.orig[c.age.match,2:11]) * setup.run$Value[j]
          }else{
            stop("workflow doesn't support values for mum/C")
          }
          edit_param_C_age(bio.prm = bio.file.new,
                             new.C = new.c.val,
                             overwrite = T,
                             single.group =T,
                             group.name = setup.run$Code[j]
          )
        }
        
      }
      
    }
    
    #Do BH alpha/beta tasks
    if(setup.run$Type[j] %in% c('BHalpha','BHbeta')){
      
      bh.match = which(bh.orig$group == setup.run$Code[j])
      old.alpha.val = as.numeric(bh.orig$alpha[bh.match])
      old.beta.val = as.numeric(bh.orig$beta[bh.match])
      
      if(setup.run$Type[j] == 'BHalpha'){
        new.alpha.val = ifelse(setup.run$Unit[j]=='scalar',old.alpha.val * setup.run$Value[j], setup.run$Value[j])
        new.beta.val = NA
      }else{
        new.alpha.val = NA
        new.beta.val = ifelse(setup.run$Unit[j]=='scalar',old.beta.val * setup.run$Value[j], setup.run$Value[j])
      }
      edit_param_BH(bio.prm = bio.file.new,
                    group.name = setup.run$Code[j],
                    alpha = new.alpha.val,
                    beta = new.beta.val,
                    overwrite = T
                    )
    }
    
    #Do KDENR tasks
    if(setup.run$Type[j] == 'KDENR'){
      
      kdenr.match = which(kdenr.orig$group == setup.run$Code[j])
      old.kdenr = as.numeric(kdenr.orig$KDENR[kdenr.match])
      new.kdenr = ifelse(setup.run$Unit == 'scalar', old.kdenr * setup.run$Value[j], setup.run$Value[j])
      edit_param_KDENR(bio.prm = bio.file.new,
                       group.name = setup.run$Code[j],
                       KDENR = new.kdenr,
                       overwrite = T)
    }
    
    #Do Diet tasks
    if(setup.run$Type[j] == 'diet'){
      
      group.string = strsplit(setup.run$Code[j],split = ':')[[1]]
      pred.name = group.string[1]
      prey.name = group.string[2]
      pred.match = which(diet.orig$pred.names == pred.name)
      prey.match = which(prey.names == prey.name)
      old.pprey = as.numeric(diet.orig[pred.match,prey.match+1])
      new.pprey = ifelse(setup.run$Unit[j] == 'scalar', old.pprey * setup.run$Value[j], setup.run$Value[j])
      
      edit_param_pprey(atl.dir = here::here('currentVersion','/'),
                       biol.file = bio.file.new,
                       fgs.file = fgs.file,
                       pred.list = pred.name,
                       prey.list = prey.name,
                       pprey.vals = new.pprey,
                       overwrite = T
                      )
    }
    
    #Do Assimilation Efficiency
    if(setup.run$Type[j] %in% c('E','EPlant','EDR','EDL')){
  
      edit_param_assim_eff(bio.file = bio.file.new,
                           spp.names = setup.run$Code[j],
                           type = setup.run$Type[j],
                           unit = setup.run$Unit[j],
                           value = setup.run$Value[j],
                           overwrite = T)
    }
    
    #Do FSP
    if(setup.run$Type[j] == 'FSP'){
      
      edit_param_FSP(bio.prm = bio.file.new,
                     group.name = setup.run$Code[j],
                     unit = setup.run$Unit[j],
                     value = setup.run$Value[j],
                     overwrite = T
                     )
      
    }
    
    if(setup.run$Type[j] == 'FSPB'){
      
      fspb.old = as.numeric(fspb.orig[which(fspb.orig$group == setup.run$Code[j]),][-1])
      fspb.old = fspb.old[!is.na(fspb.old)]
      
      if(setup.run$Unit[j] == 'scalar'){
        
        fspb.new = fspb.old * setup.run$Value[j]
      }else{
        stop("Only type 'scalar' works for FSPB changes")
      }
      
      edit_param_FSPB(bio.prm = bio.file.new,
                      group.name = setup.run$Code[j],
                      FSPB = fspb.new,
                      overwrite =T
                      )
    }
    
  }
  
  #Write shell script
  run.sh.short = paste0('RunAtlantis_',run.id[i],'.sh')
  run.sh.new = here::here('currentVersion',run.sh.short)
  file.copy(run.sh.orig,run.sh.new)
  
  #Edit shell script
  run.sh.new.lines = readLines(run.sh.new)
  run.command.line = grep('atlantisMerged',run.sh.new.lines)
  run.command.new =  paste0('atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX.prm -p at_physics.prm -b ',bio.file.short,' -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -m neus_migrations.csv -t . -d output')
  run.sh.new.lines[run.command.line] = run.command.new
  
  writeLines(run.sh.new.lines,con = run.sh.new)
  
  #Track new param files 
  setup.run$bio.file = bio.file.short
  setup.run$run.sh.file = run.sh.short
  
  out.df[[i]] = setup.run
}
system('sudo chmod -R 775 *')

out.df = bind_rows(out.df)
write.csv(out.df, paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv'),row.names = F)


base.sbatch.array = paste0(proj.dir,'currentVersion/sbatch_scenario_array_base.sh')
new.sbatch.array =  paste0(proj.dir,'currentVersion/sbatch_',experiment.id,'.sh')
file.copy(base.sbatch.array,new.sbatch.array,overwrite = T)

#replace max array number
sbatch.lines = readLines(new.sbatch.array)
new.array.line = paste0('#SBATCH --array=1-',length(run.id))
sbatch.lines[grep('--array',sbatch.lines)] = new.array.line

#replace directories
new.mkdir = paste0("sudo mkdir -p ",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID")
sbatch.lines[grep('mkdir',sbatch.lines)] = new.mkdir

new.singularity = paste0( "sudo singularity exec --bind ",proj.dir,"currentVersion:/app/model,",proj.dir,"Atlantis_Runs/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6681.sif /app/model/RunAtlantis_$SLURM_ARRAY_TASK_ID.sh")
sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity

writeLines(sbatch.lines,new.sbatch.array)

# system("find . -name "*.sh" -exec chmod +x {} \;")
batch.string = paste0("sbatch ",new.sbatch.array)
system(batch.string)
