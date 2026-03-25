#Script creates Atlantis forcing scenarios where catch.ts is manipulated to target certain fishing complexes
library(dplyr)

run.forcing = T
write.ts = F
proj.dir = here::here('','')
# proj.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'

experiment.id = 'eof_targeting_1'

#Species mappings
fgs = read.csv(paste0(proj.dir,'/currentVersion/neus_groups.csv'))
group_map = read.csv(paste0(proj.dir,'/diagnostics/functional_groups_match.csv')) |> 
  rename(Group = 'FisheryComplex',SubGroup = 'Code') |> 
  dplyr::select(Group,SubGroup)

#utitlity scripts & packages
source(paste0(proj.dir,'R/Forcing_Fishing/get_forcing_ts.r'))
source(paste0(proj.dir,'R/Forcing_Fishing/edit_forcing_ts_df.r'))

# Create at_force_LINUX.prm and runAtlantis.sh and put into a new directory
experiment.dir = paste0(proj.dir,'currentVersion/',experiment.id)
if(!dir.exists(experiment.dir)){
  dir.create(experiment.dir)  
}

catch.file.orig = paste0(proj.dir,'currentVersion/CatchFiles/total_catch.ts')

#specify original run.sh
run.sh.orig = paste0(proj.dir,'currentVersion/RunAtlantis_cloud.sh')
run.sh.lines = readLines(run.sh.orig)
run.command.line = grep('atlantisMerged',run.sh.lines)

#Specify original force.prm
force.file.orig = paste0(proj.dir,'currentVersion/at_force_LINUX.prm')
force.lines = readLines(force.file.orig)
catch.file.line = grep('Catchts0.data',force.lines)

#Get info on fishing complexes
group.names = sort(unique(group_map$Group))

groups_to_test  <- group.names[which(group.names != 'Other')]
factors_to_test  <- exp(seq(log(1E-2),log(20),length.out = 10))
eof.thresh = exp(seq(log(1E4),log(4E6),length.out = 10))

scenario_params <- expand.grid(
  dominant_group = groups_to_test,
  dominance_factor = factors_to_test,
  eof_threshold = eof.thresh
)
scenario_params$catch.scalar = NA
scenario_params$run.id = NA
nrow(scenario_params)

## For each Time calculate the proportion of Value for each Variable

##aggregate by year
base.catch.d = get_forcing_ts(code = NULL, filenm = 'total_catch',time = 'day') |> 
  dplyr::rename(catch.orig = 'Value',
                Time.d = 'Time')

base.catch.y = base.catch.d |> 
  dplyr::mutate(Time.y = floor(Time.d/ 365)) |> 
  dplyr::group_by(Time.y,Variable) |> 
  dplyr::summarise(catch.orig = sum(catch.orig,na.rm=T))

base.catch.y.tot = base.catch.y |> 
  dplyr::group_by(Time.y) |> 
  dplyr::summarise(catch.annual = sum(catch.orig,na.rm=T))

ref_weights = base.catch.y |> 
  group_by(Time.y) |> 
  mutate(Weight = catch.orig / sum(catch.orig, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(SubGroup = 'Variable',
         Time = 'Time.y') |> 
  select(SubGroup, Time, Weight) |> 
  filter(!is.na(Weight))

#Loop through scenario params
i=1
scenario_config.ls = list()
for(i in 1:nrow(scenario_params)){
# for(i in 1:2){

  scenario_params$run.id[i] = i
  #### Define Scenario scaling factors
  
  # Get parameters for the current iteration
  current_dominant_group <- as.character(scenario_params$dominant_group[i])
  current_factor <- scenario_params$dominance_factor[i]
  current_thresh <- scenario_params$eof_threshold[i]
  
  if(run.forcing){
    cat(sprintf("Running Scenario %d: Dominant Group = %s, Factor = %.1f\n", 
                i, current_dominant_group, current_factor))
    
    # Run the function
    scenario_output <- atlantiseof::generate_dominant_scenario(
      dominant_group_name = current_dominant_group,
      dominance_factor = current_factor,
      group.mapping = group_map,
      ref_sub_weights = ref_weights,
      rounding_digits = 4
    )
  
    ###Setup catch scaling
    
    #set scaling factor
    if(current_thresh == 0){
      catch.scale = 0
    } else {
      a = current_thresh/base.catch.y$catch.orig
      catch.scale = mean(a[is.finite(a)],na.rm=T)
    }
    scenario_params$catch.scalar[i] = catch.scale
    
    #multiple by new scalars from scenario_output
    new.catch = base.catch.y.tot |> 
      dplyr::left_join(scenario_output, by = c('Time.y' = 'Time')) |> 
      dplyr::mutate(catch.new = catch.annual * subgroup_weight * catch.scale,
                    catch.scale = catch.scale,
                    Time.d = Time.y * 365) |>
      dplyr::left_join(base.catch.y, by = c('Time.y',"SubGroup" = "Variable"))
   
    scenario_config.ls[[i]] = new.catch |> 
      dplyr::mutate(run.id = scenario_params$run.id[i]) |> 
      dplyr::left_join(scenario_params)
    #test that scalar applied overall and stop if fails
    new.catch.test = new.catch |>
      dplyr::group_by(Time.y,Group) |>
      dplyr::summarise(group.Total = sum(catch.orig, na.rm = T),
                       group.NewTotal = sum(catch.new,na.rm=T),
                       group_weight = mean(group_weight)) |>
      dplyr::group_by(Time.y) |>
      dplyr::mutate(Total = sum(group.Total,na.rm=T),
                    NewTotal = sum(group.NewTotal,na.rm=T)) |>
      dplyr::mutate(old.group_weight = group.Total/Total,
                    desiredTotal = Total * catch.scale * group_weight,
                    diff.tot = (desiredTotal - group.NewTotal)/desiredTotal,
                    diff.weight = group_weight - old.group_weight)
    
    if(new.catch.test$diff.tot |> abs() |> max(na.rm=T) > 0.01){
      message('Warning: Total catch by group does not match expected total')
      message(paste0('Stopped at scenario ',i))
      stop('Stopping script')
    }
    
    new.catch = new.catch |> 
      dplyr::select(Time.y,SubGroup,catch.new)
    
    #### Apply new catch forcing file and write out
    #Create new catch TS files
    new.catch.d = base.catch.d |>  
      dplyr::mutate(Time.y = floor(Time.d/365)) |> 
      dplyr::left_join(new.catch, by = c('Time.y', 'Variable' = 'SubGroup')) |> 
      dplyr::select(Time.d, Variable, catch.new)

    #Format other files for initialization
  
    if(write.ts){
      
      catch.file.new = paste0(experiment.dir,'/total_catch_',i,'.ts')
      catch.file.new.short = paste0('total_catch_',i,'.ts')
      edit_forcing_ts_df(input_file = catch.file.orig,
                         output_file = catch.file.new,
                         changes_df = new.catch.d,
                         time_col = 'Time.d',
                         code_col = 'Variable',
                         value_col = 'catch.new'
      )
                         
      #update at_force.prm
      force.file.new.short = paste0('at_force_LINUX_',i,'.prm')
      force.file.new = paste0(proj.dir,'currentVersion/',force.file.new.short)
      
      file.copy(force.file.orig, force.file.new,overwrite = T)
      
      force.file.new.lines = readLines(force.file.new)
      catch.file.line.new = paste0('Catchts0.data ',experiment.id,'/',catch.file.new.short)
      
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
  }
  
  #Print progress percent
  print(paste0(round(i/nrow(scenario_params)*100,2),'% complete'))
 
}


system('sudo chmod -R 775 *')

write.csv(scenario_params, paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv'),row.names = F)

#isolate species level scalars
spp.dat = dplyr::bind_rows(scenario_config.ls)
spp.dat.scalars = spp.dat |> 
  dplyr::select(Time.d, run.id,SubGroup, subgroup_weight) |> 
  dplyr::rename(Code = 'SubGroup') |>
  dplyr::left_join(scenario_params) |> 
  dplyr::mutate(input.scalar = catch.scalar * subgroup_weight) |> 
  dplyr::select(run.id, Time.d,Code,input.scalar) |> 
  tidyr::pivot_wider(names_from = 'Code', values_from = 'input.scalar')

write.csv(spp.dat.scalars, paste0(proj.dir,'Setup_Files/',experiment.id,'_species_scalars.csv'),row.names = F)

base.sbatch.array = paste0(proj.dir,'currentVersion/sbatch_scenario_array_base.sh')
new.sbatch.array =  paste0(proj.dir,'currentVersion/sbatch_',experiment.id,'.sh')
file.copy(base.sbatch.array,new.sbatch.array,overwrite = T)

#replace max array number
sbatch.lines = readLines(new.sbatch.array)
new.nodes.line = paste0('#SBATCH --nodes=',ceiling(nrow(scenario_params)/120))
# new.nodes.line = paste0('#SBATCH --nodes=1')
new.array.line = paste0('#SBATCH --array=1-',nrow(scenario_params))
# new.array.line = paste0('#SBATCH --array=1-2')
sbatch.lines[grep('--array',sbatch.lines)] = new.array.line
sbatch.lines[grep('--nodes',sbatch.lines)] = new.nodes.line

#replace directories
new.mkdir = paste0("sudo mkdir -p /atlantisdisk/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID")
sbatch.lines[grep('mkdir',sbatch.lines)] = new.mkdir

new.singularity = paste0( "sudo singularity exec --bind ",proj.dir,"currentVersion:/app/model,/atlantisdisk/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/runAtlantis_$SLURM_ARRAY_TASK_ID.sh")
sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity

writeLines(sbatch.lines,new.sbatch.array)

# system("find . -name "*.sh" -exec chmod +x {} \;")
batch.string = paste0("sbatch ",new.sbatch.array)
system(batch.string)


