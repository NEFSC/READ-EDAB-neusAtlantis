#Script creates Atlantis forcing scenarios where catch.ts is manipulated to target certain fishing complexes
library(dplyr)
library(tidyr)

run.forcing =F
write.ts = F # CHANGED to T so files are actually written
existing.setup = T
do.run = T
# proj.dir = here::here('','')
proj.dir = '/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'

experiment.id = 'eof_targeting_4'

existing.setup.file = here::here('Setup_Files',paste0(experiment.id,'_setup.csv'))
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

## For each Time calculate the proportion of Value for each Variable

mt_yr_2_mgn_yr =1E9/(20 * 5.7 )
mt_yr_2_mgn_s = mt_yr_2_mgn_yr/(365 * 86400)


##aggregate by year - UPDATED to use new file_path argument
base.catch.d = get_forcing_ts(file_path = catch.file.orig, code = NULL, time = 'day') |> 
  dplyr::rename(catch.orig.mgN.s = 'Value',
                Time.d = 'Time') %>% 
  dplyr::mutate(catch.orig.mT.d = 86400*catch.orig.mgN.s/mt_yr_2_mgn_yr)

# 1. Identify constant years in the original data by checking the daily range
constant_flags <- base.catch.d |>
  dplyr::mutate(Time.y = floor(Time.d / 365)) |>
  dplyr::group_by(Time.y, Variable) |>
  dplyr::summarise(
    catch_max = max(catch.orig.mgN.s, na.rm = TRUE),
    catch_min = min(catch.orig.mgN.s, na.rm = TRUE),
    # Use a small tolerance (1e-6) instead of '==' to handle floating-point quirks
    is_constant = (catch_max - catch_min) < 1e-6, 
    .groups = 'drop'
  )

# a = filter(base.catch.d, Variable == 'BLF')
# plot(catch.orig.mT.d~Time.d,a,type = 'l')

base.catch.y = base.catch.d |> 
  dplyr::mutate(Time.y = floor(Time.d/ 365)) |> 
  dplyr::group_by(Time.y,Variable) |> 
  dplyr::summarise(catch.orig.mgN.s = mean(catch.orig.mgN.s,na.rm=T), #MEAN not sum
                   catch.orig.mT.y = sum(catch.orig.mT.d,na.rm=T))

# b = filter(base.catch.y, Variable == 'BLF')
# plot(catch.orig.mT.d~Time.y,b,type = 'l')

base.catch.y.tot = base.catch.y |> 
  dplyr::group_by(Time.y) |> 
  dplyr::summarise(catch.annual.mgN.s = sum(catch.orig.mgN.s,na.rm=T),
                   catch.annual.mT.y = sum(catch.orig.mT.y,na.rm=T))

historical_mean_catch <- mean(base.catch.y.tot$catch.annual.mT.y, na.rm=T)

ref_weights = base.catch.y |> 
  group_by(Time.y) |> 
  mutate(Weight = catch.orig.mgN.s / sum(catch.orig.mgN.s, na.rm = TRUE)) |> 
  ungroup() |> 
  rename(SubGroup = 'Variable',
         Time = 'Time.y') |> 
  select(SubGroup, Time, Weight) |> 
  filter(!is.na(Weight))


#Get info on fishing complexes
group.names = sort(unique(group_map$Group))

groups_to_test  <- group.names[which(group.names != 'Other')]
# factors_to_test  <- round(exp(seq(log(1E-2),log(20),length.out = 10)),2)
factors_to_test = c(0.01, 0.02, 0.05, 0.1, 0.3, 0.6, 1, 4, 8, 20)
# eof.thresh = c(0,round(exp(seq(log(1E4),log(1E7),length.out = 10)),-4))
eof.thresh = c(0, 1E4, 1E5, 5E5, 8E5, 9E5, 1E6, 2E6, 4E6, 6E6, 8E6)

eof.thresh/ historical_mean_catch

if(existing.setup == T){
  scenario_params = read.csv(existing.setup.file)
}else{
  scenario_params <- expand.grid(
    dominant_group = groups_to_test,
    dominance_factor = factors_to_test,
    eof_threshold_mT = eof.thresh
    
  )
  scenario_params$eof_threshold_mgN = scenario_params$eof_threshold_mT * mt_yr_2_mgn_s
  scenario_params$catch.scalar = scenario_params$eof_threshold_mT/historical_mean_catch
  scenario_params$run.id = NA
  
  nrow(scenario_params)
}

#Loop through scenario params
i=1
scenario_config.ls = list()
for(i in 1:nrow(scenario_params)){

  scenario_params$run.id[i] = i
  #### Define Scenario scaling factors
  
  # Get parameters for the current iteration
  current_dominant_group <- as.character(scenario_params$dominant_group[i])
  current_factor <- scenario_params$dominance_factor[i]
  current_thresh <- scenario_params$eof_threshold[i]
  current_scale <- scenario_params$catch.scalar[i]
  
  if(run.forcing){
    cat(sprintf("Running Scenario %d: Dominant Group = %s, Factor = %.1f\n", 
                i, current_dominant_group, current_factor))
    
    # Run the function
    scenario_output <- atlantiseof:::generate_dominant_scenario(
      dominant_group_name = current_dominant_group,
      dominance_factor = current_factor,
      group.mapping = group_map,
      ref_sub_weights = ref_weights,
      rounding_digits = 4
    )
    
    ###Setup catch scaling
    
    #set scaling factor (FIXED: Scaling against TOTAL catch, not subgroup catch)
    # if(current_thresh == 0){
    #   catch.scale = 0
    # } else {
    #   historical_mean_catch <- mean(base.catch.y.tot$catch.annual, na.rm=T)
    #   catch.scale = current_thresh / historical_mean_catch
    # }
    # 
    # scenario_params$catch.scalar[i] = catch.scale
    
    #multiple by new scalars from scenario_output (FIXED: Divided by 365 for daily rate)
    new.catch = base.catch.y.tot |> 
      dplyr::left_join(scenario_output, by = c('Time.y' = 'Time')) |> 
      dplyr::mutate(catch.new = (catch.annual.mgN.s * subgroup_weight * current_scale),
                    catch.scale = current_scale,
                    Time.d = Time.y * 365) |>
      dplyr::left_join(base.catch.y, by = c('Time.y',"SubGroup" = "Variable"))
    
    scenario_config.ls[[i]] = new.catch |> 
      dplyr::mutate(run.id = scenario_params$run.id[i]) |> 
      dplyr::left_join(scenario_params)
    
    #test that scalar applied overall and stop if fails
    new.catch.test = new.catch |>
      dplyr::group_by(Time.y,Group) |>
      dplyr::summarise(group.Total = sum(catch.orig.mgN.s, na.rm = T),
                       group.NewTotal = sum(catch.new,na.rm=T), # Need *365 here to test against annual total
                       group_weight = mean(group_weight)) |>
      dplyr::group_by(Time.y) |>
      dplyr::mutate(Total = sum(group.Total,na.rm=T),
                    NewTotal = sum(group.NewTotal,na.rm=T)) |>
      dplyr::mutate(old.group_weight = group.Total/Total,
                    desiredTotal = Total * current_scale * group_weight,
                    diff.tot = (desiredTotal - group.NewTotal)/desiredTotal,
                    diff.weight = group_weight - old.group_weight)
    
    if(new.catch.test$diff.tot |> abs() |> max(na.rm=T) > 0.01){
      message('Warning: Total catch by group does not match expected total')
      message(paste0('Stopped at scenario ',i))
      stop('Stopping script')
    }
    
    #Test if new catch is correct scalar of original catch
    real.scalar.test = new.catch %>% 
      dplyr::group_by(Time.y) %>%
      dplyr::summarise(new.total = sum(catch.new,na.rm=T),
                       old.total = sum(catch.orig.mgN.s,na.rm=T)) %>% 
      dplyr::mutate(real.scalar = new.total/old.total,
                    scalar.diff = real.scalar - current_scale)
    
    if(real.scalar.test$scalar.diff |> abs() |> max(na.rm=T) > 0.01){
      message('Warning: Total catch by group does not match expected total')
      message(paste0('Stopped at scenario ',i))
      stop('Stopping script')
    }
    #   
    # new.catch = new.catch |> 
    #   dplyr::select(Time.y,SubGroup,catch.new)
    
    #### Apply new catch forcing file and write out
    #Create new catch TS files
    new.catch.d = base.catch.d |>  
      dplyr::mutate(Time.y = floor(Time.d/365)) |> 
      dplyr::left_join(dplyr::select(new.catch,Time.y,SubGroup,catch.new), by = c('Time.y', 'Variable' = 'SubGroup')) |> 
      dplyr::select(Time.d, Variable, catch.new)
    
    real.catch.d.test = base.catch.d %>% 
      dplyr::left_join(new.catch.d) %>% 
      dplyr::group_by(Time.d) %>% 
      dplyr::summarise(old.catch = sum(catch.orig.mgN.s,na.rm=T),
                      new.catch =sum(catch.new,na.rm=T)) %>% 
      dplyr::mutate(real.scalar = new.catch/old.catch,
                    scalar.diff = real.scalar - current_scale)
    
    # 2. Join, filter, and calculate differences
    validation_df <- test_data |> 
      dplyr::inner_join(new.catch.d, by = c("Time.d", "Variable")) |>
      dplyr::mutate(Time.y = floor(Time.d / 365)) |>
      # Bring in our flags
      dplyr::left_join(constant_flags, by = c("Time.y", "Variable")) |>
      # Exclude transition years where catch is not constant!
      dplyr::filter(is_constant == TRUE) |> 
      # Calculate relative difference
      dplyr::mutate(diff = abs(catch.written - catch.new),
                    rel_diff = ifelse(catch.new == 0, diff, diff / catch.new))
    
    # Determine the maximum difference ONLY on the constant years
    max_diff <- max(validation_df$rel_diff, na.rm = TRUE)
    
    if (max_diff > 1e-4) {
      stop(sprintf("ERROR: Validation failed for Scenario %d! Max relative difference is %f. The written .ts file does not match the expected scalars.", i, max_diff))
    } else {
      message(sprintf("Success: Scenario %d TS file passed validation (Max diff: %e).", i, max_diff))
    }
    
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
      
      # --- START OF VALIDATION CHECK ---
      message(sprintf("Validating generated TS file for scenario %d...", i))
      
      # Read the newly written file using the updated get_forcing_ts function
      test_data <- get_forcing_ts(file_path = catch.file.new, code = NULL, time = 'day') |> 
        dplyr::rename(catch.written = 'Value',
                      Time.d = 'Time')
      
      # Join with our expected values and calculate the difference
      validation_df <- test_data |> 
        dplyr::inner_join(new.catch.d, by = c("Time.d", "Variable")) |>
        # Calculate relative difference, handling 0s gracefully
        dplyr::mutate(diff = abs(catch.written - catch.new),
                      rel_diff = ifelse(catch.new == 0, diff, diff / catch.new))
      
      # Determine the maximum difference
      max_diff <- max(validation_df$rel_diff, na.rm = TRUE)
      
      if (max_diff > 1e-4) {
        stop(sprintf("ERROR: Validation failed for Scenario %d! Max relative difference is %f. The written .ts file does not match the expected scalars.", i, max_diff))
      } else {
        message(sprintf("Success: Scenario %d TS file passed validation (Max diff: %e).", i, max_diff))
      }
      # --- END OF VALIDATION CHECK ---
      
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

if(existing.setup){
  scenario_params =read.csv(paste0(proj.dir,'Setup_Files/',experiment.id,'_setup.csv'))
}else{
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
  
}


if(do.run){
  system('sudo chmod -R 775 *')
  system('sudo chmod -R 775 /atlantisdisk2/')
  

  base.sbatch.array = paste0(proj.dir,'currentVersion/sbatch_scenario_array_base.sh')
  new.sbatch.array =  paste0(proj.dir,'currentVersion/sbatch_',experiment.id,'.sh')
  file.copy(base.sbatch.array,new.sbatch.array,overwrite = T)
  
  #replace max array number
  sbatch.lines = readLines(new.sbatch.array)
  # new.nodes.line = paste0('#SBATCH --nodes=',ceiling(nrow(scenario_params)/64))
  new.nodes.line = paste0('#SBATCH --nodes=1')
  new.array.line = paste0('#SBATCH --array=1-',nrow(scenario_params))
  # new.array.line = paste0('#SBATCH --array=1-2')
  sbatch.lines[grep('--array',sbatch.lines)] = new.array.line
  sbatch.lines[grep('--nodes',sbatch.lines)] = new.nodes.line
  
  #replace directories
  new.mkdir = paste0("mkdir -p /atlantisdisk2/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID")
  sbatch.lines[grep('mkdir',sbatch.lines)] = new.mkdir
  
  new.singularity = paste0( "singularity exec --bind ",proj.dir,"currentVersion:/app/model,/atlantisdisk2/",experiment.id,"/",experiment.id,"_$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/runAtlantis_$SLURM_ARRAY_TASK_ID.sh")
  sbatch.lines[grep('singularity',sbatch.lines)] = new.singularity
  
  writeLines(sbatch.lines,new.sbatch.array)
  
  # system("find . -name "*.sh" -exec chmod +x {} \;")
  batch.string = paste0("sbatch ",new.sbatch.array)
  system(batch.string)
}
