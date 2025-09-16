#redistributes the effort by port for a particular fishery/fleet
library(dplyr)
project.dir = here::here("")

experiment.name = 'GF_effort_redist'
#source editing files
source(paste0(project.dir, 'R/Forcing_Fishing/edit_forcing_ts.r'))
source(paste0(project.dir, 'R/Forcing_Fishing/get_forcing_ts.r'))

#Read fleets data and identify groundfish fleets

fleet.file = paste0(project.dir,'currentVersion/neus_fisheries.csv')

fleet.def = read.csv(fleet.file)
sca.fleets = grep('^gf*',fleet.def$Code,value =T)
target.fleet = 'gfgloucester'
target.prop = 0.32

#Get effort files
effort.dir= paste0(project.dir,'currentVersion/CatchFiles/')
effort.file.name = list.files(effort.dir,'^effort_box.*.ts$')
effort.file.name.long = list.files(effort.dir,'^effort_box.*.ts$',full.names = T)
effort.file.name.short = gsub('.ts','',effort.file.name)
effort.file.box = gsub("^effort_box(\\d+)\\.ts$","\\1",effort.file.name)

new.effort.dir = paste0(project.dir,'currentVersion/CatchFiles/',experiment.name,'/')
if(!dir.exists(new.effort.dir)){
  dir.create(new.effort.dir)
}
new.file.names = paste0(effort.file.name.short,'_',experiment.name)
file.copy(effort.file.name.long, paste0(effort.dir,new.file.names,'.ts'))

#Get existing port allocation across this fishery from TS files
f=b=1
orig.effort.ls = vector(mode = 'list',length = length(sca.fleets)*length(effort.file.name))
ind.ls = 1
for(f in 1:length(sca.fleets)){
  for(b in 1:length(effort.file.name)){
    
    orig.effort.ls[[ind.ls]] = get_forcing_ts(sca.fleets[f],filenm = effort.file.name.short[b],time = 'daily') %>% 
      dplyr::mutate(box = effort.file.box[b])
    ind.ls = ind.ls +1
    
  }
}
orig.effort.df = dplyr::bind_rows(orig.effort.ls) %>% 
  dplyr::rename(fleet='Variable',
                effort = 'Value')

#find original proportion
orig.effort.df %>% 
  dplyr::filter(effort > 0) %>% 
  dplyr::group_by(fleet) %>% 
  dplyr::summarise(effort = mean(effort,na.rm=T)) %>% 
  dplyr::mutate(effort.tot = sum(effort)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(effort.prop= effort/effort.tot)
# -----------------------------------------------------------
# Section 3: Perform Reproportioning at Each Time Step
# -----------------------------------------------------------
# Create a new data frame to store the scenario results
scenario.effort.df <- orig.effort.df %>%
  filter(effort>0) %>% 
  # Group the data by each time step
  group_by(Time, fleet) %>%
  mutate(
    # The total effort for the current fleet in the current time step is just the sum of the effort column
    # because of the `group_by(Time, fleet)` call.
    total_fleet_effort_T = sum(effort)
  ) %>%
  group_by(Time) %>%
  mutate(
    # Calculate total effort for the current time step
    total_effort_T = sum(effort),
    # Calculate new total effort for the target fishery at this time step
    new_effort_A_T = total_effort_T * target.prop,
    
    # Calculate the remaining effort for non-A fisheries at this time step
    remaining_effort_T = total_effort_T - new_effort_A_T,
    
    # Calculate original total effort for non-A fisheries at this time step
    original_effort_NA_T = sum(effort[fleet != target.fleet]),
    
    # Calculate the scaling factor for non-A fisheries
    scaling_factor_NA_T = if_else(original_effort_NA_T > 0, remaining_effort_T / original_effort_NA_T, 0)
  ) %>%
  ungroup() %>% 
  
  # Now, apply the scaling based on the fleet and time step
  mutate(
    new_effort = if_else(
      fleet == target.fleet,
      # For the target fishery, scale each box's effort to maintain its proportion
      effort * (new_effort_A_T / total_fleet_effort_T),
      # For non-A fisheries, apply the scaling factor calculated for that time step
      effort * scaling_factor_NA_T
    )
  ) %>%
  # Select and rename the final columns
  select(Time, fleet, box, effort = new_effort) 
  
scenario.effort.df %>% 
  dplyr::filter(effort > 0) %>% 
  dplyr::group_by(fleet) %>% 
  dplyr::summarise(effort = mean(effort,na.rm=T)) %>% 
  dplyr::mutate(effort.tot = sum(effort)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(effort.prop= effort/effort.tot)

# Loop through each effort box
for(i in 1:length(effort.file.name)){
  
  this.box = as.numeric(effort.file.box[i])
  this.box.effort = filter(scenario.effort.df, box == this.box)
  if(nrow(this.box.effort) == 0){
    next()
  }
  
  which.fleets = sort(unique(this.box.effort$fleet))
  
  for(j in 1:length(which.fleets)){
    
    this.fleet.effort = filter(this.box.effort, fleet == which.fleets[j]) %>% 
      select(Time,effort) %>% 
      as.matrix()
    
    edit_forcing_ts(code = which.fleets[j],
                    tstype = 'effort',
                    trange = this.fleet.effort,
                    filename = new.file.names[i],
                    keep =F)
  }
  
}

#Clean up and move files to new dir
file.copy(paste0(effort.dir,new.file.names,'.ts'), paste0(new.effort.dir),overwrite = T)
file.remove(paste0(effort.dir,new.file.names,'.ts'))
file.remove(list.files(effort.dir,'*.tstemp', full.names = T))

#Edit the force.prm
force.prm.file = paste0(project.dir, 'currentVersion/at_force_LINUX.prm')
force.lines = readLines(force.prm.file)
effort.file.line = grep("Effortts[0-9]+\\.data\\s+CatchFiles/effort_box[0-9]+\\.ts",force.lines)
effort.str.orig = force.lines[effort.file.line]
effort.pattern <- "Effortts([0-9]+)\\.data\\s+CatchFiles/effort_box([0-9]+)\\.ts"
effort.replacement <- paste0("Effortts\\1.data CatchFiles/", experiment.name, "/effort_box\\2_", experiment.name, ".ts")
new.effort.str <- sub(effort.pattern, effort.replacement, effort.str.orig)

force.lines[effort.file.line] = new.effort.str
writeLines(force.lines,force.prm.file)
