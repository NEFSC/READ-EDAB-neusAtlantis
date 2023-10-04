library(atlantisdiagnostics)
library(dplyr)
library(here)

# Declare paths to files required

priority_data <- read.csv(paste0(here::here(),'/diagnostics/neus_atlantis_group_priority.csv'))
priority_data <- rename(priority_data,Code = 'code')
priority_data <- rename(priority_data,Overall = 'priority.overall')
priority_data <- arrange(priority_data,Overall,LongName)

priority_data <- filter(priority_data, Code != "REP")
priority_data <- filter(priority_data, Code != "RCB")

high_priority_group_table <- filter(select(priority_data,Code,LongName,Overall),Overall == "H")
num_priority_plots <- nrow(high_priority_group_table)
high_priority_groups <- high_priority_group_table$Code
all_groups <- priority_data$Code

base.run.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/'
biomind <- paste0(base.run.dir,'neus_outputBiomIndx.txt')
fgs <- paste0(here::here(),"/currentVersion/neus_groups.csv")

# Perform stability test on all species/groups using the last 20 years of the run
stability_table <- diag_stability(fgs, biomind, speciesCodes=all_groups, nYrs = 20,relChangeThreshold = 0.05)

reasonability_data <- read.csv(paste0(here::here(), '/data/output_diag_reasonability.csv'))
catchfile <- paste0(base.run.dir,'neus_outputCatch.txt')

#Make biomass timeseries plots
biomass = read.table(biomind, header=TRUE)
biomass <- select(biomass,!contains("Rel"))
catch = read.table(catchfile, header=TRUE)
catch <- select(catch,!contains("TsAct"))

time.stop = max(catch$Time)
time.start = time.stop - (20*365)

catch_l20 <- catch %>%
  filter(Time >= time.start & Time <= time.stop)

biom_l20 = biomass %>%
  filter(Time >= time.start & Time <= time.stop)

reasonability_groups <- reasonability_data$Code
reasonability_data <- mutate(reasonability_data,Reasonable = NA)
reasonability_data <- mutate(reasonability_data,Average_Catch = 0)
reasonability_data <- mutate(reasonability_data,F_rate = 0)
for(i in 1:nrow(reasonability_data)) {
  code <- reasonability_data$Code[i]
  code_biomass <- select(biom_l20,matches(code))
  min_biomass <- min(code_biomass)
  max_biomass <- max(code_biomass)
  avg_biomass <- mean(code_biomass[[1]])
  if ((avg_biomass < reasonability_data$Min_Target_q[i]) || (avg_biomass > reasonability_data$Max_Target_q[i])) {
    reasonability_data$Reasonable[i] <- FALSE
  } else {
    reasonability_data$Reasonable[i] <- TRUE
  }
  code_catch <- select(catch_l20,matches(code))
  avg_catch <- mean(code_catch[[1]])
  F_rate <- avg_catch / avg_biomass
  reasonability_data$Average_Catch[i] <- avg_catch
  reasonability_data$F_rate[i] <- F_rate
}

reasonability_data <- full_join(reasonability_data,priority_data, by="Code")
reasonability_data <- rename(reasonability_data,Priority = Overall)
stability_table <- select(stability_table,code,aveBio,pass,relChange)
stability_table <- rename(stability_table, Code = code, Average_Biomass= aveBio, Stable = pass, Relative_Change = relChange)

diagnostic_table <- full_join(stability_table, reasonability_data, by="Code")
diagnostic_table <- select(diagnostic_table,Code,Priority,Stable,Reasonable,Relative_Change,Min_Target_q,Max_Target_q,Average_Biomass,Average_Catch, F_rate)
diagnostic_table = diagnostic_table %>%
  mutate(Min_Target_q = Min_Target_q / 1E5,
        Max_Target_q = Max_Target_q / 1E5)

# Clean up number formatting
# options(scipen=999)
diagnostic_table$Relative_Change <- as.numeric(format(round(diagnostic_table$Relative_Change, 3), nsmall = 3))
diagnostic_table$F_rate[is.na(diagnostic_table$F_rate)] <- 0
diagnostic_table$F_rate <- round(diagnostic_table$F_rate, 2)
diagnostic_table$Average_Biomass <- diagnostic_table$Average_Biomass / 100000
diagnostic_table$Average_Catch <- diagnostic_table$Average_Catch / 100000
diagnostic_table$Average_Biomass <- round(diagnostic_table$Average_Biomass,2)
diagnostic_table$Average_Catch[is.na(diagnostic_table$Average_Catch)] <- 0
diagnostic_table$Average_Catch <- round(diagnostic_table$Average_Catch,2)

reasonable = diagnostic_table %>%
  select(Code,Reasonable)%>%
  filter(!is.na(Reasonable))%>%
  mutate(count = n())%>%
  group_by(Reasonable)%>%
  summarise(Reasonable.n = n(),
            count = mean(count))%>%
  mutate(Reasonable.pct = Reasonable.n/count)

stable = diagnostic_table %>%
  select(Code,Stable)%>%
  filter(!is.na(Stable))%>%
  mutate(count = n())%>%
  group_by(Stable)%>%
  summarise(stable.n = n(),
            count = mean(count))%>%
  mutate(stable.pct = stable.n/count)

reasonable.pct.over = diagnostic_table %>%
  select(Code,Reasonable, Min_Target_q: Average_Biomass)%>%
  filter(Reasonable == F)%>%
  mutate(is.over = Average_Biomass > Max_Target_q)%>%
  filter(is.over == T)%>%
  mutate(pct.over = Average_Biomass/Max_Target_q)

stability.pct.over =diagnostic_table %>%
  select(Code,Stable,Relative_Change)%>%
  filter(Stable == F)%>%
  mutate(pct.over = Relative_Change/ 0.05,
         is.pos = ifelse(Relative_Change>0,T,F))%>%
  group_by(is.pos)%>%
  summarise(mean.rel.change = mean(Relative_Change,na.rm=T))

#Format for print  
diagnostic_table$Average_Biomass=sapply(diagnostic_table$Average_Biomass,function(x) return(ifelse(x<0.01,'<0.01',x)))
diagnostic_table$Average_Catch=sapply(diagnostic_table$Average_Catch,function(x) return(ifelse(x<0.01,'<0.01',x)))
diagnostic_table$F_rate=sapply(diagnostic_table$F_rate,function(x) return(ifelse(x<0.01,'<0.01',x)))

out.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/tables/'
outFile <- paste0(out.dir,"table_1_species_metrics.csv")
write.csv(diagnostic_table, file=outFile, row.names=FALSE)
