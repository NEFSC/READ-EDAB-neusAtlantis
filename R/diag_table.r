library(atlantisdiagnostics)
library(dplyr)
library(here)

# Declare paths to files required

priority_data <- read.csv(paste0(here::here(),'/Setup_Files/neus_atlantis_group_priority.csv'))
priority_data <- arrange(priority_data,Overall,LongName)

priority_data <- filter(priority_data, Code != "REP")
priority_data <- filter(priority_data, Code != "RCB")

high_priority_group_table <- filter(select(priority_data,Code,LongName,Overall),Overall == "H")
num_priority_plots <- nrow(high_priority_group_table)
high_priority_groups <- high_priority_group_table$Code
all_groups <- priority_data$Code

biomind <- paste0(here::here(),"/Setup_Files/neus_outputBiomIndx.txt")
fgs <- paste0(here::here(),"/currentVersion/neus_groups.csv")

# Perform stability test on all species/groups using the last 20 years of the run
stability_table <- diag_stability(fgs, biomind, speciesCodes=all_groups, nYrs = 20)

reasonability_data <- read.csv(paste0(here::here(), '/Setup_Files/output_diag_reasonability.csv'))
catchfile <- paste0(here::here(),'/Setup_Files/neus_outputCatch.txt')

#Make biomass timeseries plots
biomass = read.table(biomind, header=TRUE)
biomass <- select(biomass,!contains("Rel"))
catch = read.table(catchfile, header=TRUE)
catch <- select(catch,!contains("TsAct"))
catch <- filter(catch,Time %% 365 == 0)
catch_l20 <- tail(catch,20)

biom_l20 <- filter(biomass,Time %% 365 == 0)
biom_l20 <- tail(biom_l20,20)

reasonability_groups <- reasonability_data$Code
reasonability_data <- mutate(reasonability_data,Reasonable = NA)
reasonability_data <- mutate(reasonability_data,Average_Catch = 0)
reasonability_data <- mutate(reasonability_data,F_rate = 0)
for(i in 1:nrow(reasonability_data)) {
  code <- reasonability_data$Code[i]
  code_biomass <- select(biom_l20,matches(code))
  min_biomass <- min(code_biomass)
  max_biomass <- max(code_biomass)
  if ((min_biomass < reasonability_data$Min_Target_q[i]) || (max_biomass > reasonability_data$Max_Target_q[i])) {
    reasonability_data$Reasonable[i] <- FALSE
  } else {
    reasonability_data$Reasonable[i] <- TRUE
  }
  code_catch <- select(catch_l20,matches(code))
  avg_biomass <- mean(code_biomass[[1]])
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

# Clean up number formatting
options(scipen=999)
diagnostic_table$Relative_Change <- as.numeric(format(round(diagnostic_table$Relative_Change, 3), nsmall = 3))
diagnostic_table$F_rate[is.na(diagnostic_table$F_rate)] <- 0
diagnostic_table$F_rate <- as.numeric(format(round(diagnostic_table$F_rate, 3), nsmall = 3))
diagnostic_table$Average_Biomass <- diagnostic_table$Average_Biomass / 100000
diagnostic_table$Average_Catch <- diagnostic_table$Average_Catch / 100000
diagnostic_table$Average_Biomass <- as.numeric(format(round(diagnostic_table$Average_Biomass,2), nsmall = 2))
diagnostic_table$Average_Catch[is.na(diagnostic_table$Average_Catch)] <- 0
diagnostic_table$Average_Catch <- as.numeric(format(round(diagnostic_table$Average_Catch,2), nsmall = 2))


outFile <- paste0(here::here(),"/Manuscript/Tables/diagnostic_table.csv")
write.csv(diagnostic_table, file=outFile, row.names=FALSE)


