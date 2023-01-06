
library(here)
library(tidyverse)
library(forecast)
library(Kendall)

#Check the range of relative slopes in survdat data
#Stability = relativized slope of last x years of timeseries

TS_LENGTH = 20
reference.dir = here('Atlantis_Runs','Dev_11032022')
output.dir <- here('Atlantis_Runs','Dev_11032022','Post_Processed')

#Read in survdat
data = readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>%
  dplyr::filter(variable %in% c("tot.biomass")) %>%
  dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
  dplyr::select(-units)%>%
  select(YEAR,value,Code)%>%
  group_by(Code,YEAR)%>%
  summarise(value = sum(value,na.rm=T) )

data_var = readRDS(here::here('data',"sweptAreaBiomassNEUS.rds")) %>% 
  dplyr::filter(variable %in% c("tot.bio.var"))%>%
  dplyr::mutate(value=ifelse(grepl("kg\\^2$",units),value/1e6,value)) %>%
  dplyr::select(-units) %>%
  select(YEAR,value,Code)%>%
  rename(value.var = value)

data = left_join(data,data_var)%>%
  mutate(value.min = value - 3*sqrt(value.var),
         value.min = ifelse(value.min < 0, 0, value.min),
         value.max = value + 3*sqrt(value.var))

#Read in q's, Set spp with assessment-based estimates to q=1
q_data <- readRDS(here::here('data',"emax_qs.rds"))
q_data <- mutate(q_data,Avg.q = (Fall.q + Spring.q) / 2)
q_data <- mutate(q_data,Avg.q = ifelse(Code %in% c('BLF','MEN','SUF','STB'),1,Avg.q))
q_data <- select(q_data,Code,Avg.q)
q_data <- distinct(q_data)
q_data <- group_by(q_data,Code)
q_data <- summarise(q_data,Avg.q = mean(Avg.q))


#Adjusted for q and aggregate by atlantis group
data.obs.lim = data %>%
  left_join(q_data)%>%
  mutate(value.min.q = value.min/Avg.q,
         value.max.q = value.max/Avg.q)%>%
  group_by(Code,YEAR)%>%
  summarise(value.min.q = sum(value.min.q),
            value.max.q = sum(value.max.q)) %>%
  group_by(Code)%>%
  summarise(obs.min = min(value.min.q),
            obs.max = max(value.max.q))


#Find the absolute min/max for each species

#Read in atlantis data
biom_atl <- read.csv(paste0(reference.dir,'/neus_outputBiomIndx.txt'),sep = ' ')

groups = unique(data$Code)
numGroups <- length(groups)

reasonability_df <- data.frame(matrix(ncol = 17, nrow = numGroups))
cNames <- c("Group", "Initial_Magnitude", "Magnitude", "Trend", "Initial_Bio", "Mean_Bio", "Min_Target", "Max_Target", "Scalar_Below_Min_Initial", "Scalar_Above_Max_Initial", "Below_Min", "Above_Max", "Outside_Range", "Scalar_Below_Min", "Scalar_Above_Max", "Trend_Survdat", "Trend_Atlantis")
colnames(reasonability_df) <- cNames

atl_groupnames <- colnames(biom_atl)
for(i in 1:numGroups) {
  
  groupCode <- groups[i]
  # q_data_for_group <- filter(q_data,Code == groupCode)
  # group_bio_data <- filter(data,Code == groupCode)
  # group_sd <- sd(group_bio_data$value)
  # q <- q_data_for_group$Avg.q[1]
  if (groupCode %in% atl_groupnames) {
    
    groupData_obs = filter(data.obs.lim, Code == groupCode)
    # groupData_obs <- filter(data,Code == groupCode)

    # minBiom_Obs <- min((groupData_obs$value - 3 * sqrt(groupData_obs$value.var)) / q)
    minBiom_Obs = groupData_obs$obs.min[1]
    
    # maxBiom_Obs <- max((groupData_obs$value + 3 * sqrt(groupData_obs$value.var)) / q)
    maxBiom_Obs = groupData_obs$obs.max[1]

    groupData_atl_full <- select(biom_atl,(contains(groupCode) & !contains("rel")))
    groupData_atl <- tail(groupData_atl_full, n=TS_LENGTH)

    pct_BelowMin <- 0
    pct_AboveMax <- 0
    pct_BelowMinInitial <- 0
    pct_AboveMaxInitial <- 0

    minBiom_atl <- min(groupData_atl[,1])
    maxBiom_atl <- max(groupData_atl[,1])
    avgBiom_atl <- mean(groupData_atl[,1])
    initBiom_atl <- groupData_atl_full[1,1]

    magnitude_initial <- "TRUE"
    magnitude <- "TRUE"
    # if (is.na(group_sd)) {
    #   magnitude_initial <- "UNKNOWN"
    #   magnitude <- "UNKNOWN"
    # } else {
      if (avgBiom_atl < minBiom_Obs) {
        pct_BelowMin <- minBiom_atl / minBiom_Obs
        magnitude <- "FALSE"
      }
      
      if (avgBiom_atl > maxBiom_Obs) {
        pct_AboveMax <- maxBiom_atl / maxBiom_Obs
        magnitude <- "FALSE"
      }
      
      if (initBiom_atl < minBiom_Obs) {
        magnitude_initial <- "FALSE"
        pct_BelowMinInitial <- initBiom_atl / minBiom_Obs
      }
      
      if (initBiom_atl > maxBiom_Obs) {
        magnitude_initial <- "FALSE"
        pct_AboveMaxInitial <- initBiom_atl / maxBiom_Obs
      }
      
      
    # }
    
    numBelowMin <- 0
    numAboveMax <- 0
    numOutsideRange <- 0
    numBelowMin <- 0
    numAboveMax <- 0
    numOutsideRange <- 0
    
    length_TS_atl <- nrow(groupData_atl)
    # if (!is.na(group_sd)) {
      for (j in 1:length_TS_atl) {
        if (groupData_atl[j,1] < minBiom_Obs) {
          numBelowMin <- numBelowMin + 1
        } else if (groupData_atl[j,1] > maxBiom_Obs) {
          numAboveMax <- numAboveMax + 1
        }
      }
    # }
#    if (minBiom_Obs < 0) {
#      minBiom_Obs <- 0
#    }
    numOutsideRange <- numBelowMin + numAboveMax
    print(groupCode)
    reasonability_df$Group[i] <- groupCode
    reasonability_df$Initial_Bio[i] <- initBiom_atl
    reasonability_df$Mean_Bio[i] <- avgBiom_atl
    reasonability_df$Initial_Magnitude[i] <- magnitude_initial
    reasonability_df$Magnitude[i] <- magnitude
    reasonability_df$Min_Target[i] <- minBiom_Obs
    reasonability_df$Max_Target[i] <- maxBiom_Obs
    reasonability_df$Scalar_Below_Min[i] <- pct_BelowMin
    reasonability_df$Scalar_Above_Max[i] <- pct_AboveMax
    reasonability_df$Scalar_Below_Min_Initial[i] <- pct_BelowMinInitial
    reasonability_df$Scalar_Above_Max_Initial[i] <- pct_AboveMaxInitial
    reasonability_df$Below_Min[i] <- numBelowMin
    reasonability_df$Above_Max[i] <- numAboveMax
    reasonability_df$Outside_Range[i] <- numOutsideRange
    
  }
}
reasonability_df <- filter(reasonability_df, !is.na(Group))

# Compare trends
rows_in_reasonability_df <- nrow(reasonability_df)
for(i in 1:rows_in_reasonability_df) {
  groupCode <- reasonability_df$Group[i]
  print(groupCode)
  
  groupData_obs <- filter(data,Code == groupCode)

  groupData_atl <- select(biom_atl,(contains(groupCode) & !contains("rel")))
  groupData_atl <- tail(groupData_atl, n=TS_LENGTH)
    
  groupData_obs <- tail(groupData_obs, n = TS_LENGTH)

  avgBiom_obs <- mean(groupData_obs$value)
  avgBiom_atl <- mean(groupData_atl[,1])
    
    
  length_obs_ts <- nrow(groupData_obs)
    
  trend <- "UNAVAILABLE"

  ts_atl <- ts(groupData_atl[,1], start = c(1, TS_LENGTH), frequency = 1)
    
  fit_atl <- tslm(ts_atl ~ trend)
    
  coef_atl <- fit_atl$coefficients
    
  trend_atl <- coef_atl[[2]]
  trend_MK_atl <- MannKendall(ts_atl)
  sig_atl <- trend_MK_atl[2]$sl[1]
  if (sig_atl < 0.05) {
    if (trend_atl < 0) {
      trend_type_atl <- "-"
    } else if (trend_atl > 0) {
      trend_type_atl <- "+"
    }
  } else {
    trend_type_atl <- "0"
  }
  trend_obs <- 0
    
  if (!(length_obs_ts < 3)) {
    ts_obs <- ts(groupData_obs$value, start = c(1, TS_LENGTH), frequency = 1)
    fit_obs <- tslm(ts_obs ~ trend)
    coef_obs <- fit_obs$coefficients
    trend_obs <- coef_obs[[2]]
      
    trend_MK_obs <- MannKendall(ts_obs)
      
    sig_obs <- trend_MK_obs[2]$sl[1]
      
    trend <- "Unknown"
    if (sig_obs < 0.05) {
      if (trend_obs < 0) {
        trend_type_obs <- "-"
      } else if (trend_obs > 0) {
        trend_type_obs <- "+"
      }
    } else {
      trend_type_obs <- "0"
    }
    print(trend_type_obs)
      

    if (trend_type_atl == trend_type_obs) {
      trend <- "TRUE"
    } else {
      trend <- "FALSE"
    }
  }

    
    reasonability_df$Trend[i] <- trend
    reasonability_df$Trend_Survdat[i] <- trend_type_obs 
    reasonability_df$Trend_Atlantis[i] <- trend_type_atl
    
}


reasonability_df <- filter(reasonability_df, !is.na(Group))

write.table(reasonability_df, file = paste0(output.dir, '/output_diag_reasonability.csv'), row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")

