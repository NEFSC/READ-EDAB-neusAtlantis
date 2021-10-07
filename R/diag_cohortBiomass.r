library(dplyr)
setwd("C:/Users/robert.gamble/Desktop/Atlantis_1_5/neus-atlantis/currentVersion/output")

cohortBiomFile <- "neus_outputAgeBiomIndx.txt"
cohortBiom <- read.csv(cohortBiomFile,sep = " ", stringsAsFactors=FALSE, header=TRUE)

mortFile <- "neus_outputMort.txt"
mort <- read.csv(mortFile,sep = " ", stringsAsFactors=FALSE, header=TRUE)
mort <- select(mort,contains(".F"))
mort_l20 <- slice(mort,-c(1:34,55))
meanMort <- summarize_all(mort_l20,mean)

setwd("C:/Users/robert.gamble/Desktop/Atlantis_1_5/neus-atlantis/diagnostics")

neusPriority <- read.csv("neus_atlantis_group_priority.csv",sep=",",stringsAsFactors=FALSE,header=TRUE)
neusPriority <- select(neusPriority, c(Code,Priority))
numRows <- nrow(cohortBiom)
lastRow <- numRows - 1
firstRow <- numRows - 100
cohortBiom <- slice(cohortBiom,firstRow:lastRow)

Code <- c("MAK","HER","WHK","BLF","WPF","SUF","WIF","WTF", "FOU", "HAL",	"PLA",	"FLA",	"BFT",	"TUN",	"BIL",	"MPF",	"BUT",	"BPF",	"ANC",	"GOO",	"MEN",	"FDE",	"COD",	"SHK",	"OHK",	"POL",	"RHK",	"BSB",	"SCU",	"TYL",	"RED",	"OPT",	"SAL",	"DRM",	"STB",	"TAU",	"WOL",	"SDF",	"FDF",	"HAD",	"YTF",	"DOG",	"SMO")
numGroups <- length(Code)


Max_Cohort <- c()
Status <- c()
Stability <- c()

for (i in 1:numGroups) {
  groupName <- Code[i]
  groupCohort <- select(cohortBiom,contains(groupName))
  groupCohortMean <- summarise_each(groupCohort, funs(mean))
  maxCohortMean <- which.max(groupCohortMean)
  Max_Cohort <- c(Max_Cohort, maxCohortMean)
  if (maxCohortMean == 1 || maxCohortMean == 10) {
    Status <- c(Status,"FAIL")
  } else {
    Status <- c(Status, "PASS")
  }
  maxMeanIndex <- which.max(groupCohortMean)
  maxMeanIndex <- maxMeanIndex[[1]]
  stabVal <- groupCohort[100,maxMeanIndex] / groupCohort[5,maxMeanIndex]
  if (stabVal < 0.75) {
    Stability <- c(Stability,"  Declining")
  } else if (stabVal > 1.25) {
    Stability <- c(Stability, "  Increasing")
  } else {
    Stability <- c(Stability, "  Stable")
  }
}


diagnostics <- data.frame(Code,Status,Max_Cohort,Stability)
diagnostics <- inner_join(diagnostics,neusPriority,by="Code")
diagnostics$Fishing <- "4 - None"

numGroups <- nrow(diagnostics)
for (i in 1:numGroups) {
  groupCol <- select(meanMort,contains(diagnostics$Code[i]))
  print(groupCol[1,1])
  if (groupCol[1,1] >= 0.05) {
    diagnostics$Fishing[i] <- "1"
  } else if (groupCol[1,1] > 0.001) {
    diagnostics$Fishing[i] <- "2"
  } else if (groupCol[1,1] > 0.0001) {
    diagnostics$Fishing[i] <- "3"
  } else {
    diagnostics$Fishing[i] <- "4"
  }
}

diagnostics <- arrange(diagnostics,Priority,Status,Max_Cohort,Stability,Fishing,Code)
rownames(diagnostics) <-c()
diagnostics
