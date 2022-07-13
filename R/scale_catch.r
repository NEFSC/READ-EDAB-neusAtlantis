library(dplyr)

scale_catch <- function(original_catch, filename) {
  
  catch_new <- original_catch
  group_scalars <- read.csv(filename, sep = ",", stringsAsFactors = FALSE, header=TRUE)
  
  if (group_scalars$Group[1] == "ALL") {
    catch_new[,2:90] <- original_catch[,2:90] * group_scalars$Scalar[1]
  } else {
    numRows <- nrow(group_scalars)
    for (i in 1:numRows) {
      groupName <- group_scalars$Group[i]
      scalar <- group_scalars$Scalar[i]
      catch_new[[groupName]] <- original_catch[[groupName]] * scalar
    }
  }
  
  return(catch_new)
}

setwd("C:/Users/robert.gamble/Desktop/Atlantis_1_5/neus-atlantis/currentVersion/CatchFiles")
catchFile <- "total_catch.ts"

original_catch_file <- read.csv(catchFile, sep = " ", stringsAsFactors = FALSE, header = FALSE)
original_catch_header <- original_catch_file[c(1:454),]
original_catch_header[is.na(original_catch_header)] <- ""
original_catch <- original_catch_file[-c(1:454),]
original_catch[] <- lapply(original_catch, as.numeric)

colnames(original_catch) <- c("Time", "MAK", "HER", "WHK", "BLF", "WPF", "SUF", "WIF", "WTF", "FOU", "HAL", "PLA", "FLA", "BFT", "TUN",
                              "BIL", "MPF", "BUT", "BPF", "ANC", "GOO", "MEN", "FDE", "COD", "SHK", "OHK", "POL", "RHK", "BSB", "SCU",
                              "TYL", "RED", "OPT", "SAL", "DRM", "STB", "TAU", "WOL", "SDF", "FDF", "HAD", "YTF", "DOG", "SMO", "SSH", 
                              "DSH", "BLS", "POR", "PSH", "WSK", "LSK", "SK", "SB", "PIN", "REP", "RWH", "BWH", "SWH", "TWH", "INV", 
                              "LSQ", "ISQ", "SCA", "QHG", "CLA", "BFF", "BG", "LOB", "RCB", "BMS", "NSH", "OSH", "ZL", "BD", "MA", 
                              "MB", "SG", "BC", "ZG", "PL", "DF", "PS", "ZM", "ZS", "PB", "BB", "BO", "DL", "DR", "DC")

# Example 1
# The scaling file has two columns, Group and Scalar (sep = ",")
# Three groups (WHK, MAK, SUF)
# Three scalars (0.01, 0.01, 10)
# Outcome: WHK and MAK have their catch multiplied by 0.01, SUF has its catch multiplied by 10

scale_catch_file_some = "C:/Users/robert.gamble/Desktop/Atlantis_1_5/neus-atlantis/currentVersion/scale_catch_1.csv"
scaled_catch_some <- scale_catch(original_catch, scale_catch_file_some)
outputFile_some <- "test_catch_some.ts"

write.table(original_catch_header, file = outputFile_some, append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(scaled_catch_some, file = outputFile_some, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )

# Example 2
# The scaling file has two columns, Group and Scalar (sep = ",")
# One group (ALL)
# One scalar (0.01)
# Outcome: All groups have their catch multiplied by 0.01

scale_catch_file_all = "C:/Users/robert.gamble/Desktop/Atlantis_1_5/neus-atlantis/currentVersion/scale_catch_2.csv"
scaled_catch_all <- scale_catch(original_catch, scale_catch_file_all)
outputFile_all <- "test_catch_all.ts"

write.table(original_catch_header, file = outputFile_all, append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(scaled_catch_all, file = outputFile_all, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )
