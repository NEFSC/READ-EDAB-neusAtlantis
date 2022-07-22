library(dplyr)
library(here)

# Data file (sep = ',') follows the following format:

# Col 1: 'Group' - Atlantis group name, e.g HER, MAK, etc
# Col 2: 'Start_Time' - Start time for the change (in days since start of run, can be 0)
# Col 3: 'End_Time' - End time for the change (in days since start of run, can be 0)
# Col 4: 'Change' - Scalar/Replacement of original catch for a group in the start and end times specified
# Col 5: 'Type' - 'Scalar' or 'Replace'.  If Scalar, multiply Change by original catch.  If Change, replace original catch

scale_catch <- function(original_catch, filename) {
  
  catch_new <- original_catch
  group_changes <- read.csv(filename, sep = ",", stringsAsFactors = FALSE, header=TRUE)
  
    if (group_changes$Group[1] == "ALL") {
      start_time <- group_changes$Start_Time[1] + 1
      end_time <- group_changes$End_Time[1] + 1 
      if (group_changes$Type == "Scalar") {
        catch_new[start_time:end_time,2:90] <- original_catch[start_time:end_time,2:90] * group_changes$Change[1]
      } else if (group_changes$Type == "Replace") {
        catch_new[start_time:end_time,2:90] <- group_changes$Change[1]
      }
    } else {
      numRows <- nrow(group_changes)
      for (i in 1:numRows) {
        start_time <- group_changes$Start_Time[i] + 1
        end_time <- group_changes$End_Time[i] + 1 
        groupName <- group_changes$Group[i]
        print(groupName)
        scalar <- group_changes$Change[i]
        if (group_changes$Type[i] == "Scalar") {
          catch_new[[groupName]][start_time:end_time] <- original_catch[[groupName]] * scalar
        } else if (group_changes$Type[i] == "Replace") {
          catch_new[[groupName]][start_time:end_time] <- scalar
        } 
      }
    }
  return(catch_new)
}

atl.dir <- getwd()
catch.dir <- here::here("currentVersion/CatchFiles/")

# Name of catch file you want to modify - place '/' in front
catchFileName <- "/total_catch.ts"
dataFilename_some <- "/scale_catch_1.csv"
outputFileName <- "/test_catch_some.ts"

catchFile <- paste0(catch.dir, catchFileName)

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

# -------------------------- Example 1 -----------------------------

data.dir <- paste0(atl.dir, "/currentVersion/CatchFiles/")
out.dir <- paste0(atl.dir,"/currentVersion/CatchFiles/")


# Change data file to what you need it to be - include a '/' at the start
dataFile_some = paste0(data.dir, dataFilename_some)
scaled_catch_some <- scale_catch(original_catch, dataFile_some)

# Change output filename to what you want it to be - add '/' at the start
outputFile_some <- paste0(out.dir, outputFileName)

write.table(original_catch_header, file = outputFile_some, append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(scaled_catch_some, file = outputFile_some, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )



# Example 2
# The scaling file has two columns, Group and Scalar (sep = ",")
# One group (ALL)
# One scalar (0.01)
# Outcome: All groups have their catch multiplied by 0.01

dataFilename_some <- "/scale_catch_2.csv"
dataFile_some = paste0(data.dir, dataFilename_some)
scaled_catch_some <- scale_catch(original_catch, dataFile_some)

# Change output filename to what you want it to be - add '/' at the start
outputFileName <- "/test_catch_all.ts"
outputFile_all <- paste0(out.dir, outputFileName)


write.table(original_catch_header, file = outputFile_all, append = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(scaled_catch_all, file = outputFile_all, append = TRUE, sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE )
