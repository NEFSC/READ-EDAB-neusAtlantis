# Data file (sep = ',') follows the following format:

# Col 1: 'Group' - Atlantis group name, e.g HER, MAK, etc.  If the first row says "ALL", the changes will be applied to all groups
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
