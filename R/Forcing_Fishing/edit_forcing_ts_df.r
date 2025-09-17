# Load necessary libraries
library(dplyr)
library(tidyr)

#' Modifies a .ts forcing file with a dataframe of new values.
#'
#' @param input_file Path to the original .ts file (e.g., "total_catch.ts").
#' @param output_file Path to write the modified .ts file to.
#' @param changes_df A data frame containing the new values.
#' @param time_col The name of the time column in changes_df (default: "Time.d").
#' @param code_col The name of the subgroup/code column in changes_df (default: "SubGroup").
#' @param value_col The name of the new value column in changes_df (default: "catch.new").
#'
#' @return Invisibly returns the path to the output file.
#' 

edit_forcing_ts_df <- function(input_file, 
                               output_file, 
                               changes_df, 
                               time_col = "Time.d", 
                               code_col = "SubGroup", 
                               value_col = "catch.new") {
  
  # --- 1. Read and Parse the Input .ts File ---
  
  # Read all lines from the file
  all_lines <- readLines(input_file)
  
  # Identify the header lines (assuming they start with '#')
  header_lines <- all_lines[startsWith(all_lines, "#")]
  
  # Find the line number where the actual data starts (the first line without '#')
  data_start_line <- length(header_lines) + 1
  
  # Read the data section of the file into a dataframe
  catch_data_wide <- read.table(input_file, skip = data_start_line - 1, header = TRUE)
  
  # The first column of the data is the timestep. Let's rename it for clarity.
  # We assume the file format is [Time, Code1, Code2, ...]
  names(catch_data_wide)[1] <- "Time"
  
  # --- 2. Reshape, Join, and Update Values ---
  
  # Rename columns in the user's dataframe for a clean join
  changes_df <- changes_df %>%
    rename(Time = all_of(time_col),
           SubGroup = all_of(code_col),
           catch.new = all_of(value_col))
  
  # Reshape the original data, join the changes, and update the values
  updated_data_long <- catch_data_wide %>%
    # Reshape from wide to long format: [Time, SubGroup, catch.orig]
    tidyr::pivot_longer(cols = -Time, names_to = "SubGroup", values_to = "catch.orig") %>%
    # Join with the dataframe of new values
    dplyr::left_join(changes_df, by = c("Time", "SubGroup")) %>%
    # Create the final value column: use the new value if it exists, otherwise keep the original
    dplyr::mutate(final_catch = coalesce(catch.new, catch.orig)) %>%
    # Keep only the columns needed for reshaping back
    dplyr::select(Time, SubGroup, final_catch)
  
  # --- 3. Reshape Back to Original Format and Write File ---
  
  # Reshape the updated long data back to the original wide format
  final_data_wide <- updated_data_long %>%
    tidyr::pivot_wider(names_from = SubGroup, values_from = final_catch)
  
  # Write the header lines to the new file
  writeLines(header_lines, output_file)
  
  # Append the modified data to the new file, preserving the original column names
  suppressWarnings(
    write.table(final_data_wide, 
                output_file, 
                append = TRUE,
                col.names = F,
                sep = "   ", # Adjust separator if needed
                row.names = FALSE, 
                quote = FALSE)
  )
  
  cat(sprintf("Successfully modified data written to: %s\n", output_file))
  return(invisible(output_file))
}