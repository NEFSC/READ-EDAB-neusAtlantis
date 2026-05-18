# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)

edit_forcing_ts_df <- function(input_file, 
                               output_file, 
                               changes_df, 
                               time_col = "Time.d", 
                               code_col = "SubGroup", 
                               value_col = "catch.new") {
  
  # --- 1. Read and Parse the Input .ts File ---
  
  all_lines <- readLines(input_file)
  header_lines <- all_lines[startsWith(all_lines, "#")]
  
  # Extract species names from the header to use as column names
  df_map <- data.frame(Column = integer(), Code = character())
  for (line in header_lines) {
    if(grepl("\\.name", line)) {
      col_match <- stringr::str_match(line, "COLUMN[0-9]+")
      col_num <- as.integer(stringr::str_match(col_match, "[0-9]+"))
      species <- tail(unlist(stringr::str_split(trimws(line), "\\s+")), 1)
      df_map <- rbind(df_map, data.frame(Column = col_num, Code = species))
    }
  }
  
  # Sort by column number just to be safe
  df_map <- df_map[order(df_map$Column), ]
  
  # Read the data section (header = FALSE so we don't lose the first row!)
  catch_data_wide <- read.table(input_file, skip = length(header_lines), header = FALSE)
  
  # Apply the extracted column names (Time is always first)
  names(catch_data_wide) <- df_map$Code
  
  # --- 2. Reshape, Join, and Update Values ---
  
  changes_df <- changes_df %>%
    rename(Time = all_of(time_col),
           SubGroup = all_of(code_col),
           catch.new = all_of(value_col))
  
  updated_data_long <- catch_data_wide %>%
    tidyr::pivot_longer(cols = -Time, names_to = "SubGroup", values_to = "catch.orig") %>%
    dplyr::left_join(changes_df, by = c("Time", "SubGroup")) %>%
    dplyr::mutate(final_catch = coalesce(catch.new, catch.orig)) %>%
    dplyr::select(Time, SubGroup, final_catch)
  
  # --- 3. Reshape Back to Original Format and Write File ---
  
  final_data_wide <- updated_data_long %>%
    tidyr::pivot_wider(names_from = SubGroup, values_from = final_catch)
  
  writeLines(header_lines, output_file)
  
  suppressWarnings(
    write.table(final_data_wide, 
                output_file, 
                append = TRUE,
                col.names = FALSE, # Ensure headers aren't written to the data block
                sep = "\t",        # Tabs usually format cleaner for Atlantis TS files
                row.names = FALSE, 
                quote = FALSE)
  )
  
  cat(sprintf("Successfully modified data written to: %s\n", output_file))
  return(invisible(output_file))
}