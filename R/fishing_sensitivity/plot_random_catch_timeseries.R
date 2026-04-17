# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#' Plot Catch Timeseries from Randomized Experiments
#'
#' @param catch_file Character string; the path to the catch forcing text file.
#' @param sim_list List; an R list where each element contains a numeric vector 
#'   representing the timestep order for that specific simulation run.
#' @param exp_ids Numeric vector; the indices/IDs of the experiments (from sim_list) to plot.
#' @param species_names Character vector; the names of the species to extract and plot (e.g., c("MAK", "COD")).
#'
#' @return A ggplot object containing the faceted timeseries.
#' @export
plot_catch_timeseries <- function(catch_file, sim_list, exp_ids, species_names) {
  
  # ----------------------------------------------------------------------------
  # 1. Parse the Header to Extract Column Names
  # ----------------------------------------------------------------------------
  # Read the lines of the file. We read the whole file as text to ensure we 
  # capture the full header, regardless of how many columns exist.
  all_lines <- readLines(catch_file, warn = FALSE)
  
  # Search for the lines that define the column names 
  # (e.g., "## COLUMN1.name Time", "## COLUMN2.name MAK")
  name_lines <- grep("^## COLUMN[0-9]+\\.name", all_lines, value = TRUE)
  
  # Extract just the column names by removing the "## COLUMN[x].name " prefix
  col_names <- trimws(sub("^## COLUMN[0-9]+\\.name\\s+", "", name_lines))
  
  if (length(col_names) == 0) {
    stop("Could not parse any column names from the header.")
  }
  
  # ----------------------------------------------------------------------------
  # 2. Read the Catch Data
  # ----------------------------------------------------------------------------
  # read.table will automatically skip all header lines starting with '#'
  raw_data <- read.table(catch_file, comment.char = "#", header = FALSE)
  
  # Assign the parsed names to the data frame
  # Ensure we only assign names up to the number of columns parsed, just in case
  colnames(raw_data) <- col_names[1:ncol(raw_data)]
  
  # Verify all requested species exist in the parsed columns
  missing_spp <- setdiff(species_names, colnames(raw_data))
  if (length(missing_spp) > 0) {
    stop(paste("The following species were not found in the dataset:", 
               paste(missing_spp, collapse = ", ")))
  }
  
  # ----------------------------------------------------------------------------
  # 3. Process Data for the Requested Experiments
  # ----------------------------------------------------------------------------
  plot_data_list <- lapply(exp_ids, function(id) {
    
    # Extract the sequence of timesteps for this specific experiment ID
    if (id > length(sim_list)) {
      stop(paste("Experiment ID", id, "exceeds the length of sim_list."))
    }
    seq_idx <- sim_list[[id]]
    
    # Subset the raw data using the randomized sequence indices
    # We extract the 'Time' column (just in case) and the requested species
    cols_to_keep <- c("Time", species_names)
    # Ensure "Time" exists, otherwise just take the species
    cols_to_keep <- intersect(cols_to_keep, colnames(raw_data)) 
    
    exp_dat <- raw_data[seq_idx, cols_to_keep, drop = FALSE]
    
    # Add a continuous sequential time step for the X-axis of the plot
    # since the actual 'Time' column is now randomized/shuffled
    exp_dat$Simulation_Step <- 1:nrow(exp_dat)
    exp_dat$Experiment <- as.character(id)
    
    # Pivot the data from wide to long format for ggplotting
    exp_dat_long <- tidyr::pivot_longer(
      exp_dat,
      cols = dplyr::all_of(species_names),
      names_to = "Species",
      values_to = "Catch"
    )
    
    return(exp_dat_long)
  })
  
  # Combine all the processed experiment data into one single data frame
  plot_data <- dplyr::bind_rows(plot_data_list)
  
  # ----------------------------------------------------------------------------
  # 3.5 Process the "Real" Original Data
  # ----------------------------------------------------------------------------
  # Extract the original unshuffled data for the requested species
  real_dat <- raw_data[, intersect(colnames(raw_data), species_names), drop = FALSE]
  real_dat$Simulation_Step <- 1:nrow(real_dat)
  
  # Pivot the original data to long format
  real_dat_long <- tidyr::pivot_longer(
    real_dat,
    cols = dplyr::all_of(species_names),
    names_to = "Species",
    values_to = "Catch"
  )
  
  # ----------------------------------------------------------------------------
  # 4. Generate the Plot
  # ----------------------------------------------------------------------------
  g <- ggplot() +
    # Add experimental lines first
    geom_line(data = plot_data, aes(x = Simulation_Step, y = Catch, color = Experiment), 
              alpha = 0.8, linewidth = 0.8) +
    # Add the real original data line on top (in black)
    geom_line(data = real_dat_long, aes(x = Simulation_Step, y = Catch, linetype = "Original Data"), 
              color = "black", linewidth = 1.2) +
    # Add a specific legend key for the black line
    scale_linetype_manual(values = c("Original Data" = "solid"), name = NULL) +
    facet_wrap(~ Species, scales = "free_y") +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    ) +
    labs(
      title = "Species Catch Timeseries by Simulation Run",
      subtitle = "Black line represents the original (unshuffled) catch data",
      x = "Simulation Timestep",
      y = "Catch",
      color = "Experiment ID"
    )
  
  return(g)
}

# ==============================================================================
# EXAMPLE USAGE:
# ==============================================================================
# # Assuming you have a file named "catch_forcing.txt" and your list 'sim_list'
# 
# my_simulations <- readRDS('Z:/fishing_sensitivity_manuscript/data/random_catch_combined/random_catch_combined_permutations.rds')
# 
# my_plot <- plot_catch_timeseries(
#   catch_file = here::here('currentVersion','CatchFiles','total_catch_random_catch1.ts'),
#   sim_list = my_simulations,
#   exp_ids = c(1, 2),                # Plot experiment 1 and 2
#   species_names = c("MAK", "COD")   # Species of interest
# )
# # 
# print(my_plot)