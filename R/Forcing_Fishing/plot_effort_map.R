# -----------------------------------------------------------------------------
# SCRIPT NAME: plot_effort_map.R
#
# AUTHOR: [Original Author Name]
# DATE: [Date of Last Revision]
#
# DESCRIPTION:
# This script creates a spatial map of fishing effort from an Atlantis model
# run. It reads effort data from a NetCDF output file, processes it, and joins
# it with a shapefile of the model domain. The final output is a faceted plot
# showing the proportional effort distribution for each groundfish fleet.
#
# KEY FUNCTIONS:
# 1.  Loads Atlantis model effort data from a NetCDF file.
# 2.  Extracts and aggregates effort data for specified fishing fleets.
# 3.  Calculates proportional effort per spatial polygon for each fleet.
# 4.  Reads a shapefile defining the model's spatial polygons.
# 5.  Joins the effort data with the spatial data.
# 6.  Generates and saves a multi-panel map visualizing the fishing effort
#     footprint for each fleet.
#
# LIBRARIES:
# - ncdf4: For reading and processing NetCDF files.
# - here: For constructing portable file paths.
# - dplyr: For data manipulation.
# - sf: For handling simple features (spatial data).
# - ggplot2: For creating plots.
# - stringr: For string manipulation (extracting fleet IDs).
# - rnaturalearth: For fetching coastline data for maps.
#
# INPUTS:
# - Atlantis run name (`run.name`).
# - NEUS model domain shapefile (`Neus_ll_0p01.shp`).
# - Fisheries definition file (`neus_fisheries.csv`).
# - Atlantis NetCDF output file (`neus_outputTOTCATCH.nc`).
#
# OUTPUTS:
# - PNG Plot:
#   - Groundfish_fishing_footprints.png
# -----------------------------------------------------------------------------


# --- 1. LOAD LIBRARIES ---
library(ncdf4)
library(here)
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)
library(rnaturalearth) # Used for ne_countries() to get coastline map


# --- 2. SETUP AND CONFIGURATION ---

# Define the name of the Atlantis model run to be analyzed.
run.name = 'GF_gloucester_scale_2'
# Define paths to input files.
neus.shp.path = here::here('Geometry', 'gis', 'Neus_ll_0p01.shp')
fleet.def.path = here::here('currentVersion', 'neus_fisheries.csv')
catch.nc.path = here::here('Atlantis_Runs', run.name, 'neus_outputTOTCATCH.nc')


# --- 3. PROCESS NETCDF EFFORT DATA ---

# Read the fisheries definition file to map fleet codes to IDs.
fleet.def = read.csv(fleet.def.path)
# Define which fleets to include in the plot (all groundfish fleets).
fleets.to.plot = grep('^gf', fleet.def$Code, value = T)

# Open the NetCDF output file.
catch.nc = nc_open(catch.nc.path)
varnames = names(catch.nc$var)
# Find all variables related to fleet effort ('*Effort*FC*').
effort.vars = grep('Effort', varnames, value = TRUE)
effort.vars = effort.vars[grepl('FC', effort.vars)]

# Extract the numeric fleet ID from each variable name (e.g., 'Tot_Effort_FC1' -> 1).
effort.fleet.id = as.numeric(stringr::str_extract(effort.vars, "\\d+$"))

# Loop through each effort variable to extract and process the data.
fleet.effort.ls = list()
for (i in 1:length(effort.vars)) {
  # Get the effort data array for the current fleet.
  vardat = ncvar_get(catch.nc, effort.vars[i])
  # Sum effort over the time dimension to get total effort per polygon.
  vardat.sum.t = apply(vardat, 2, sum, na.rm = T)
  # Find which time steps had non-zero effort.
  nonzero.effort = which(vardat.sum.t > 0)
  
  current_fleet_id = effort.fleet.id[i]
  
  # If there was effort, calculate the mean effort across time for each polygon.
  if (length(nonzero.effort) > 0) {
    vardat.nonzero = vardat[, nonzero.effort, drop = FALSE]
    vardat.mean = apply(vardat.nonzero, 1, mean, na.rm = T)
    fleet.effort.ls[[i]] = data.frame(
      polygon = (1:dim(vardat)[1]) - 1,
      effort.mean = vardat.mean,
      fleet.id = current_fleet_id
    )
  }
}
# Close the NetCDF file connection.
nc_close(catch.nc)

# Combine the list of data frames into a single data frame.
# Join with fleet definitions to get fleet codes/names.
# Calculate proportional effort for each polygon within its fleet.
fleet.effort.df = bind_rows(fleet.effort.ls) %>%
  left_join(fleet.def, by = c('fleet.id' = 'Index')) %>%
  filter(Code %in% fleets.to.plot & effort.mean > 0) %>%
  group_by(fleet.id, Code) %>%
  mutate(effort.tot = sum(effort.mean, na.rm = T)) %>%
  ungroup() %>%
  mutate(effort.prop = effort.mean / effort.tot)


# --- 4. PREPARE SPATIAL DATA FOR PLOTTING ---

# Read the NEUS model domain shapefile into an sf object.
neus_sf = st_read(neus.shp.path)

# Join the spatial polygons with the processed effort data.
# The 'BOX_ID' in the shapefile should match the 'polygon' ID from the NC file.
map.data = neus_sf %>%
  mutate(BOX_ID = as.numeric(BOX_ID)) %>%
  left_join(fleet.effort.df, by = c("BOX_ID" = "polygon")) %>%
  filter(!is.na(Code)) # Keep only polygons for the fleets we are plotting.

# Get a world map and transform its CRS to match the project's CRS.
# This ensures the coastline layer aligns correctly with the model domain.
world <- ne_countries(scale = "medium", returnclass = "sf")
project_crs <- st_crs(neus_sf)
world_transformed <- st_transform(world, crs = project_crs)


# --- 5. BUILD AND SAVE THE PLOT ---

# Create the ggplot map.
effort_plot <- ggplot() +
  # Layer 1: All NEUS polygons as a light grey background for context.
  geom_sf(data = neus_sf,
          fill = "gray90",
          color = "white",
          linewidth = 0.1) +
  
  # Layer 2: Shaded coastline data for land.
  geom_sf(data = world_transformed,
          fill = "tan",
          color = "gray50") +
  
  # Layer 3: Overlay the fleet effort data.
  # Fill color is mapped to the fleet Code.
  # Transparency (alpha) is mapped to the proportional effort.
  geom_sf(
    data = map.data,
    aes(fill = Code, alpha = effort.prop),
    color = "black",
    linewidth = 0.1
  ) +
  
  # Set the plot's coordinate limits to zoom in on the study area.
  coord_sf(
    xlim = st_bbox(neus_sf)$xlim,
    ylim = st_bbox(neus_sf)$ylim,
    expand = FALSE
  ) +
  
  # Facet the plot to create a separate panel for each fleet.
  facet_wrap( ~ Code) +
  
  # --- Define aesthetic scales ---
  scale_fill_brewer(name = "Fleet", palette = 'Set1') +
  scale_alpha_continuous(name = "Effort Proportion", range = c(0.1, 1)) +
  
  # --- Labels and Theme ---
  labs(
    title = "Groundfish Fishery Effort Footprint by Port",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  guides(fill = 'none') # Hide the fill legend as facets are already labeled.

# Print the plot to the RStudio plot pane.
print(effort_plot)

# Save the plot to a PNG file in the 'Figures' directory.
ggsave(
  here::here('Figures', 'Groundfish_fishing_footprints.png'),
  effort_plot,
  width = 8,
  height = 12,
  units = 'in'
)
