# CHECK ALL FILENAMES FOR YOUR MODEL

# where are files on the atlantis google drive? 
g.name <- "Testing/OutForSarah"

# which local directory should they write to?
d.name <- here::here("diagnostics", "temp")

# input file names
functional.groups.file <- "neus_groups.csv"  
biomass.pools.file <- "neus_init.nc"
box.file <- "neus_tmerc_RM2.bgm"
initial.conditions.file <- "neus_init.nc"
fisheries.file <- "neus_fisheries.csv"
biol.prm.file <- "at_biology.prm"


# define the hindcast period--these are survey years
hindcast <- c(1980:2010)







