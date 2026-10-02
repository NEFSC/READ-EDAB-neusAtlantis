#' Extract info from fishbase
#' 
#' Pull information from fishbase. For example: maximum weight (kg) and maximum length (cm), common length (cm)
#'
#' See https://fishbase.mnhn.fr/manual/english/fishbasethe_species_table.htm

library(magrittr)
# read in main species file
allGroups <- readr::read_csv(file = here::here("data","functionalGroupNames.csv"),show_col_types=FALSE)

# select single species (omit functional groups)
species <- allGroups %>%
  dplyr::count(Code) %>% 
  dplyr::filter(n == 1) 

# additonal species which have 2 NESPP3 codes and are duplicated
moreSpecies <- allGroups %>%
  dplyr::count(Scientific_Name,Species,Code) %>%
  dplyr::filter(n > 1) %>%
  dplyr::select(Code,n)

species <- rbind(species,moreSpecies)

# get scientific names for species
# hard code changes for species that have old names in SVDBS
sciName <- allGroups %>% 
  dplyr::filter(Code  %in% species$Code) %>%
  dplyr::select(Species,Scientific_Name,Code) %>%
  dplyr::distinct() %>% 
  dplyr::filter(!is.na(Scientific_Name)) %>%
  dplyr::mutate(Scientific_Name =  abutils::capitalize_first_letter(unlist(Scientific_Name))) %>%
  dplyr::mutate(Scientific_Name = dplyr::if_else(Scientific_Name == 'Paralichthys oblongus','Hippoglossina oblonga',Scientific_Name)) %>%
  dplyr::mutate(Scientific_Name = dplyr::if_else(Scientific_Name == 'Loligo pealeii','Doryteuthis pealeii',Scientific_Name)) %>%
  dplyr::mutate(Scientific_Name = dplyr::if_else(Scientific_Name == 'Brevoortia',"Brevoortia tyrannus",Scientific_Name))
                                                  
              
rowAll <- function(x) rowSums(x) == ncol(x) -1

# pull info from species table in fishbase
fb <- rfishbase::species(sciName$Scientific_Name,server = "fishbase") %>%
  dplyr::select(Species,Length,CommonLength,Weight,LongevityWild) %>%
  dplyr::mutate(dplyr::across(Length:Weight,as.numeric)) %>%
  dplyr::distinct()

# check for duplicates
dups <- fb %>% dplyr::count(Species) %>% dplyr::filter(n>1)
fb %>% dplyr::filter(Species %in% dups$Species)

# pull out vertebrate species
fb <- fb %>% 
  dplyr::filter(!rowAll(dplyr::across(.cols = everything(),.fns = ~ is.na(.x))))

# search sealife base for invertibrate info
slb <- rfishbase::species(sciName$Scientific_Name,server = "sealifebase") %>%
  dplyr::select(Species,Length,CommonLength,Weight,LongevityWild) %>% 
  dplyr::mutate(dplyr::across(Length:Weight,as.numeric)) %>%
  dplyr::distinct()

# filter out invertibrates
slb <- slb %>% 
  dplyr::filter(!rowAll(dplyr::across(.cols = everything(),.fns = ~ is.na(.x))))

# join vertebrate and invertebrate info
speciesStats <- rbind(fb,slb) %>%
  dplyr::left_join(.,sciName,by = c("Species"="Scientific_Name")) %>%
  dplyr::rename(Common_Name = Species.y) %>%
  dplyr::rename(code=Code) %>% 
  dplyr::rename(scientificName=Species) %>%
  dplyr::rename(maxObsLength= Length) %>%
  dplyr::rename(maxObsWeight= Weight) %>%
  dplyr::rename(maxObsAge = LongevityWild)

missingData <- speciesStats %>%
  dplyr::filter(rowAll(dplyr::across(.cols = everything(),,.fns = ~ is.na(.x))))
if (nrow(missingData)>0) {
  missingData %>% dplyr::select(Species)
}

saveRDS(speciesStats,here::here("data","speciesFishbaseStats.RDS"))

# 
# 1 Tetrapturus albidus         -Kajikia albida (renamed)
# 2 Peprilus alepidotus     -Peprilus paru (syn)
# 3 Etrumeus teres         - Etrumeus sadina (syn)
# 4 Menticirrhus saxatillis - Menticirrhus saxatilis (spelling)
# 5 Zenopsis ocellata      - Zenopsis conchifer (renamed)
# 6 Citharicthys arctifrons - Citharichthys arctifrons (spelling)
# 7 Paralichthys oblongus  - Hippoglossina oblonga (renamed)
# 8 Loligo pealeii         - Doryteuthis pealeii (syn)
# 9 Lepidochelys kempi     - turtle
# 10 Raja eglanteria    - Rostroraja eglanteria (renamed)
# 
# #}
