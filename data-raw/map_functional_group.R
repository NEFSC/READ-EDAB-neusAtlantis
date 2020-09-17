#' Create Mapping of Species to functional group
#' 
#' Write the mapping to a file format (GitHub flavored markdown) that can used on the wiki. 
#' 
#'  Requires andybeet/utilities package for "capitalize_first_letter" function
library(magrittr)

# read in functional group codes and name
fg <-  readr::read_csv(here::here("data-raw","functional_group_names.csv"))

# read in species membership to group, then join with functional group names
data <- readr::read_csv(here::here("data-raw","Atlantis_1_5_groups_svspp_nespp3.csv")) %>%
  dplyr::mutate(NESPP3 = sprintf("%03d",NESPP3)) %>%
  dplyr::left_join(.,fg,by=c("Code"="Group Code"))


# connect to server
channel <- dbutils::connect_to_database("sole","abeet")
# get info by looking up by svspp code
pullBySVSPP <- dbutils::create_species_lookup(channel,species=na.omit(data$SVSPP),speciesType = "SVSPP")
SVSPPData <- pullBySVSPP$data %>%
  dplyr::select(SVSPPsv,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
  dplyr::distinct()

# get same data by looking up by nespp3
pullByNESPP3 <- dbutils::create_species_lookup(channel,species=na.omit(data$NESPP3),speciesType = "NESPP3")
NESPP3Data <- pullByNESPP3$data %>%
  dplyr::select(NESPP3,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
  dplyr::distinct()

# merge two results, rename variable
masterList <- dplyr::left_join(data,SVSPPData, by=c("SVSPP"="SVSPPsv"))  %>%
  dplyr::full_join(.,NESPP3Data, by="NESPP3") %>%
  dplyr::arrange(Code) %>%
  dplyr::rename(Species = Name,Functional_Group = `Group Name`,Common_Name = COMNAME.y,Scientific_Name=SCIENTIFIC_NAME.y,Species_Itis=SPECIES_ITIS.y)  %>% 
  dplyr::mutate(Common_Name = utilities::capitalize_first_letter(Common_Name)) %>%
  dplyr::select(Code,Functional_Group,Species,Common_Name,Scientific_Name,SVSPP,NESPP3,Species_Itis)


# format to markdown table. Copy output to wiki
# open file and write
outputFile <- here::here("data-raw","functionalGroups.txt")
fileConn<-file(outputFile,open="w")
header <- paste0("|",paste0(names(masterList),collapse = "|"),"|")
cat(header,file=fileConn,append=T)
cat("\n",file=fileConn,append=T)
spacer <- paste0("|",paste0(rep("---",ncol(masterList)),collapse = "|"),"|")
cat(spacer,file=fileConn,append=T)
cat("\n",file=fileConn,append=T)

for (irow in 1:nrow(masterList)) {
  rowData <- paste0("|",paste0(masterList[irow,],collapse = "|"),"|")
  cat(rowData,file=fileConn,append=T)
  cat("\n",file=fileConn,append=T)
}

close(fileConn)