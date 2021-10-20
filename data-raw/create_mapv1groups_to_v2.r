#' Read and process changes in groups from V1.0 to V2.0
#' 
#' Create a txt file in md format for use in the wiki
#' 

codes <- readr::read_csv(here::here("data-raw/data","specieslist_v1_2.csv")) %>% 
  dplyr::select(GroupV1,Atcode,GroupV2,At2code) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Atcode,At2code)

# format to markdown table. Copy output to wiki
# open file and write
outputFile <- here::here("data","v1v2GroupMap.txt")
fileConn<-file(outputFile,open="w")
header <- paste0("|",paste0(names(codes),collapse = "|"),"|")
cat(header,file=fileConn,append=T)
cat("\n",file=fileConn,append=T)
spacer <- paste0("|",paste0(rep("---",ncol(codes)),collapse = "|"),"|")
cat(spacer,file=fileConn,append=T)
cat("\n",file=fileConn,append=T)

for (irow in 1:nrow(codes)) {
  rowData <- paste0("|",paste0(codes[irow,],collapse = "|"),"|")
  cat(rowData,file=fileConn,append=T)
  cat("\n",file=fileConn,append=T)
}

close(fileConn)


