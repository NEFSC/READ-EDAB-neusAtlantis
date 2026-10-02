#' loads in the catchPerFishery.txt file
#'
#' 
#'
#'


load_catch_fishery <- function(dir, file_catch, fgs, verbose = FALSE) {
  
  file.catch <- file.path(dir, file_catch)
  catchbio <- read.table(file.catch, header = TRUE) |>
    dplyr::as_tibble()
  fgsn <- fgs[fgs$IsTurnedOn > 0, ]
  colnames(fgsn) <- tolower(colnames(fgsn))
  fishedlookup <- fgsn[fgsn$isfished > 0, ]
  #names(catchbio)[match(fishedlookup$code, names(catchbio))] <- fishedlookup$name
  speciesCodes <- fishedlookup |>
    dplyr::select(code,name) |>
    dplyr::rename(species = name) |>
    dplyr::as_tibble()
  
  catchbiomass <- catchbio |> 
    tidyr::pivot_longer(col=c(-Time,-Fishery)) |>
    dplyr::left_join(speciesCodes, by = c("name"="code"))

  # out <- data.frame(species = catchbio$species, agecl = NA, 
  #                   polygon = NA, layer = NA, time = catchbio$Time, atoutput = catchbio$catchbio)
  # out <- out[order(out$species, out$time, out$polygon, out$agecl), ]
  
  out <- catchbiomass |> 
    dplyr::rename(time = Time,
                  code = name,
                  atoutput = value,
                  fishery = Fishery) |>
    dplyr::arrange(species,time)
  
  return(out)
}

