#' loads in the catchPerFishery.txt file
#'
#' 
#'
#'


load_catch_fishery <- function(dir, file_catch, fgs, verbose = FALSE) {
  
  file.catch <- file.path(dir, file_catch)
  catchbio <- read.table(file.catch, header = TRUE)
  fgs <- fgs[fgs$IsTurnedOn > 0, ]
  colnames(fgs) <- tolower(colnames(fgs))
  fishedlookup <- fgs[fgs$isfished > 0, ]
  names(catchbio)[match(fishedlookup$code, names(catchbio))] <- fishedlookup$name
  
  catchbio <- catchbio |> 
    tidyr::pivot_longer(col=c(-Time,-Fishery)) 
  
  # out <- data.frame(species = catchbio$species, agecl = NA, 
  #                   polygon = NA, layer = NA, time = catchbio$Time, atoutput = catchbio$catchbio)
  # out <- out[order(out$species, out$time, out$polygon, out$agecl), ]
  
  out <- catchbio |> 
    dplyr::rename(time = Time,
                  species = name,
                  atoutput = value,
                  fishery = Fishery) |>
    dplyr::arrange(species,time)
  
  return(out)
}

