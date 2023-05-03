load_nc_temp =function (nc, fgs, bps, select_groups, select_variable, prm_run, 
          bboxes, check_acronyms = TRUE, warn_zeros = FALSE, report = TRUE) 
{
  supported_variables <- c("N", "Nums", "ResN", "StructN", 
                           "Eat", "Growth", "Prodn", "Grazing", "Catch")
  if (length(select_groups) == 0) 
    stop("No functional groups selected.")
  if (length(select_variable) == 0) 
    stop("No variable selected.")
  if (length(select_variable) > 1) 
    stop("Only one variable allowed per function call.")
  if (any(!is.element(select_variable, supported_variables))) {
    stop(paste("Only", paste(supported_variables, collapse = ", "), 
               "can be selected as 'select_variable'"))
  }
  fgs <- load_fgs(fgs = fgs)
  if (check_acronyms) {
    active_groups <- fgs[fgs$IsTurnedOn == 1, "Name"]
    inactive_groups <- select_groups[which(!is.element(select_groups, 
                                                       active_groups))]
    if (length(inactive_groups) >= 1) {
      select_groups <- select_groups[!is.element(select_groups, 
                                                 inactive_groups)]
      warning(paste(paste("Some selected groups are not active in the model run. Check 'IsTurnedOn' in fgs\n"), 
                    paste(inactive_groups, collapse = "\n")))
    }
    if (all(!is.element(select_groups, active_groups))) {
      stop(paste("None of the species selected are active in the model run.", 
                 "Check spelling and Check 'IsTurnedOn' in fgs"))
    }
  }
  at_out <- RNetCDF::open.nc(con = nc)
  on.exit(RNetCDF::close.nc(at_out))
  # if (select_variable != "N" & all(is.element(select_groups, 
  #                                             bps))) 
  #   stop("The only output for Biomasspools is N.")
  var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_out)$nvars - 
                                     1), function(x) RNetCDF::var.inq.nc(at_out, x)$name)
  n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
  n_boxes <- RNetCDF::dim.inq.nc(at_out, 1)$length
  n_layers <- RNetCDF::dim.inq.nc(at_out, 2)$length
  cohorts <- 1:max(fgs$NumCohorts)
  search <- list()
  for (i in seq_along(select_groups)) {
    search[[i]] <- c(unlist(lapply(paste0(select_groups[i], 
                                          cohorts), paste0, select_variable)), unlist(lapply(paste0(select_groups[i], 
                                                                                                    select_variable), paste0, cohorts)), unlist(lapply(paste0(select_groups[i], 
                                                                                                                                                              cohorts), paste, select_variable, sep = "_")), unlist(lapply(paste(select_groups[i], 
                                                                                                                                                                                                                                 select_variable, sep = "_"), paste0, cohorts)), 
                     unlist(lapply(paste(select_groups[i], cohorts, sep = "_"), 
                                   paste, select_variable, sep = "_")), unlist(lapply(paste(select_groups[i], 
                                                                                            select_variable, sep = "_"), paste, cohorts, 
                                                                                      sep = "_")), unlist(paste0(select_groups[i], 
                                                                                                                 select_variable)), unlist(paste(select_groups[i], 
                                                                                                                                                 select_variable, sep = "_")))
    search[[i]] <- search[[i]][is.element(search[[i]], var_names_ncdf)]
    search[[i]] <- unique(search[[i]])
  }
  search_clean <- do.call(c, search)
  if (length(search_clean) == 0) 
    return(0)
  at_data <- list()
  if (report) 
    pb <- dplyr::progress_estimated(length(search_clean))
  for (i in seq_along(search_clean)) {
    at_data[[i]] <- RNetCDF::var.get.nc(ncfile = at_out, 
                                        variable = search_clean[i])
    if (report) 
      pb$tick()$print()
  }
  if (report) 
    pb$stop()
  final_species <- select_groups[sapply(lapply(select_groups, 
                                               grepl, x = search_clean), any)]
  id <- sapply(final_species, function(x) which(x == fgs$Name))
  if ("NumGeneTypes" %in% names(fgs)) {
    final_agecl <- fgs$NumCohorts[id] * fgs$NumGeneTypes[id]
  }else {
    final_agecl <- fgs$NumCohorts[id]
  }
  num_layers <- RNetCDF::var.get.nc(ncfile = at_out, variable = "numlayers")
  if (length(dim(num_layers)) == 2) {
    if (all(apply(num_layers, MARGIN = 1, FUN = function(x) length(unique)) == 
            1)) {
      num_layers <- num_layers[, 1]
    }
    else {
      stop("Different numbers of layers per Box. This nc-structure is not supported.")
    }
  }
  num_layers <- num_layers + ifelse(num_layers == 0, 0, 1)
  for (i in seq_along(num_layers)) {
    if (i == 1) 
      layerid <- array(dim = c(n_layers, n_boxes))
    if (num_layers[i] == 0) {
      layerid[, i] <- 0
    }
    else {
      if (!is.null(bboxes) & is.element((i - 1), bboxes)) {
        layerid[, i] <- 0
      }
      else {
        layerid[, i] <- c(rep(0, times = n_layers - 
                                num_layers[i]), rep(1, times = num_layers[i]))
      }
    }
  }
  boxes <- 0:(n_boxes - 1)
  island_ids <- num_layers == 0
  if (!is.null(bboxes)) {
    boundary_ids <- is.element(boxes, bboxes)
    island_ids <- island_ids | boundary_ids
  }
  boxes <- boxes[!island_ids]
  num_layers <- num_layers[!island_ids]
  polygons <- rep(boxes, times = num_layers)
  layers <- sapply(num_layers[num_layers != 0] - 2, function(x) c(seq(x, 
                                                                      from = 0, by = 1), n_layers - 1))
  if (any(sapply(layers, length) != num_layers[num_layers != 
                                               0])) {
    stop("Number of layers incorrect. Contact package development team.")
  }
  layers <- do.call(c, layers)
  if (length(polygons) != length(layers)) 
    stop("Number of polygons and layers do not match. Contact package development team.")
  at_data3d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 
                               3)]
  at_data2d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 
                               2)]
  int_fs <- final_species
  int_fa <- final_agecl
  if (length(at_data3d) >= 1) {
    if (select_variable == "N") {
      int_fs <- final_species[!is.element(final_species, 
                                          bps)]
      int_fa <- final_agecl[!is.element(final_species, 
                                        bps)]
      int_fa[int_fa != 2] <- 1
    }
    for (i in seq_along(at_data3d)) {
      if (i == 1) 
        result3d <- list()
      for (j in 1:n_timesteps) {
        if (j == 1) 
          values <- array(dim = c(length(layers), n_timesteps))
        values[, j] <- at_data3d[[i]][, , j][layerid == 
                                               1]
      }
      result3d[[i]] <- as.vector(values)
    }
    result3d <- data.frame(species = unlist(lapply(X = mapply(FUN = rep, 
                                                              x = int_fs, each = int_fa, SIMPLIFY = F, USE.NAMES = F), 
                                                   FUN = rep, each = length(layers) * n_timesteps)), 
                           agecl = unlist(lapply(X = lapply(X = int_fa, FUN = seq, 
                                                            from = 1, by = 1), FUN = rep, each = length(layers) * 
                                                   n_timesteps)), polygon = unlist(lapply(X = n_timesteps * 
                                                                                            int_fa, FUN = rep, x = polygons)), layer = unlist(lapply(X = n_timesteps * 
                                                                                                                                                       int_fa, FUN = rep, x = layers)), time = unlist(lapply(X = int_fa, 
                                                                                                                                                                                                             FUN = rep, x = rep(0:(n_timesteps - 1), each = length(layers)))), 
                           atoutput = do.call(c, result3d), stringsAsFactors = F)
  }
  if (length(at_data2d) >= 1) {
    if (select_variable == "N") {
      int_fs <- final_species[is.element(final_species, 
                                         bps)]
      int_fa <- final_agecl[is.element(final_species, 
                                       bps)]
    }
    if (select_variable == "Grazing") 
      int_fa <- 1
    for (i in seq_along(at_data2d)) {
      if (i == 1) 
        result2d <- list()
      for (j in 1:n_timesteps) {
        if (j == 1) 
          values <- array(dim = c(length(boxes), n_timesteps))
        values[, j] <- at_data2d[[i]][, j][boxes + 1]
      }
      result2d[[i]] <- as.vector(values)
    }
    result2d <- data.frame(species = unlist(lapply(X = mapply(FUN = rep, 
                                                              x = int_fs, each = int_fa, SIMPLIFY = F, USE.NAMES = F), 
                                                   FUN = rep, each = length(boxes) * n_timesteps)), 
                           agecl = unlist(lapply(X = lapply(X = int_fa, FUN = seq, 
                                                            from = 1, by = 1), FUN = rep, each = length(boxes) * 
                                                   n_timesteps)), polygon = unlist(lapply(X = n_timesteps * 
                                                                                            int_fa, FUN = rep, x = boxes)), time = unlist(lapply(X = int_fa, 
                                                                                                                                                 FUN = rep, x = rep(0:(n_timesteps - 1), each = length(boxes)))), 
                           atoutput = do.call(c, result2d), stringsAsFactors = F)
    if (select_variable == "N") 
      result2d$layer <- n_layers - 1
  }
  if (all(sapply(lapply(at_data, dim), length) == 3) & select_variable != 
      "N") 
    result <- result3d
  if (all(sapply(lapply(at_data, dim), length) == 2) & select_variable != 
      "N") 
    result <- result2d
  if (select_variable == "N") {
    if (length(at_data2d) >= 1 & length(at_data3d) == 0) 
      result <- result2d
    if (length(at_data2d) == 0 & length(at_data3d) >= 1) 
      result <- result3d
    if (length(at_data2d) >= 1 & length(at_data3d) >= 1) 
      result <- rbind(result2d, result3d)
  }
  min_pools <- is.element(result$atoutput, c(0, 1e-08, 1e-16))
  if (length(min_pools) > 0) {
    print_min_pools <- sum(min_pools) - length(result[min_pools & 
                                                        result$time == 1, 1]) - length(result[min_pools & 
                                                                                                result$time > 1 & result$layer == 7, 1])
    if (print_min_pools > 0 & warn_zeros) {
      warning(paste0(round(print_min_pools/dim(result)[1] * 
                             100), "% of ", select_variable, " are true min-pools (0, 1e-08, 1e-16)"))
    }
    result <- result[!min_pools, ]
  }
  if (select_variable == "N" & any(final_agecl == 2)) {
    result <- result %>% dplyr::group_by_("species", "polygon", 
                                          "layer", "time") %>% dplyr::summarise_(atoutput = ~sum(atoutput)) %>% 
      dplyr::ungroup()
  }
  if(nrow(result)== 0){
    return(NULL)
  }else{
    result$species <- convert_factor(data_fgs = fgs, col = result$species)
    toutinc <- extract_prm(prm_biol = prm_run, variables = "toutinc")
    result$time <- result$time * toutinc/365
    return(result)
  }

}
