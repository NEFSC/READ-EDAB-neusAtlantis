
# scenario.diet.data = 'D:/fishing_sensitivity_test/scenario_stats_diet.rds'
# run.prefix = 'fish_sens_catch_scalar_species_1'
# setup.file = here::here('Setup_Files','fish_sens_catch_scalar_species_1.csv')

tic()
make_diet_network = function(run.dir,start.time,end.time,min.fract){
  
  library(dplyr)
  library(fluxweb)
  library(igraph)


  #read in diet data
  diet.agg = data.table::fread(paste0(run.dir,'neus_outputDietCheck.txt'))%>%
    filter(Time >= start.time & Time <= end.time)%>%
    select(-Stock, -Updated,-Cohort)%>%
    group_by(Time,Predator)%>%
    summarise(across(everything(),mean))%>%
    ungroup()%>%
    select(-Time)%>%
    group_by(Predator)%>%
    summarise(across(everything(),mean))%>%
    ungroup()
  
  #predator and prey vectors
  pred.names = diet.agg$Predator
  prey.names = colnames(diet.agg)[-1]
  
  missing.prey = prey.names[which(!(prey.names %in% pred.names))]
  
  #remove Predator column
  diet.mat= diet.agg %>% select(-Predator) %>% as.matrix()
  
  #filter out by min.fract
  diet.mat2 = apply(diet.mat,MARGIN = c(1,2),function(x) ifelse(x>=min.fract,x,0))
  
  missing.rows = matrix(0,ncol = ncol(diet.mat2),nrow = length(missing.prey))
  pred.names = c(pred.names,missing.prey)
  
  diet.mat2 = rbind(diet.mat2,missing.rows)
  
  #add back in Predators
  diet.agg =as.data.frame(diet.mat2)
  diet.agg$pred = pred.names
  diet.long = diet.agg %>%
    tidyr::gather('variable','value',-pred)%>%
    filter(value !=0)%>%
    mutate(variable = as.character(variable)) 
  colnames(diet.long) = c('pred','prey','prop')
  
  ##Format for network plot
  
  #Get source, destination, and nodes list
  sources <- diet.long %>%
    distinct(prey) %>%
    rename(label = prey)
  
  destinations <- diet.long %>%
    distinct(pred) %>%
    rename(label = pred)
  
  nodes <- full_join(sources, destinations, by = "label") %>%
    arrange(label) %>%
    tibble::rowid_to_column("id")
  
  
  ###Format meta deta on species and guild
  fgs = read.csv(here::here('currentVersion','neus_groups.csv'),as.is =T) %>%
    select(Code,LongName)
  
  spp.info = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is=T) %>%
    select(Code,Guild)%>%
    filter(Code %in% unique(nodes$label))%>%
    arrange(Code)%>% 
    mutate(id =1:n()) %>%
    left_join(fgs)
  
  ###Get edge list formatted
  diet.long = tibble::tibble(diet.long)
    # mutate(prop = ifelse(prop > 0, 1, 0))
  
  edges <- diet.long %>% 
    left_join(nodes, by = c("prey" = "label")) %>% 
    rename(from = id) %>% 
    left_join(nodes, by = c("pred" = "label")) %>% 
    rename(to = id) %>%
    select(from, to, prop) %>%
    mutate(width = (prop*100/20) + 1)

  
  nodes = left_join(nodes,spp.info,by = c('label' = 'Code'))%>%
    rename(name = 'LongName')
  diet.network <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
 
  return(diet.network) 
}


diet.network = make_diet_network(run.dir = here::here('Atlantis_Runs','Extended_Constant_Catch','/'),
                                 start.time = 365*57,end.time = 365*77,min.fract = 0)

#From https://rfrelat.github.io/BalticFoodWeb.html
calculate_trophic_level = function(net){
  
  library(fluxweb)
  #Get adjacency matrix 
  mat <- get.adjacency(net, sparse=F)
  
  #Detect basal node
  basal <- rownames(subset(mat, apply(mat, 2, sum)==0) & apply(mat, 1, sum)!= 0)
  #Compute the shortest path to basal node
  paths_prey <- suppressWarnings(shortest.paths(graph = net, v= V(net), to = V(net)[basal], 
                                                mode = "in", weights = NULL, algorithm = "unweighted"))
  
  paths_prey[is.infinite(paths_prey)] <- NA
  shortest_paths <- suppressWarnings(as.matrix(apply(paths_prey, 1, min, na.rm=TRUE)))
  #for species with no prey apart of them
  shortest_paths[is.infinite(shortest_paths)] <- NA
  
  # Shortest TL
  sTL <- 1 + shortest_paths
  
  # Compute average prey trophic level
  # inspired from cheddar package calculation
  W <- t(mat)
  rs <- rowSums(W)
  W <- W/matrix(rs, ncol = ncol(W), nrow = nrow(W))
  W[0 == rs, ] <- 0
  I <- diag(ncol(W))
  tl0<-rowSums(I - W)
  result <- tryCatch(solve(I - W), error = function(e) e)
  if ("error" %in% class(result)) {
    avtl <- rep(NA, ncol(pm))
    names(avtl) <- colnames(pm)
  }
  else {
    avtl <- rowSums(result)
  }
  
  # Short-weighted TL is the average between 
  # Shortest TL and average prey TL
  SWTL <- (sTL + avtl)/2
  
  tl.df = data.frame(LongName = row.names(SWTL),trophic.level = SWTL[,1])
  rownames(tl.df) = NULL
  
  return(tl.df)
}

trophic.level = calculate_trophic_level(diet.network)

run.name = 'Extended_Constant_Catch'
calculate_foodweb_metrics = function(net,run.name){
  
  
  #Species richness
  S = vcount(net)
  
  #connectance
  connectance = ecount(net)/S^2
  
  #Generality (number of prey)
  pred = degree(net, mode = 'in')>0
  generality = sum(degree(net, mode = 'in')[pred],na.rm=T)/sum(pred,na.rm=T)
  
  #Vulnerability (number of predators)
  prey = degree(net,mode = 'out')>0
  vulnerability = sum(degree(net, mode="out")[prey])/sum(prey)
  
  #Mean shortest path
  sp = shortest.paths(net)
  short.path = mean(sp[upper.tri(sp)])
  
  #Trophic level
  tlnodes = calculate_trophic_level(net) 
  
  TL = mean(tlnodes$trophic.level)
  
  net.stats = data.frame(run.name = run.name,
                         connectance = connectance,
                         generality = generality,
                         vulnerability = vulnerability,
                         mean.short.path = short.path,
                         mean.trophic.lev = TL
                         )
  return(net.stats)
  
}

toc()
