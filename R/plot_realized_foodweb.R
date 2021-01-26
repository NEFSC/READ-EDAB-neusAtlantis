# Plot realized "food-web" based on dietcheck.txt

plot_realized_foodweb = function(run.dir,diet.file,min.fract= 0.05,min.time,max.time,rand.layout=T,seed = 25,phys.off = F){
  
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(igraph)
  library(visNetwork)
  library(networkD3)
  
  #read in diet data
  diet.data = as.data.frame(data.table::fread(diet.file,stringsAsFactors = F))
  
  #aggregate over time and cohort by predator
  
  #over time first
  diet.agg = diet.data %>%
    filter(Time>=min.time & Time <=max.time) %>%
    select(-Time,-Stock,-Updated) %>%
    group_by(Predator,Cohort) %>%
    summarize_all(list(~mean(.)))
  
  #Over cohort
  diet.agg = diet.agg %>%
    select(-Cohort) %>%
    group_by(Predator) %>%
    summarize_all(list(~mean(.)))
    
  #predator and prey vectors
  pred.names = diet.agg$Predator
  prey.names = colnames(diet.agg)[-1]
  
  #remove Predator column
  diet.mat= diet.agg %>% select(-Predator) %>% as.matrix()
  
  #filter out by min.fract
  diet.mat2 = apply(diet.mat,MARGIN = c(1,2),function(x) ifelse(x>=min.fract,x,0))
  #add back in Predators
  diet.agg =as.data.frame(diet.mat2)
  diet.agg$pred = pred.names
  diet.long = reshape2::melt(diet.agg,id.vars = 'pred',factorsAsStrings = T) %>%
    filter(value != 0) %>%
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
    rowid_to_column("id")
  
  #Get edge list formatted
  diet.long = tibble(diet.long)
  
  edges <- diet.long %>% 
    left_join(nodes, by = c("prey" = "label")) %>% 
    rename(from = id) %>% 
    left_join(nodes, by = c("pred" = "label")) %>% 
    rename(to = id) %>%
    select(from, to, prop) %>%
    mutate(width = (prop*100/20) + 1)

  diet.network <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  # plot(diet.network, edge.arrow.size = 0.2)
  
  # visNetwork::visNetwork(nodes,edges)
  p=visNetwork(nodes, edges) %>% 
     visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T, collapse = T) %>%
    visEdges(arrows = "middle", color = list(color = 'grey', highlight = 'grey',hover = 'grey')) %>%
    visNodes(color = list(background = 'brown',
                          label = list(color = 'black',face = 'bold'),
                          border = 'black',
                          hover = list(border = 'black',background = 'lightblue')),
             font = list(size= 24, face = 'bold'))
  if(rand.layout){
    p = p %>% visLayout(randomSeed = seed) %>% visPhysics(repulsion = list(nodeDistance = 400))
  }else{
    p = p %>% visHierarchicalLayout(direction = 'LR', levelSeparation = 100, nodeSpacing = 150)
  }
  if(phys.off){
    p = p %>% visPhysics(stabilization = F) %>% visEdges(smooth = F)
  }
    
    
    # visIgraphLayout(layout = "layout_with_fr") %>% 
    
  print(p)
  # nodes$n.nodes = sapply(nodes$label, function(x) return(diet.long %>% filter(pred == x | prey == x) %>% nrow()))
  
}


run.name = 'ZooTuningRevisit_3'
atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/'
run.dir = paste0(atl.dir,run.name,'/')
diet.file = paste0(run.dir,'neus_outputDietCheck.txt')

plot_realized_foodweb(run.dir,
                      diet.file,
                      min.time = 16000,
                      max.time = 19000,
                      min.fract = 0.25,
                      rand.layout = T,
                      phys.off = F)
