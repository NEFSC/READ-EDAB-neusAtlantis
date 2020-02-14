## Script to make diet comparisons (predators and prey) for a given set of species across model runs
## Specify: run names, species list, prey/pred subsetting criteria (top x% or top #)
## prey plots look at the top prey for a specified vector of predators
## predator plots look at the top predators for a given vector of prey
## All look at the total consumed biomass over the run to calculate top predators/prey
## diet.method = 'percent' only takes the prey that make up the top 'diet.fract' percent of a predators diet (and vice versa)
## diet.method = 'diet.topn' takes the top 'n' prey from a predators diet

##Important!! Assumes that all output files are in a folder named witht the same prefix of the output files
## Also assumes that you've run RM's preprocess_2 script for all compared runs

run.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Run_Files/'
plot.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Figures/'

#Functional groups file
fgs = read.csv('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_groups.csv',header = T, stringsAsFactors = F)

#Runs you want to compare
run.names = c('atneus_v15_01272020','02042020_NewBivalvePred_3','02062020_PrimProdDiag_1')

#Codes for all groups you want visualized
prey.groups = c('PL')

#Flags on whether to look at diet comparisons or biomass comparisons
make_dietcomp_plots = F
make_biomasscomp_plots = T

diet.method = 'percent'
diet.fract = 0.9
tstart = 20

#Log of biomass for timeseries
log.biomass = T

#Start and end years for timeseries
tstart = 0
tstop = 50

prey.groups.full = fgs$LongName[match(prey.groups,fgs$Code)]

plots.all = list()
biomass.ls = list()

run.names = run.names

for(i in 1:length(run.names)){
  
  load(paste0(run.dir,run.names[i],'/',run.names[i],'_prepro.rdata'))
  biomass_consumed = result$biomass_consumed
  
  #Identify top x prey or top y% of prey consumed
  
  if(diet.method == 'percent'){
    if(!is.na(prey.groups)){
      pred = biomass_consumed %>% 
        filter(pred %in% prey.groups.full & time > tstart) %>%
        group_by(pred,agecl,prey) %>%
        summarize(consumed = sum(atoutput)) %>%
        mutate(pct_consumed = consumed/sum(consumed))%>%
        ## Based on top x num
        # top_n(3) %>%
        arrange(pred,desc(pct_consumed))%>%
        ## Based on cumulative pct
        mutate(cum_consumed = cumsum(pct_consumed)) %>%
        filter(cumsum(cum_consumed>diet.fract)<=1)
    }
    if(!is.na(prey.groups)){
      prey = biomass_consumed %>%
        filter(prey %in% prey.groups.full& time > tstart) %>%
        select(prey,pred,agecl,atoutput) %>%
        group_by(prey,pred) %>%
        summarize(consumed = sum(atoutput)) %>%
        mutate(pct_consumed = consumed/sum(consumed)) %>%
        ## Based on top x num
        # top_n(3) %>%
        arrange(prey,desc(pct_consumed)) %>%
        ## Based on Cumulative pct
        mutate(cum_consumed = cumsum(pct_consumed)) %>%
        filter(cumsum(cum_consumed>diet.fract)<=1)
    }
  } else if(diet.method == 'diet.topn'){
    if(!is.na(prey.groups)){
      pred = biomass_consumed %>% 
        filter(pred %in% prey.groups.full& time > tstart) %>%
        group_by(pred,agecl,prey) %>%
        summarize(consumed = sum(atoutput)) %>%
        mutate(pct_consumed = consumed/sum(consumed))%>%
        top_n(n) %>%
        arrange(pred,desc(pct_consumed))
    }
    
    if(!is.na(prey.groups)){
      prey = biomass_consumed %>%
        filter(prey %in% prey.groups.full& time > tstart) %>%
        select(prey,pred,agecl,atoutput) %>%
        group_by(prey,pred) %>%
        summarize(consumed = sum(atoutput)) %>%
        mutate(pct_consumed = consumed/sum(consumed)) %>%
        top_n(n) %>%
        arrange(prey,desc(pct_consumed))
    } 
    
  }
  
  if(make_dietcomp_plots == T){
    #plot pred and prey as image()
    plots.all[[(2*i)-1]] = ggplot(pred,aes(x=pred,y=prey,fill = pct_consumed))+
      geom_tile()+
      scale_fill_gradient(name = 'Prop. Consumed \nBiomass',low = 'grey80',high = 'black',na.value = 'white')+
      theme_minimal()+
      xlab('Predator')+ylab('Prey')+ggtitle(run.names[i])+
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30)
      )
    plots.all[[(2*i)]] = ggplot(prey,aes(x=pred,y=prey,fill = pct_consumed))+
      geom_tile()+
      scale_fill_gradient(name = 'Prop. Consumed \nBiomass',low = 'grey80',high = 'black',na.value = 'white')+
      theme_minimal()+
      xlab('Predator')+ylab('Prey')+ggtitle(run.names[i])+
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30)
      )
  }
  
  if(make_biomasscomp_plots == T){
    #plot biomass timeseries
    biomass.ls[[i]] = result$biomass %>% 
      filter(species %in% unique(c(prey.groups.full,prey$pred))& time > tstart) %>%
      add_column(run = run.names[i])
  }
  
  print(i)
}

if(make_biomasscomp_plots == T){
  #combine biomass data
  biomass.all = do.call('rbind',biomass.ls)
  if(log.biomass == T){
    biomass.all$atoutput = log(biomass.all$atoutput,10)
  }
  
  #Make biomass plots
  spp.full = unique(biomass.all$species)
  bio.plots = list()
  
  for(i in 1:length(spp.full)){
    sub = biomass.all %>% filter(species == spp.full[i])
    init.bio = sub$atoutput[1]
    sub = sub %>% filter(time >= tstart)
    bio.plots[[i]] = ggplot(data = sub, aes(x = time, y= atoutput,col = run))+
      geom_path(size = 0.5)+
      # geom_hline(aes(yintercept = init.bio),lty = 2)+ 
      annotate('segment',x = tstart, xend = tstop,y = init.bio,yend = init.bio,lty = 2)+
      #ylim(0,max(sub$atoutput))+
      scale_color_manual(name = 'Run Name',values = brewer.pal(length(run.names),'Set1'))+
      ggtitle(spp.full[i])+xlab('Time (yr)')+ylab('Biomass')+
      theme_minimal()+
      theme(
        panel.grid = element_blank()
      )
    #init.bio = ifelse(log.biomass == T,log(sub$atoutput[1],10),sub$atoutput[1])
    
    if(log.biomass == T){
      bio.plots[[i]] = bio.plots[[i]] + ylab('log(Biomass)')
    }
  }
  biomass.plot = do.call('grid.arrange',c(bio.plots))
  ggsave(paste0(plot.dir,' Biomass Diagnostics.png'),biomass.plot, width = 18, height = 10, units = 'in',dpi = 300)
  
} 

if(make_dietcomp_plots == T){
  #Make diet plots
  diet.plot = do.call('grid.arrange',c(plots.all,nrow = length(run.names)))
  ggsave(paste0(plot.dir,' Diet Diagnostics.png'),diet.plot,width = 18, height = 18, units = 'in', dpi = 300)
  
}


