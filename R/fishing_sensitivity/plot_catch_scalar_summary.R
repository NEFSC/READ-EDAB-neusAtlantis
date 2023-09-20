#' Script to generate plots relating to catch scalar scenarios including:
#' 1) delta_biomass ~ catch scalar, by species -> PDF
#' 2) mean_age ~ catch scalar, by species -> PDF
#' 3) delta_numbers ~ catch scalar, by species -> PDF
#' 4) numbers_trend ~ biomass_trend by guild

data.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/data/fscale_combined/'
figure.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/fscale_combined/'

#make some fake data based on the run.index
experiment.id = 'fscale_combined'


ref.run.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/'
data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary_20805_28105.rds'))
proj.start = 20805

fgs.file = here::here('currentVersion','neus_groups.csv')
                          

plot_catch_scalar_summary = function(data.dir,figure.dir,setup.df,ref.run.dir,ref.time,fgs.file){
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(gridExtra)
  library(ggrepel)
  
  fgs = read.csv(fgs.file,as.is =T) %>%select(Code,LongName,IsTurnedOn)
  
  setup.df = read.csv(here::here('diagnostics','scenario_db',paste0(experiment.id,'_setup.csv')),as.is = T)
  
  # setup.df$target.species = sapply(setup.df$Run,function(x) return(strsplit(x,split=paste0(experiment.id,'|_'))[[1]][3]),USE.NAMES = F)
  mig.groups = read.csv(here::here('currentVersion','neus_migrations_orig.csv'),as.is=T)$GroupCode
  
  setup.df = setup.df %>% select(run.id,scalar,target.species)
  
  data.ref = readRDS(paste0(ref.run.dir,'Post_Processed/Data/ref_run_summary_20805_28105.rds'))%>%
    rename(biomass.age.mean.ref = 'biomass.age.mean')%>%
    mutate(biomass.ref.plot = ifelse(Code %in% mig.groups,biomass.ref.max,biomass.ref.mean),
           number.ref.plot = ifelse(Code %in% mig.groups,number.ref.max,number.ref.mean))
  
  #Read in biomass and numbers data

  biomass = readRDS(paste0(data.dir,'scenario_stats_biomass.rds'))%>%
    mutate(biomass.plot = ifelse(species %in% mig.groups,biomass.max,biomass.mean))
  
  numbers = readRDS(paste0(data.dir,'scenario_stats_numbers.rds'))%>%
    mutate(number.plot = ifelse(species %in% mig.groups,number.max,number.mean))
  
  data.run = biomass %>%
    left_join(numbers)%>%
    tidyr::separate(run.name,c('dum','ID'),'_',remove =F )%>%
    mutate(ID = as.numeric(ID))%>%
    rename(Code = 'species')%>%
    left_join(setup.df,by = c('run.name' = 'run.id'))%>%
    left_join(fgs)%>%
    filter(IsTurnedOn == 1)
    
  #Combine run output and ref data
  data.all = data.run %>% 
    left_join(data.ref, by = 'Code') %>%
    mutate(biomass.rel = biomass.plot/biomass.ref.plot,
           number.rel = number.plot/number.ref.plot)%>%
    left_join(fgs)%>%
    filter(IsTurnedOn == 1)%>%
    filter(scalar %in% c(2,5,10,25,50,100))
  
  spp.names = sort(unique(data.run$Code))
  
  biomass.plot.name = paste0(figure.dir,'biomass_catch_scalar_species.pdf')
  number.plot.name = paste0(figure.dir,'numbers_catch_scalar_species.pdf')
  biomass.number.plot.name = paste0(figure.dir,'biomass_v_numbers_catch_scalar_species.pdf')
  number.age.plot.name = paste0(figure.dir,'numbers_age_catch_scalar_species.pdf')
  biomass.age.plot.name = paste0(figure.dir,'biomass_age_catch_scalar_species.pdf')
  
  biomass.plot = number.plot = biomass.number.plot = number.age.plot = biomass.age.plot = list()
  
  # biomass.numbers = data.frame(Code = spp.names,biomass.slope = NA,biomass.int = NA, number.slope = NA, number.int = NA)
  biomass.numbers = data.frame(Code = spp.names,biomass.a = NA, biomass.b = NA, biomass.c = NA,  biomass.d = NA,number.a = NA, number.b = NA, number.c = NA,number.d = NA)

  i=6
  for(i in 1:length(spp.names)){
  
    data.species = data.all %>% 
      filter(Code == spp.names[i] & Code == target.species)%>%
      arrange(scalar)
      
    data.species.nozero = data.species %>%
      filter(scalar !=0)%>%
      select(Code,target.species,LongName,scalar,biomass.rel,number.rel)
    
    dum.line = data.species.nozero[1,]
    dum.line$scalar = 1
    dum.line$biomass.rel = 1
    dum.line$number.rel = 1
    
    data.species.nozero = bind_rows(dum.line,data.species.nozero)
    
    if(nrow(data.species)== 0| all(is.na(data.species$biomass.ref.mean))){
      biomass.numbers$biomass.a[i] = NA
      biomass.numbers$biomass.b[i] = NA
      biomass.numbers$biomass.c[i] = NA
      biomass.plot[[i]] = NULL
      biomass.age.plot[[i]] = NULL
      number.age.plot[[i]] = NULL
      number.plot[[i]] = NULL
      next()
    }else{
      #fit a linear model to biomass and numbers vs fishing scalar and extract the slopes
      # b.lm = lm(biomass.rel.log~scalar.log,data.species.nozero)
      b.nls = try(nls(biomass.rel ~ (1/(a+b*scalar^c))+d,data.species.nozero, start = list(a = 1,b = 0.1, c= 2,d=min(data.species.nozero$biomass.rel)),lower = c(0,0,0,0), alg = 'port'), silent = T)
      if(class(b.nls)== 'try-error'){
        b.lm = lm(biomass.rel~ scalar,data.species.nozero)
        biomass.numbers$biomass.a[i] = coef(b.lm)[2]
        biomass.numbers$biomass.b[i] = coef(b.lm)[1]
        
        a = coef(b.lm)[2]
        b = coef(b.lm)[1]
        
        biomass.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= biomass.rel))+
          geom_point()+
          geom_path()+
          stat_function(fun = function(x,a,b) b + x*a,lty = 2, args = list(a = a, b = b))+
          ggtitle(data.species$LongName[1])+
          geom_hline(yintercept = 0,lty =2)+
          theme_bw()+
          xlab('Fishing Scalar')+
          ylab('Relative Biomass')+
          scale_x_continuous(breaks = data.species$scalar)+
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.minor = element_blank())
        
      }else{
        biomass.numbers$biomass.a[i] = coef(b.nls)[1]
        biomass.numbers$biomass.b[i] = coef(b.nls)[2]
        biomass.numbers$biomass.c[i] = coef(b.nls)[3]
        biomass.numbers$biomass.d[i] = coef(b.nls)[4]
        
        a = coef(b.nls)[1]
        b= coef(b.nls)[2]
        c = coef(b.nls)[3]
        d = coef(b.nls)[4]
        
        
        biomass.plot[[i]] = ggplot(data = data.species.nozero, aes(x= scalar, y= biomass.rel))+
          geom_point()+
          geom_path()+
          stat_function(fun = function(x,a,b,c,d) (1/(a + b*x^c))+d,lty = 2, args = list(a = a, b=b ,c =c,d = d))+
          ggtitle(data.species$LongName[1])+
          geom_hline(yintercept = 0,lty =2)+
          theme_bw()+
          xlab('Fishing Scalar')+
          ylab('Relative Biomass')+
          scale_x_continuous(breaks = data.species$scalar)+
          theme(plot.title = element_text(hjust = 0.5),
                panel.grid.minor = element_blank())
        
      }
      
      # print(biomass.plot[[i]])
      # print(i)
      # plot(biomass.rel~scalar,data.species.nozero)
      # curve(1/(1+0.01*x^0),0,100)
      # predict.x = seq(0,100,1)
      # lines(x = predict.x, y = 1/(coef(b.nls)[1]+coef(b.nls)[2]*predict.x^coef(b.nls)[3]))
      
      #Do biomass vs fishing scalar

    }
    

    
    if(all(is.na(data.species$number.ref.mean))){
      biomass.numbers$number.a[i] = NA
      biomass.numbers$number.b[i] = NA
      biomass.numbers$number.c[i] = NA
      biomass.age.plot[[i]] = NULL
      number.age.plot[[i]] = NULL
      number.plot[[i]] = NULL
      next()
    }else{

      n.nls = try(nls(number.rel ~ (1/(a+b*scalar^c))+d,data.species.nozero, start = list(a = 1,b = 0.1, c= 2,d = min(data.species.nozero$number.rel)),lower = c(0,0,0,0), alg = 'port'), silent = T)
      if(class(n.nls)== 'try-error'){
        n.lm = lm(number.rel~ scalar,data.species.nozero)
        biomass.numbers$number.a[i] = coef(n.lm)[2]
        biomass.numbers$number.b[i] = coef(n.lm)[1]
        
        a = coef(n.lm)[2]
        b = coef(n.lm)[1]

        number.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= number.rel))+
          geom_point()+
          geom_path()+
          stat_function(fun = function(x,a,b) b + x*a,lty = 2, args = list(a = a, b = b))+
          ggtitle(data.species$LongName[1])+
          theme_bw()+
          xlab('Fishing Scalar')+
          ylab('Relative Numbers')+
          theme(plot.title = element_text(hjust = 0.5))

      }else{
        biomass.numbers$number.a[i] = coef(n.nls)[1]
        biomass.numbers$number.b[i] = coef(n.nls)[2]
        biomass.numbers$number.c[i] = coef(n.nls)[3]
        biomass.numbers$number.d[i] = coef(n.nls)[4]
        
        a = coef(n.nls)[1]
        b = coef(n.nls)[2]
        c = coef(n.nls)[3]
        d = coef(n.nls)[4]

        number.plot[[i]] = ggplot(data = data.species.nozero, aes(x= scalar, y= number.rel))+
          geom_point()+
          geom_path()+
          stat_function(fun = function(x,a,b,c,d) (1/(a + b*x^c))+d,lty = 2, args = list(a = a, b=b ,c =c,d = d))+
          ggtitle(data.species$LongName[1])+
          theme_bw()+
          xlab('Fishing Scalar')+
          ylab('Relative Numbers')+
          theme(plot.title = element_text(hjust = 0.5))
      }
      # plot(number.rel~scalar,data.species.nozero)
      # lines(x = predict.x, y = 1/(coef(n.nls)[1]+coef(n.nls)[2]*predict.x^coef(n.nls)[3]))

      #Do mean age (abundance-weighted) vs fishing scalar
      number.age.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= number.age.mean))+
        geom_point()+
        ylim(0,10)+
        # geom_smooth(method = 'lm')+
        ggtitle(data.species$LongName[1])+
        theme_bw()+
        xlab('Fishing Scalar')+
        ylab('Mean age (abundance-weighted)')+
        theme(plot.title = element_text(hjust = 0.5))


      #Do mean age (biomass-weighted) vs fishing scalar
      biomass.age.plot[[i]] = ggplot(data = data.species, aes(x= scalar, y= biomass.age.mean))+
        geom_point()+
        geom_path()+
        ylim(0,10)+
        # geom_smooth(method = 'lm')+
        ggtitle(data.species$LongName[1])+
        theme_bw()+
        xlab('Fishing Scalar')+
        ylab('Mean age (biomass-weighted)')+
        theme(plot.title = element_text(hjust = 0.5))
    }

  }
  
  saveRDS(biomass.numbers,paste0(data.dir,'biomass_numbers_fit.rds'))
  
  
  if(!dir.exists(figure.dir)){
    dir.create(figure.dir)
  }
  pdf(biomass.plot.name)
  for(i in 1:length(biomass.plot)){
    if(is.null(biomass.plot[[i]])){next()}
    print(biomass.plot[[i]])
  }
  # do.call('grid.arrange',biomass.plot)
  dev.off()
  
  pdf(number.plot.name)
  for(i in 1:length(number.plot)){
    if(is.null(number.plot[[i]])){next()}
    grid.arrange(number.plot[[i]])}
  dev.off()
  
  pdf(biomass.age.plot.name)
  for(i in 1:length(biomass.age.plot)){
    if(is.null(biomass.age.plot[[i]])){next()}
    grid.arrange(biomass.age.plot[[i]])}
  dev.off()
  
  pdf(number.age.plot.name)
  for(i in 1:length(number.age.plot)){
    if(is.null(number.age.plot[[i]])){next()}
    grid.arrange(number.age.plot[[i]])}
  dev.off()
  
  #plot biomass slope vs numbers slope and color code by guild
  
  spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),as.is = T) %>% select(Code, Guild)
  
  data.slope = biomass.numbers %>% 
    filter(!is.na(number.c))%>%
    left_join(spp2guild)%>%
    left_join(data.ref)
    
  
  ggplot(data = data.slope, aes( x= biomass.c, y = number.c, color = Guild, label = Code))+
    geom_point(size = 5, alpha = 0.5)+
    geom_hline(yintercept = 0, lty = 2)+
    geom_vline(xintercept = 0,lty =2)+
    stat_function(fun = function(x) x,lty = 1, color = 'black')+
    geom_text_repel(max.overlaps = 30)+
    xlab('log (biomass slope)')+
    ylab('log (numbers slope)')+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/numbers_biomass_guild.png'))

  ggplot(data = data.slope, aes(x = exploit.prop,y = biomass.c, color = Guild, label = Code))+
    geom_point(alpha = 0.5,size = 5)+
    geom_text_repel()+
    xlab('Exploitation Rate')+
    ylab('Fishing Sensitivity')+
    theme_bw()+
    theme(legend.position = 'bottom')
    ggsave(paste0(figure.dir,'/biomass_exploitation_guild.png'))
    
  ggplot(data = data.slope, aes(x = recruit.ref,y = biomass.c, color = Guild, label = Code))+
      geom_point(alpha = 0.5,size = 5)+
      geom_text_repel()+
      xlab('Reruitment')+
      ylab('Fishing Sensitivity')+
      theme_bw()+
      theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/recruitment_exploitation_guild.png'))
    
  ggplot(data = data.slope, aes(x = biomass.ref.plot ,y = biomass.c, color = Guild, label = Code))+
    geom_point(alpha = 0.5,size = 5)+
    geom_text_repel()+
    xlab('Reference Biomass')+
    ylab('Fishing Sensitivity')+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/reference_biomass_exploitation_guild.png'))
  
  ggplot(data = data.slope, aes(x = catch.ref,y = biomass.c, color = Guild, label = Code))+
    geom_point(alpha = 0.5,size = 5)+
    geom_text_repel()+
    xlab('Reference Catch')+
    ylab('Fishing Sensitivity')+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'/reference_catch_exploitation_guild.png'))
    
  #plot maximum scalar
  spp.names2 = sort(unique(data.all$target.species))
  guild2spp = read.csv(here::here('diagnostics','functional_groups_match.csv')) %>%
    select(Code,LongName,Guild)
  
  
  data.thresh = data.frame(Code = spp.names2,max.scalar = NA)
  for(i in 1:length(spp.names2)){
    
    data.spp = data.all %>% 
      filter(target.species == spp.names2[i] & Code == spp.names2[i])%>%
      mutate(bio.prop = biomass.plot/biomass.ref.plot)%>%
      filter(bio.prop >= 0.1)
    
    data.thresh$max.scalar[i] =  max(data.spp$scalar,na.rm=T)
  }
  data.thresh = data.thresh %>%
    left_join(fgs)%>%
    left_join(guild2spp)%>%
    left_join(data.ref)
  
  ggplot(data = data.thresh,aes(x= reorder(LongName,max.scalar),y=max.scalar,fill = Guild))+
    geom_bar(stat = 'identity')+
    guides(fill= guide_legend(title.position = 'left',nrow = 1))+
    ylab('Max Scalar that Persists')+
    xlab('')+
    coord_flip()+
    theme_bw()+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'max_scalar_guild.png'),width =10,height =8, units = 'in',dpi =300)
  
  ggplot(data = data.thresh, aes(x= exploit.prop,y = max.scalar, color = Guild))+
    geom_point(size = 3)+
    theme_bw()+
    guides(color = guide_legend(nrow =1,title = ''))+
    xlab('Fishing Mortality')+
    ylab('Maximum Fishing Multiplier')+
    theme(legend.position = 'bottom')
  ggsave(paste0(figure.dir,'max_scalar_Exploitation_guild.png'))
  
  thresh.prop = data.thresh %>%
    group_by(max.scalar)%>%
    summarise(N =n())%>%
    mutate(pct = N/nrow(data.thresh))
    
  thresh.prop$cum.pct = cumsum(thresh.prop$pct)
  
  nrow(data.thresh %>% filter(exploit.prop < 0.01))/nrow(data.thresh)
}
