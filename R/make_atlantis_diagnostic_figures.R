#' Creates standard diagnostic figures for Atlantis output
#'
#'Uses the "result" object output from process_atl_output() to create the full set of diagnostic and summary 
#'figures and tables from atlantis model run. 
#'
#'@out.dir string. Path where desired output tables and figures should be saved
#'@atl.dir string. Path where atlantis model output is located
#'@param.dir string. Path where atlantis parameter files are located
#'@run.prefix sring. Prefix specified in Atlantis.bat file that begins each output file
#'@run.name string. Prefered name to use for output file names
#'@result list. Output produced by process_atl_output(). Default name is 'result'. Should be loaded in first
#'@param.ls list. Output from get_atl_paramfiles
#'
#'@benthic.box numeric. Box ID for benthic plots
#'@benthic.level numeric. Level for benthic plots (default is 4 for NEUS model)
#'
#'@phytopl.history string. Full file name for phytoplankton historical data. Columns: (Time, PL.ts, PS.ts, DF.ts), Biomass is domain-wide in tonnes
#'@zoopl.history string. Full file name for zooplankton historical data. Columsn: (Time, ZL, ZM, ZS, ZG). Biomass is domain-wide in tonnes
#'
#'@plot.benthic logical. Benthic plots show timeseries of all benthic and epibenthic groups for one box
#'@plot.overall.biomass logical. Plots showing the total biomass across all functional groups as stacked barplots
#'@plot.biomass.timeseries logical. Plots showing biomass-related timeseries on various aggregations  and reference points
#'@plot.length.age logical. Plots relating to the length-age relationship of age-structured groups
#'@plot.biomass.box logical. Plots relating to biomass but grouped by box
#'@plot.c.mum logical. Plots and tables related to tuning C and mum parameters
#'@plot.sn.rn logical. Plots relating to SN and RN timeseries
#'@plot.recruits logical. Plots of recruitment and SSB timeseries
#'@plot.numbers.timeseries logical. Plots showing timeseries of numbers (as opposed to biomass)
#'@plot.physics logical. Plots of physical statevariables as well as fluxes
#'@plot.growth.cons logical. Plots relating to growth and consumption 
#'@plot.cohort logical. Plots showing timeseries of each cohort across age-structured groups
#'@plot.diet logical. Plots showing predation of and consumption by each functional group
#'@plot.spatial.biomass logical. Plots showing the spatial (box/level) structure of groups' biomass
#'@plot.spatial.biomass.seasonal logical. Plots showing the spatial (box/level) structure of groups' biomass
#'@plot.LTL logical. Plots comparing LTL groups (domain-wide) to data
#'@plot.catch logical. Plots annual catch(mt) age based catch (numbers) and age based %ages
#'@plot.max.weight logical. Plots the maximum size of fish in each size class over the domain
#'@plot.mortality logical. Plots Mortality (F, M1, M2) from two output sources (Mort, SpecificMort)


#'
#'@return A series of figures and tables based on output grouping flags
#' 
#' Author: Ryan Morse, modified by J. Caracappa
#' 


make_atlantis_diagnostic_figures = function(
  out.dir,
  fig.dir,
  atl.dir,
  param.dir,
  run.prefix,
  run.name,
  
  param.ls,
  
  benthic.box,
  benthic.level= 4,
  
  phytopl.history,
  zoopl.history,
  
  plot.all, 
  
  plot.benthic,
  plot.overall.biomass,
  plot.biomass.timeseries,
  plot.length.age,
  plot.biomass.box,
  plot.c.mum,
  plot.sn.rn,
  plot.recruits,
  plot.numbers.timeseries,
  plot.physics,
  plot.growth.cons,
  plot.cohort,
  plot.diet,
  plot.consumption,
  plot.spatial.biomass,
  plot.spatial.biomass.seasonal,
  plot.LTL,
  plot.catch, 
  plot.max.weight,
  plot.mortality
){
  
  `%>%` = dplyr::`%>%`
  #Utility function
  add.title = function(plot,title){
    plot = plot + ggplot2::ggtitle(title)+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    return(plot)
  }
  
  #Load groups data
  group.code = atlantistools::get_age_acronyms(param.ls$groups.file)
  group.data = atlantistools::load_fgs(param.ls$groups.file)
  group.index = dplyr::select(group.data,c(Code,LongName))
  
  
  #Load BGM data
  box.bgm = atlantistools::load_box(param.ls$bgm)
  
  #plot parameters
  plot.labels = list(x = 'Time (years)',y = 'Biomass (tonnes)')
  
  
  # Benthic box timeseries --------------------------------------------------
  
  
  #Select box for timeseries of all benthic groups
  if(plot.benthic |plot.all){
    biomass.spatial.stanza = readRDS(paste0(out.dir,'biomass_spatial_stanza.rds'))
    benthic.biomass.spatial = dplyr::filter(biomass.spatial.stanza, layer == benthic.level & polygon == benthic.box)
    benthic.spp = unique(benthic.biomass.spatial$species)
    box.area = box.bgm$boxes[[benthic.box+1]]$area
    
    pdf(file = paste0(fig.dir,run.name, ' Box_',benthic.box,'_benthos.pdf'))
    for(i in 1:length(benthic.spp)){
      spp.ind = benthic.spp[i]
      benthic.biomass.spp = dplyr::filter(benthic.biomass.spatial,species == spp.ind)
      plot(benthic.biomass.spp$atoutput/box.area~benthic.biomass.spp$time,
           type= 'l',main = paste(spp.ind,'Box',benthic.box),ylab = 'Mg N m-2',xlab = 'Time')
    }
    dev.off()
    
    rm(biomass.spatial.stanza)
  }
  
  # Catch Timeseries ------------------------------------------------------
  
  if(plot.catch|plot.all) {
    
    catchmt = readRDS(paste0(out.dir,'catchmt.rds'))
    
    #Catch by species time series (metric tonnes)
    temp.plot.1 = atlantistools::plot_line(catchmt)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = plot.labels)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = list(x='Time (years)', y = 'Metric Tonnes'))
    temp.plot.1 = add.title(temp.plot.1,'Catch')
    
    #Catch at age time series (numbers)
    
    totcatch = readRDS(paste0(out.dir,'totcatch.rds'))
    
    temp.plot.2 = atlantistools::plot_line(totcatch, col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(p = temp.plot.2, labels = c(plot.labels, list(colour = 'Ageclas')))
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,labels = list(x='Time (years)', y = 'Numbers'))
    temp.plot.2 = add.title(temp.plot.2,'Catch at Age')
    
    #Catch at age - percent
    catch.age.pct = atlantistools::agg_perc(totcatch, groups = c('time','species'))
    temp.plot.6 = atlantistools::plot_bar(catch.age.pct, fill = 'agecl', wrap = 'species')
    temp.plot.6 = ggplot2::update_labels(temp.plot.6,labels = list(x='Time (years)', y = 'Numbers (%)'))
    temp.plot.6 = add.title(temp.plot.6, 'Catch at age - Percent')
    
    pdf(paste0(fig.dir,run.name,' Catch Timeseries.pdf'),width = 20, height = 20, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.6)
    dev.off()
    
    rm(totcatch,catchmt)
  }
  
  # Mortality Timeseries ------------------------------------------------------
  
  if(plot.mortality|plot.all) {
    
    # plot mortality from Mort.txt
    mort = readRDS(paste0(out.dir,'mort.rds'))
    itype <- 1
    plotMort <- list()
    # Annual Mortality time series M, F by species on same plot
    temp.plot.1 = atlantistools::plot_line(mort,col="source")
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = list(x='Time (years)', y = 'Mortality'))
    temp.plot.1 = add.title(temp.plot.1,'Mortality (F & M2)')
    plotMort[[itype]] <- temp.plot.1
    
    # plot mortaliy from specificMort.txt
    specificmort <- readRDS(paste0(out.dir,'specificmort.rds'))
    
    # plot absolute mortality. 3 pages, one page for F,M1,M2
    for (atype in rev(unique(specificmort$mort))) {
      itype <- itype + 1
      mort <- specificmort %>% 
        dplyr::filter(mort == atype)
      temp.plot = atlantistools::plot_line(mort, col = 'agecl')
      temp.plot = ggplot2::update_labels(p = temp.plot, labels = c(list(x='Time (years)', y = 'Mortality'), list(colour = 'Ageclas')))
      temp.plot = add.title(temp.plot,paste0('Mortality at Age (',atype,')'))
      
      plotMort[[itype]] <- temp.plot
      
    }
    
    # plot relative mortality by age class
    # select species with 10 age classes
    for (iage in 1:max(specificmort$agecl)) {
      mortality <- specificmort %>%
        dplyr::filter(code %in% atlantistools::get_cohorts_acronyms(param.ls$groups.file,numCohorts = 10)) %>%
        dplyr::filter(agecl == iage)
      
      pct = atlantistools::agg_perc(mortality, groups = c('time','species'))
      temp.plot = atlantistools::plot_bar(pct, fill = 'mort', wrap = 'species')
      temp.plot = ggplot2::update_labels(temp.plot,labels = list(x='Time (years)', y = 'Rate (proportion)'))+
        ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
      temp.plot = add.title(temp.plot, paste0("Relative Mortality Rates for species with 10 age classes (Age ",iage, ")"))
      
      plotMort[[itype + iage]] <- temp.plot
    }
    
    # select species with 2 age classes
    for (i2age in 1:2) {
      mortality <- specificmort %>%
        dplyr::filter(code %in% atlantistools::get_cohorts_acronyms(param.ls$groups.file,numCohorts = 2)) %>%
        dplyr::filter(agecl == i2age)
      
      pct = atlantistools::agg_perc(mortality, groups = c('time','species'))
      temp.plot = atlantistools::plot_bar(pct, fill = 'mort', wrap = 'species')
      temp.plot = ggplot2::update_labels(temp.plot,labels = list(x='Time (years)', y = 'Rate (proportion)'))+
        ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
      temp.plot = add.title(temp.plot, paste0("Relative Mortality Rates for species with 2 age classes (Age ",i2age, ")"))
      
      plotMort[[itype + iage + i2age]] <- temp.plot
    }
    
    # select species with 1 age classes (Biomass pool)
    mortality <- specificmort %>%
      dplyr::filter(code %in% atlantistools::get_cohorts_acronyms(param.ls$groups.file,numCohorts = 1)) %>%
      dplyr::filter(agecl == 1)
    
    pct = atlantistools::agg_perc(mortality, groups = c('time','species'))
    temp.plot = atlantistools::plot_bar(pct, fill = 'mort', wrap = 'species')
    temp.plot = ggplot2::update_labels(temp.plot,labels = list(x='Time (years)', y = 'Rate (proportion)'))+
      ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
    temp.plot = add.title(temp.plot, paste0("Relative Mortality Rates for species with 1 age class (Age 1) "))
    
    plotMort[[itype + iage + i2age + 1]] <- temp.plot
    
    
    
    pdf(paste0(fig.dir,run.name,' Specific Mortality Timeseries.pdf'),width = 20, height = 20, onefile = T)
    for (iplot in 1:(itype+iage+i2age+1)) {
      gridExtra::grid.arrange(plotMort[[iplot]])
    }
    dev.off()
    
    
    rm(mort,specificmort)
  } 
  
  # Overall biomass ---------------------------------------------------------
  
  
  #Make overall biomass plot (stacked barplot of total biomass domain-wide)
  if(plot.overall.biomass|plot.all){
    biomass = readRDS(paste0(out.dir,'biomass.rds'))
    
    #combine threshold = 10
    biomass.df.10 = atlantistools::combine_groups(biomass, group_col = 'species', combine_thresh = 10)
    temp.plot.1 = atlantistools::plot_bar(biomass.df.10)
    temp.plot.1 = temp.plot.1 + ggplot2::ggtitle('Top 10 Groups')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1, labels = plot.labels)
    
    #Combine threshold = 20
    biomass.df.20 = atlantistools::combine_groups(biomass, group_col = 'species', combine_thresh = 20)
    temp.plot.2 = atlantistools::plot_bar(biomass.df.20)
    temp.plot.2 = temp.plot.2 + ggplot2::ggtitle('Top 20 Groups')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, labels = plot.labels)
    
    pdf(file = paste0(fig.dir,run.name,' overall biomass.pdf'),width = 12, height = 6, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    dev.off()
  }
  
  
  # Biomass Timeseries ------------------------------------------------------
  
  
  #Make biomass timeseries plots
  if(plot.biomass.timeseries|plot.all){
    
    biomass = readRDS(paste0(out.dir,'biomass.rds'))
    
    #biomass by species timeseries
    temp.plot.1 = atlantistools::plot_line(biomass)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = plot.labels)
    temp.plot.1 = add.title(temp.plot.1,'Biomass')
    
    #biomass at age timeseries
    biomass.age = readRDS(paste0(out.dir,'biomass_age.rds'))
    
    temp.plot.2 = atlantistools::plot_line(biomass.age, col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(p = temp.plot.2, labels = c(plot.labels, list(colour = 'Ageclas')))
    temp.plot.2 = add.title(temp.plot.2,'Biomass at Age')
    
    #biomass at age relative to initial biomass timeseries
    rel.biomass.age = atlantistools::convert_relative_initial(biomass.age)
    temp.plot.3 = atlantistools::plot_line(rel.biomass.age, col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3,list(x = 'Time (years)', y = expression(biomass/bio[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3,'Biomass at Age Relatative to Initial Biomass')
    
    #Biomass vs Bio init
    rel.biomass = atlantistools::convert_relative_initial(biomass)
    temp.plot.4 = atlantistools::plot_line(rel.biomass)
    temp.plot.4 = ggplot2::update_labels(temp.plot.4,list(x = 'Time (years)',y = expression (Biomass/Biomass[init])))
    temp.plot.4 = atlantistools::plot_add_box(temp.plot.4)
    temp.plot.4 = add.title(temp.plot.4,'Biomass Relatative to Initial Biomass')
    
    #Invert bio timeseries
    biomass.age.invert = readRDS(paste0(out.dir,'biomass_age_invert.rds'))
    
    temp.plot.5 = atlantistools::plot_line(biomass.age.invert)
    temp.plot.5 = ggplot2::update_labels(temp.plot.5, labels = plot.labels)
    temp.plot.5 = add.title(temp.plot.5,'Invert Biomass')
    
    #Bio at age
    bio.age.pct = atlantistools::agg_perc(biomass.age, groups = c('time','species'))
    temp.plot.6 = atlantistools::plot_bar(bio.age.pct, fill = 'agecl', wrap = 'species')
    temp.plot.6 = ggplot2::update_labels(temp.plot.6,labels = list(x='Time (years)', y = 'Numbers (%)'))
    temp.plot.6 = add.title(temp.plot.6, 'Biomass at age - Percent')
    
    pdf(paste0(fig.dir,run.name,' biomass timeseries.pdf'),width = 20, height = 20, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    gridExtra::grid.arrange(temp.plot.5)
    gridExtra::grid.arrange(temp.plot.6)
    dev.off()
    
    rm(biomass,biomass.age,biomass.age.invert)
  }
  
  
  # Length-age plots --------------------------------------------------------
  
  
  #Make length.age plots
  if(plot.length.age|plot.all){
    
    length.age = readRDS(paste0(out.dir,'length_age.rds'))
    
    #Length at age ts by spp
    # init.length.old = read.csv(paste0(param.dir,'/vertebrate_init_length_cm.csv'),header =T, stringsAsFactors = F)
    init.length = read.csv(paste0(param.dir,'/vertebrate_init_length_cm_Adjusted.csv'),header =T, stringsAsFactors = F)%>%
      select(Code,species,agecl,new.length.ref)%>%
      tidyr::spread(agecl,new.length.ref)
    init.length = init.length[order(init.length$species),]
    spp.names = unique(length.age$species)
    
    pdf(file = paste0(fig.dir,run.name,' tuning length at age by species.pdf'),width = 12, height = 6, onefile = T)
    for(x in 1:length(spp.names)){
      spp.id = spp.names[x]
      length.age.spp = dplyr::filter(length.age, species == spp.id & time >0)
      init.length.age.spp = dplyr::filter(init.length[,2:12],species == spp.id)
      boxplot(length.age.spp$atoutput ~ length.age.spp$agecl, ylab = 'cm',xlab = 'cohort',
              main = spp.id, ylim = c(0,max(length.age.spp$atoutput)))
      points(seq(1:10),init.length.age.spp[2:11],col = 'red',pch= 17)
    }
    dev.off()
    
    #Length age together
    temp.plot.1 = atlantistools::plot_line(length.age,col = 'agecl')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = c(x = 'Time (years)',y = 'Length (cm)', colour = 'Ageclass'))
    temp.plot.1 = add.title(temp.plot.1,'Length-at-age')
    
    #Length at age vs. length init
    rel.length.age = atlantistools::convert_relative_initial(length.age)
    temp.plot.2 = atlantistools::plot_line(rel.length.age,col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,list(x='Time (years)',y = expression(length/length[init])))
    atlantistools::plot_add_box(temp.plot.2)
    temp.plot.2 = add.title(temp.plot.2,'Length at Age vs. Initial Length')
    
    pdf(file = paste0(fig.dir,run.name,' Length at Age Timeseries.pdf'),width = 16, height = 16, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    dev.off()
    
    rm(length.age)
  }
  
  # Max weight by age class
  if(plot.max.weight|plot.all){
    maxSize <- readRDS(paste0(out.dir,'max_weight.rds'))
    maxSize <- maxSize %>% 
      dplyr::group_by(species,agecl) %>%
      dplyr::summarize(mm=max(maxMeanWeight)/1000,.groups="drop") %>% # convert to kilograms
      dplyr::mutate(agecl = as.factor(agecl))
    
    weight.plot <- atlantistools:::custom_map(data = maxSize, x = "agecl", y = "mm") +
      ggplot2::geom_bar(stat = "identity") +
      atlantistools::theme_atlantis()
    weight.plot <- atlantistools:::custom_wrap( weight.plot, col = "species", ncol = 7)
    
    weight.plot <- atlantistools:::ggplot_custom( weight.plot) +
      ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) 
    weight.plot <- ggplot2::update_labels( weight.plot,labels = list(x='Age Class', y = 'Weight (Kg)'))
    weight.plot <-  add.title( weight.plot, paste0("Maximum Weight"))
    weight.plot <-  weight.plot + ggplot2::scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","9","10"))
    
    
    pdf(file = paste0(fig.dir,run.name,' Max Weight.pdf'),width = 20, height = 20, onefile = T)
    gridExtra::grid.arrange(weight.plot)
    dev.off()
    
    
    rm(maxSize)
  }
  
  
  
  # Biomass box plots -------------------------------------------------------
  
  
  #Make Biomass Box plots
  if(plot.biomass.box|plot.all){
    
    biomass.box = readRDS(paste0(out.dir,'biomass_box.rds'))
    #Bio per box
    temp.plot.1 = atlantistools::plot_line(biomass.box)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,plot.labels)
    temp.plot.1 = atlantistools::custom_grid(temp.plot.1, grid_x = 'polygon',grid_y = 'species')
    temp.plot.1 = add.title(temp.plot.1,'Biomass by Box')
    
    #Invert bio by box
    biomass.box.invert = readRDS(paste0(out.dir,'biomass_box_invert.rds'))
    temp.plot.2 = atlantistools::plot_line(biomass.box.invert)
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,plot.labels)
    temp.plot.2 = atlantistools::custom_grid(temp.plot.2,grid_x = 'polygon',grid_y = 'species')
    temp.plot.2 = add.title(temp.plot.2, 'Invert Biomass by Box')
    
    pdf(file = paste0(fig.dir,run.name,' Biomass Box Timeseries.pdf'),width = 48, height = 60, onefile =T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    dev.off()
    
    rm(biomass.box,biomass.box.invert)
  }
  
  
  # C/Mum tuning ------------------------------------------------------------
  
  #C_Mum tuning
  if(plot.c.mum|plot.all){
    
    #Data processing
    
    #mum by age
    mum.age = atlantistools::prm_to_df_ages(param.ls$biol.prm, param.ls$groups.file,group = group.code,parameter = 'mum')
    mum.age = tidyr::spread(mum.age,agecl,mum)
    mum.age = dplyr::left_join(mum.age,group.index, by = c('species'='LongName'))
    
    #C by age
    C.age = atlantistools::prm_to_df_ages(param.ls$biol.prm,param.ls$groups.file,group = group.code,parameter = 'C')
    C.age = tidyr::spread(C.age,agecl,c)
    C.age = dplyr::left_join(C.age,group.index,by = c('species' = 'LongName'))
    
    #Initial length
    init.length = read.csv(paste0(param.dir,'/vertebrate_init_length_cm.csv'),header =T, stringsAsFactors = F)
    init.length = init.length[order(init.length$Long.Name),]
    
    ## RM "scale mum and C to length at age relative to initial conditions"
    length.age = readRDS(paste0(out.dir,'length_age.rds'))
    length.age.mn = dplyr::group_by(length.age,species,agecl)
    length.age.mn = dplyr::summarise(length.age.mn, avg = mean(atoutput))
    length.age.mn = tidyr::spread(length.age.mn,agecl,avg)
    
    #Mean length at age divided by initial length at age
    #Used to scale mum and C
    match.id = which(!(init.length$Long.Name %in% length.age.mn$species))
    init.length = init.length[-match.id,]
    length.v.length.init = length.age.mn[,2:11]/init.length[,4:13]
    row.names(length.v.length.init) =init.length$Code
    
    #Scale mum and C by difference between length at age relative to initial conditions
    mum.age = mum.age[-match.id,]
    mum.scale = mum.age[,2:11]/length.v.length.init
    rownames(mum.scale) = mum.age$Code
    mum.scale = mum.scale[order(row.names(mum.scale)),]
    
    C.age = C.age[-match.id,]
    C.scale = C.age[,2:11]/length.v.length.init
    row.names(C.scale) = C.age$Code
    C.scale = C.scale[order(row.names(C.scale)),]
    
    #Write length-scaled C and mum to file
    write.csv(mum.scale, file = paste0(fig.dir,run.name,'newMum_lengthbased.csv'),row.names =T)
    write.csv(C.scale,file = paste0(fig.dir,run.name,'newC_lengthbased.csv'),row.names = T)
    
    ### Also scale mum/C relative to RN vs RN init
    RN.age = readRDS(paste0(out.dir,'RN_age.rds'))
    RN.mn = dplyr::group_by(RN.age,species,agecl)
    RN.mn = dplyr::summarize(RN.mn,avg = mean(atoutput))
    RN.mn = tidyr::spread(RN.mn,agecl,avg)
    
    RN.init = dplyr::filter(RN.age,time == 0)
    RN.init = tidyr::spread(RN.init,agecl,atoutput)  
    
    RN.v.RN.init = round(RN.mn[,2:11]/RN.init[,3:12], digits = 2)
    row.names(RN.v.RN.init) = mum.age$Code
    
    #Test to compare
    RN.rel = atlantistools::convert_relative_initial(RN.age)
    RN.rel = dplyr::group_by(RN.rel,species,agecl)
    RN.rel = dplyr::summarise(RN.rel,avg = mean(atoutput))
    RN.rel = tidyr::spread(RN.rel,agecl,avg)
    
    #RN based mum scale
    mum.scale = mum.age[,2:11]/RN.v.RN.init
    row.names(mum.scale) = mum.age$Code
    mum.scale = mum.scale[order(row.names(mum.scale)),]
    
    #RN based C scale
    C.scale = C.age[,2:11]/RN.v.RN.init
    row.names(C.scale) = C.age$Code
    C.scale = C.scale[order(row.names(C.scale)),]
    
    #growth scalar
    growth.scalar = 1/RN.v.RN.init
    growth.scalar = growth.scalar[order(row.names(growth.scalar)),]
    
    #Write RN based mum/C to file
    write.csv(growth.scalar, file = paste0(fig.dir,run.name,'_RNbased_growth_scalar.csv'),row.names = T)
    write.csv(mum.scale, file = paste0(fig.dir,run.name,'_newMum_RNbased.csv'),row.names =T)
    write.csv(C.scale,file = paste0(fig.dir,run.name,'_newC_RNbased.csv'),row.names =T)
    
    #
    mum.age = mum.age[order(mum.age$Code),]
    C.age = C.age[order(C.age$Code),]
    write.csv(mum.age, file = paste0(fig.dir,run.name,'_Mum_used.csv'),row.names =T)
    write.csv(C.age,file = paste0(fig.dir,run.name,'_C_used.csv'),row.names = T)
    
    #
    mum.C = round(mum.age[,2:11]/C.age[,2:11],digits = 2)
    row.names(mum.C) = mum.age$Code
    mum.C = mum.C[order(row.names(mum.C)),]
    write.csv(mum.C, file = paste0(fig.dir,run.name,'_mum_to_C_ratio.csv'),row.names = T)
    
    #SN check
    SN.age = readRDS(paste0(out.dir,'SN_age.rds'))
    
    SN.init = dplyr::filter(SN.age,time ==0 )
    SN.init$highMum = SN.init$atoutput*0.1
    SN.init$lowMum = SN.init$atoutput*0.5
    SN.init$lowC = SN.init$atoutput*0.1
    SN.init$highC = SN.init$atoutput*0.06
    
    #transform mum and C wide to long
    mum.long = mum.age[,2:12]
    mum.long = reshape2::melt(mum.long)
    mum.long$variable = as.numeric(mum.long$variable)
    
    C.long = C.age[,2:12]
    C.long = reshape2::melt(C.long)
    C.long$variable = as.numeric(C.long$variable)
    
    #Combine 
    SN.test = dplyr::left_join(SN.init, group.index, by = c('species' = 'LongName'))
    mum.long = dplyr::left_join(SN.test,mum.long,by = c('Code','agecl' = 'variable'))
    SN.mum.C = dplyr::left_join(mum.long,C.long, by = c('Code','agecl' = 'variable'))
    
    SN.mum.C$mum.below.high = SN.mum.C$value.x<(SN.mum.C$highMum*1.05)
    SN.mum.C$mum.over.low = SN.mum.C$value.x > (SN.mum.C$lowMum*0.95)
    SN.mum.C$C.below.high = SN.mum.C$value.y < (SN.mum.C$highC*1.05)
    SN.mum.C$C.over.low = SN.mum.C$value.y > (SN.mum.C$lowC*0.95)
    
    write.csv(SN.mum.C,file = paste0(fig.dir,run.name,'_SN_sanity_check_on_mum_and_C.csv'),row.names = F)
   
    rm(RN.age,SN.age) 
  }
  
  
  # SN/RN plots -------------------------------------------------------------
  
  #SN/RN plots
  if(plot.sn.rn|plot.all){
    SN.box = readRDS(paste0(out.dir,'SN_box.rds'))
    RN.box = readRDS(paste0(out.dir,'RN_box.rds'))
    RN.age = readRDS(paste0(out.dir,'RN_age.rds'))
    SN.age = readRDS(paste0(out.dir,'SN_age.rds'))
    
    #SN per box
    temp.plot.1 = atlantistools::plot_line(SN.box)
    temp.plot.1 = atlantistools::custom_grid(temp.plot.1,grid_x = 'polygon', grid_y = 'species')
    temp.plot.1 = add.title(temp.plot.1,'SN by Box')
    
    #RN per box
    temp.plot.2 = atlantistools::plot_line(RN.box)
    temp.plot.2 = atlantistools::custom_grid(temp.plot.2,grid_x = 'polygon', grid_y = 'species')
    temp.plot.2 = add.title(temp.plot.2,'RN by Box')
    
    #SN vs SN init
    SN.rel = atlantistools::convert_relative_initial(SN.age)
    temp.plot.3 = atlantistools::plot_line(SN.rel, col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3,list(x = 'Time (years)', y= expression(SN/SN[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3,'SN vs SN Init')
    
    #RN vs RN init
    RN.rel = atlantistools::convert_relative_initial(RN.age)
    temp.plot.4 = atlantistools::plot_line(RN.rel, col = 'agecl')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4,list(x = 'Time (years)', y= expression(RN/RN[init])))
    temp.plot.4 = atlantistools::plot_add_box(temp.plot.4)
    temp.plot.4 = add.title(temp.plot.4,'RN vs RN Init')
    
    #SN/RN domain-wide
    RN.SN = SN.box %>% 
      dplyr::rename('SN' = atoutput) %>%
      dplyr::left_join(RN.box) %>%
      dplyr::rename('RN' = atoutput) %>%
      dplyr::group_by(species,time) %>%
      dplyr::summarize(SN = sum(SN,na.rm=T),
                       RN = sum(RN,na.rm=T)) %>%
      dplyr::mutate(RN.SN = RN/SN) 
    
    
    temp.plot.5 = ggplot2::ggplot(RN.SN, ggplot2::aes(x= time,y = RN.SN))+
      ggplot2::geom_line()+
      ggplot2::geom_hline(yintercept = 2.65,lty = 2)+
      ggplot2::facet_wrap(~species)+
      ggplot2::theme_classic()+
      ggplot2::theme(plot.title =  ggplot2::element_text(size = 14),
                     axis.title =  ggplot2::element_text(size = 14),
                     axis.text =  ggplot2::element_text(size = 12),
                     strip.text =  ggplot2::element_text(size = 14))
    # ggsave(filename = paste0(atl.dir,'Figures/','test.pdf'),width = 30, height = 30, units = 'in')
    
    pdf(file = paste0(fig.dir,run.name,' SN RN Timeseries.pdf'),width = 60, height = 60, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    gridExtra::grid.arrange(temp.plot.5)
    dev.off()
    
    rm(RN.box,RN.age,SN.box,SN.age)
  }
  
  
  # Recruitment/SSB plots ---------------------------------------------------
  
  
  #Plot recruits
  if(plot.recruits|plot.all){
    
    # Recruits TS
    ssb.recruits = readRDS(paste0(out.dir,'ssb_recruits.rds'))
    
    temp.plot.1 = atlantistools::plot_line(ssb.recruits, y = 'rec')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = list(x = 'Time (days)', y = 'Numbers'))
    temp.plot.1 = add.title(temp.plot.1, 'Recruits')
    
    # SSB TS
    temp.plot.2 = atlantistools::plot_line(ssb.recruits, y = 'ssb')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,labels = list(x = 'Time (days)', y = 'Numbers'))
    temp.plot.2 = add.title(temp.plot.2, 'SSB')
    
    # Recruit per SSB
    ssb.recruits$rec.per.sbb = ssb.recruits$rec/ssb.recruits$ssb
    temp.plot.3 = atlantistools::plot_line(ssb.recruits, y= 'rec.per.sbb', yexpand = T)
    temp.plot.3 = ggplot2::update_labels(temp.plot.3,labels = list(x = 'Time (days)',y = 'Numbers'))
    temp.plot.3 = add.title(temp.plot.3, 'Recruits per SSB')
    
    pdf(file = paste0(fig.dir,run.name,' Recruitment SSB Timeseries.pdf'),width = 14, height = 14,onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    dev.off()
    
    rm(ssb.recruits)
  }
  
  
  # Numbers timeseries ------------------------------------------------------
  
  
  #Plot Numbers timeseries
  if(plot.numbers.timeseries|plot.all){
    
    #Numbers TS
    numbers = readRDS(paste0(out.dir,'numbers.rds'))
    
    temp.plot.1 = atlantistools::plot_line(numbers)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = list(x='Time (years)', y = 'Numbers'))
    temp.plot.1 = add.title(temp.plot.1,'Numbers')
    
    #Numbers at age
    numbers.age = readRDS(paste0(out.dir,'numbers_age.rds'))
    
    temp.plot.2 = atlantistools::plot_line(numbers.age, col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, labels = list(x='Time (years)',y = 'Numbers', colour = 'Ageclass'))
    temp.plot.2 = add.title(temp.plot.2, 'Numbers at age')
    
    #Num age vs num init
    nums.rel = atlantistools::convert_relative_initial(numbers.age)
    temp.plot.3 = atlantistools::plot_line(nums.rel, col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3, list(x='Time (years)', y= expression(Numbers/Numbers[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3, 'Numbers vs. Initial Numbers')
    
    #Numbers per ageclass, used to scale recruitment values from initial conditions ageclass 1
    nums.rel = dplyr::group_by(nums.rel,species,agecl)
    nums.rel = dplyr::summarise(nums.rel, avg = mean(atoutput))
    nums.rel = tidyr::spread(nums.rel, agecl, avg)
    
    nums.scale = 1/rowMeans(nums.rel[,2])
    nums.c = data.frame(nums.rel[,1])
    nums.c$scale = nums.scale
    
    nums.init = dplyr::filter(numbers.age, time == 0 & agecl == 1)
    
    RN.age = readRDS(paste0(out.dir,'RN_age.rds'))
    SN.age = readRDS(paste0(out.dir,'SN_age.rds'))
    
    RN.age = dplyr::filter(RN.age, time == 0 & agecl == 1)
    SN.age = dplyr::filter(SN.age, time == 0 & agecl == 1)
    
    nums.RN.SN = dplyr::left_join(nums.init, group.index, by = c('species' = 'LongName'))
    nums.RN.SN$SN_RN = RN.age$atoutput + SN.age$atoutput
    nums.RN.SN$totalN = nums.RN.SN$atoutput*nums.RN.SN$SN_RN
    numscale.f = dplyr::left_join(nums.RN.SN, nums.c, by = 'species')
    
    write.csv(numscale.f, file = paste0(fig.dir,run.name,'_init_nums_scalar_for_recruits.csv'),row.names = T)
    
    #num at age %
    num.pct = atlantistools::agg_perc(numbers.age, groups = c('time','species'))
    temp.plot.4 = atlantistools::plot_bar(num.pct, fill = 'agecl', wrap = 'species')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4, labels = list(x= "Time (years0",y = 'Numbers (%)'))
    temp.plot.4 = add.title(temp.plot.4, 'Numbers at age - Percent')
    
    #Biomass Pool Grazers
    #Biomass pool grazing
    grazing =readRDS(paste0(out.dir,'grazing.rds'))
    
    temp.plot.5 = atlantistools::plot_line(grazing)
    temp.plot.5 = ggplot2::update_labels(temp.plot.5,list(x = 'Time (years)',y='Numbers'))
    temp.plot.5 = add.title(temp.plot.5, 'Biomass Pool Grazers')
    
    pdf(file=paste0(fig.dir,run.name,' Numbers Timeseries.pdf'), width = 24, height = 24, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    gridExtra::grid.arrange(temp.plot.5)
    dev.off()
    
    rm(RN.age,SN.age,numbers,numbers.age,grazing)
  }
  
  
  # Physics plots -----------------------------------------------------------
  
  
  #plot physics variables
  if(plot.physics|plot.all){
    
    #Physics snapshot
    physics.statevars = readRDS(paste0(out.dir,'physics_statevars.rds'))
    
    temp.plot.1 = atlantistools::plot_line(physics.statevars, wrap = NULL)
    temp.plot.1 = atlantistools::custom_grid(temp.plot.1, grid_x = 'polygon', grid_y = 'variable')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1, list(y = ''))
    temp.plot.1 = add.title(temp.plot.1, 'Physics Snapshot')
    
    #Phys plots
    physics = atlantistools::flip_layers(physics.statevars)
    physics = split(physics,physics$variable)
    phys.plots = list()
    for(v in 1:length(physics)){
      phys.plots[[v]] = atlantistools::plot_line(physics[[v]],wrap = NULL)
      phys.plots[[v]] = atlantistools::custom_grid(phys.plots[[v]],grid_x = 'polygon', grid_y = 'layer')
      phys.plots[[v]] = add.title(phys.plots[[v]],names(physics)[v])
      phys.plots[[v]] = ggplot2::update_labels(phys.plots[[v]], labels = list(x = 'time', y= names(phys.plots)[v]))
    }
    
    #fluxes 1
    flux = readRDS(paste0(out.dir,'flux.rds'))
    temp.plot.2 = atlantistools::flip_layers(flux)
    temp.plot.2 = atlantistools::plot_line(temp.plot.2,wrap = NULL, col = 'variable')
    temp.plot.2 = atlantistools::custom_grid(temp.plot.2, grid_x = 'polygon', grid_y = 'layer')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, list(y = ''))
    temp.plot.2 = add.title(temp.plot.2, 'Fluxes')
    
    #fluxes 2
    source.sink = readRDS(paste0(out.dir,'source_sink.rds'))
    temp.plot.3 = atlantistools::flip_layers(source.sink)
    temp.plot.3 = atlantistools::plot_line(temp.plot.3,wrap = NULL, col = 'variable')
    temp.plot.3 = atlantistools::custom_grid(temp.plot.3, grid_x = 'polygon', grid_y = 'layer')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3, list(y = ''))
    temp.plot.3 = add.title(temp.plot.3, 'Source-Sink')
    
    #Changes in wc w/ rel dz
    dz = readRDS(paste0(out.dir,'dz.rds'))
    nominal.dz = readRDS(paste0(out.dir,'nominal_dz.rds'))
    
    check.dz = dplyr::left_join(dz,nominal.dz, by = c('polygon','layer'))
    check.dz = dplyr::mutate(check.dz, check.dz = atoutput.x/atoutput.y)
    check.dz = dplyr::filter(check.dz, !is.na(check.dz))    
    
    temp.plot.4 = atlantistools::plot_line(check.dz,x = 'time',y = 'check.dz',wrap = 'polygon',col = 'layer')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4,list(x='Time (years)',y= expression(dz/nominal_dz)))
    temp.plot.4 = add.title(temp.plot.4,'Change in Water Column Height')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4, list(y = ''))
    
    pdf(file = paste0(fig.dir,run.name, ' Physics Timeseries.pdf'),width = 60, height = 18, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    for(i in 1:length(phys.plots)){
      gridExtra::grid.arrange(phys.plots[[i]])
    }
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    dev.off()
    
    rm(physics.statevars,flux,source.sink,dz,nominal.dz)
  }
  
  
  # Growth and Consumption --------------------------------------------------
  
  
  #plot growth and consumption
  if(plot.growth.cons|plot.all){
    
    #Growth at ageclass v growth init
    growth.age = readRDS(paste0(out.dir,'growth_age.rds'))
    growth.rel = atlantistools::convert_relative_initial(growth.age)
    temp.plot.1 = atlantistools::plot_line(growth.rel, col = 'agecl')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1, list(x= 'Time (years)', y = expression(Growth/Growth[init])))
    temp.plot.1 = atlantistools::plot_add_box(temp.plot.1)
    temp.plot.1 = add.title(temp.plot.1, 'Growth at Age vs. Iniital Growth')
    
    #Grwoth vs growth init
    growth.rel.init = readRDS(paste0(out.dir,'growth_rel_init.rds'))
    temp.plot.2 = atlantistools::plot_line(growth.rel.init, y = 'gr_rel', col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, list(y = expression((Growth-Growth[req]/Growth[req]))))
    temp.plot.2 = add.title(temp.plot.2,'Growth vs. Initial Growth')
    
    #Consumptions at age vs. initial
    eat_age = readRDS(paste0(out.dir,'eat_age.rds'))
    cons.rel = atlantistools::convert_relative_initial(eat_age)
    temp.plot.3 = atlantistools::plot_line(cons.rel,col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3, list(x = 'Time (years)', y= expression (Consumption/Consumption[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3, 'Consumption at Age vs. Initial Consumption')
    
    #Consumption at age timeseries
    temp.plot.4 = atlantistools::plot_line(eat_age, col = 'agecl')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4, list(x='Time (years)',y='Biomass (tonnes)',color = 'Ageclass'))
    temp.plot.4 = add.title(temp.plot.4, 'Consumption at Age')
    
    pdf(file = paste0(fig.dir,run.name,' Growth and Consumption.pdf'),width = 24, height = 24, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    dev.off()
    
    rm(growth.age,growth.rel.init,eat_age)
  }
  
  
  # Cohort timeseries -------------------------------------------------------
  
  
  #plot cohort timeseries
  if(plot.cohort|plot.all){
    
    numbers.age = readRDS(paste0(out.dir,'numbers_age.rds'))
    pdf(file = paste0(fig.dir, run.name, ' Cohort Timeseries.pdf'),width = 24, height = 18, onefile =T)
    for(i in 1:10){
      age.sub = dplyr::filter(numbers.age, agecl == i)
      temp.plot = atlantistools::plot_line(age.sub)
      temp.plot = ggplot2::update_labels(temp.plot,list(x='Time (years)',y = 'Numbers'))
      temp.plot = add.title(temp.plot,paste0('Age-',i))
      gridExtra::grid.arrange(temp.plot)
    }
    dev.off()
    
    rm(numbers.age)
  }
  
  
  # Diet --------------------------------------------------------------------
  
  
  #Diet figures
  if(plot.diet|plot.all){
    
    bio_consumed = readRDS(paste0(out.dir,'biomass_consumed.rds'))
    library(atlantistools)
    if(nrow(bio_consumed)>0){
      
      # diet.plots = atlantistools::plot_diet(result$biomass.consumed, wrap_col =  'agecl', combine_thresh =  3)
      wrap_col = 'agecl'
      combine_thresh = 3
      species = NULL
      
      check_df_names(data = bio_consumed, expect = c("pred", "agecl", 
                                                     "time", "prey", "atoutput"), optional = "polygon")
      agg_bio <- agg_data(bio_consumed, groups = c("time", "pred", 
                                                   "agecl", "prey"), fun = sum)
      preddata <- agg_perc(agg_bio, groups = c("time", "pred", 
                                               "agecl"))
      preydata <- agg_perc(agg_bio, groups = c("time", "prey", 
                                               "agecl"))
      pred_comb <- combine_groups(preddata, group_col = "prey", 
                                  groups = c("time", "pred", "agecl"), combine_thresh = combine_thresh)
      prey_comb <- combine_groups(preydata, group_col = "pred", 
                                  groups = c("time", "prey", "agecl"), combine_thresh = combine_thresh)
      plot_sp <- function(data, col, wrap_col) {
        if (nrow(data) == 0) {
          plot <- ggplot2::ggplot() + ggplot2::theme_void()
        }
        else {
          agg_data <- agg_data(data, groups = col, out = "sum_diet", 
                               fun = sum)
          data[, col] <- factor(data[[col]], levels = agg_data[[1]][order(agg_data$sum_diet, 
                                                                          decreasing = TRUE)])
          plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time, 
                                                      y = ~atoutput, fill = lazyeval::interp(~var, 
                                                                                             var = as.name(col)))) + ggplot2::geom_bar(stat = "identity") + 
            ggplot2::scale_fill_manual(values = c(get_colpal(),RColorBrewer::brewer.pal(8,'Set1'))) + 
            ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(wrap_col)), 
                                ncol = 5, labeller = "label_both") + ggplot2::labs(x = NULL, 
                                                                                   y = NULL, title = NULL) + theme_atlantis() + 
            ggplot2::theme(legend.position = "right")
          plot <- atlantistools:::ggplot_custom(plot)
        }
        return(plot)
      }
      if (is.null(species)) {
        species <- sort(union(union(union(preddata$pred, preddata$prey), 
                                    preydata$pred), preydata$prey))
      }
      grobs <- vector("list", length = length(species))
      for (i in seq_along(grobs)) {
        grobs[[i]] <- vector("list", length = 2)
      }
      for (i in seq_along(species)) {
        df_pred <- dplyr::filter_(pred_comb, ~pred == species[i])
        df_prey <- dplyr::filter_(prey_comb, ~prey == species[i])
        grobs[[i]][[1]] <- plot_sp(df_pred, col = "prey", wrap_col = wrap_col)
        grobs[[i]][[2]] <- plot_sp(df_prey, col = "pred", wrap_col = wrap_col)
      }
      for (i in seq_along(grobs)) {
        heading <- grid::textGrob(paste("Diet proportions for species:", 
                                        species[i]), gp = grid::gpar(fontsize = 14))
        grobs[[i]][[1]] <- grobs[[i]][[1]] + ggplot2::labs(y = "Predator perspective")
        grobs[[i]][[2]] <- grobs[[i]][[2]] + ggplot2::labs(y = "Prey perspective")
        grobs[[i]] <- gridExtra::arrangeGrob(grobs = c(list(heading), 
                                                       grobs[[i]]), heights = grid::unit(c(0.05, 0.475, 
                                                                                           0.475), units = "npc"))
      }
      names(grobs) <- species
      
      pdf(file = paste0(fig.dir,run.name, ' Diet Proportions.pdf'),paper = 'A4r',width = 22, height = 16, onefile = T)
      for(i in seq_along(grobs)){
        gridExtra::grid.arrange(grobs[[i]])
      }
      dev.off()
    }
    
    rm(bio_consumed)
    
  }
  
  if(plot.consumption|plot.all){
    if(file.size(param.ls$prod.nc)> 1E9){
      print('Output file size too large to generate consumption plots')
    }else{
      source(here::here('R','plot_overall_predation.R'))  
      consumption = get_consumption(prod.file = param.ls$prod.nc,
                                    fgs.file = param.ls$groups.file)
      data.sub = subset_diet(diet.file = param.ls$dietcheck,
                             consumption = consumption,
                             spp.names  = group.index$Code)
      plot_overall_predation(data = data.sub,
                             bioindex.file = paste0(atl.dir,'neus_outputBiomIndx.txt'),
                             catch.file = paste0(atl.dir,'neus_outputCatch.txt'),
                             min.fract = 0.1,
                             fig.dir = fig.dir,
                             file.prefix = run.name)
    }
  }
  
  # Spatial biomass ---------------------------------------------------------
  
  
  #Spatial biomass
  if(plot.spatial.biomass){

    biomass.spatial.stanza = readRDS(paste0(out.dir,'biomass_spatial_stanza.rds'))
    volume = readRDS(paste0(out.dir,'volume.rds'))

    temp.plots = atlantistools::plot_spatial_box(biomass.spatial.stanza,
                                                 bgm_as_df = atlantistools::convert_bgm(bgm = param.ls$bgm), timesteps = 7)
    pdf(file = paste0(fig.dir, run.name, ' Spatial Biomass Box Distribution.pdf'),width = 24, height =18 )
    for( i in seq_along(temp.plots)){
      gridExtra::grid.arrange(temp.plots[[i]])
    }
    dev.off()

    biomass.spatial.stanza= dplyr::filter(biomass.spatial.stanza,!is.na(layer))
    temp.plots.2 = atlantistools::plot_spatial_ts(biomass.spatial.stanza,
                                                  bgm_as_df = atlantistools::convert_bgm(bgm = param.ls$bgm), vol = volume )
    pdf(file = paste0(fig.dir, run.name, ' Spatial Biomass Distribution Timeseries.pdf'),width =24, height =18 )
    for(i in seq_along(temp.plots.2)){
      gridExtra::grid.arrange(temp.plots.2[[i]])
    }
    dev.off()

    rm(biomass.spatial.stanza)

  }
  
  
  # LTL plots ---------------------------------------------------------------
  
  
  #LTL plots
  if(plot.LTL|plot.all){
    
    #Read in bio data
    biom = read.table(paste0(atl.dir,run.prefix,'BiomIndx.txt'),header= T)
    phyto = read.table(phytopl.history,header =T,sep = ',')
    zoo = read.table(zoopl.history,header = T, sep = ',')
    
    pdf(file = paste0(fig.dir,run.name,' LTL Timeseries.pdf'),width = 12, height = 12, onefile =T)
    
    #Picophytoplankton
    plot(PS~Time,biom,type='l',ylim = c(0,max(c(biom$PS,phyto$PS.ts))))
    lines(PS.ts~days,phyto,col='red')
    legend('topleft',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty='n')
    mtext(text = 'PS',3)
    
    #Diatom
    plot(PL~Time,biom, type='l',ylim = c(0,max(c(biom$PL,phyto$PL.ts))))
    lines(PL.ts~days,phyto,col='red')
    legend('topleft',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty = )
    mtext('PL',3)
    
    #Dinoflag
    plot(DF~Time,biom,type='l',ylim = c(0,max(c(biom$DF,phyto$DF.ts))))
    lines(DF.ts~days,phyto,col='red')
    legend('topleft',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty = )
    mtext('DF',3)
    
    #Large Zooplankton
    plot(ZL~Time,biom,type='l',ylim = c(0,max(c(biom$ZL,zoo$ZL))))
    lines(ZL~Time,zoo,col = 'red')
    legend('bottomright',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty = )
    mtext('ZL',3)
    
    #Medium Zooplankton
    plot(ZM~Time,biom,type='l',ylim = c(0,max(c(biom$ZM,zoo$ZM))))
    lines(ZM~Time,zoo,col='red')
    legend('topleft',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty = )
    mtext('ZM',3)
    
    #Small Zooplankton
    plot(ZS~Time,biom,type='l',ylim = c(0,max(c(biom$ZS,zoo$ZS))))
    lines(ZS~Time,zoo,col='red')
    legend('topleft',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty = )
    mtext('ZS',3)
    
    #Gelatenous Zooplankton
    plot(ZG~Time,biom,type='l',ylim=c(0,max(c(biom$ZG,zoo$ZG))))
    lines(ZG~Time,zoo,col = 'red')
    legend('topleft',legend = c(run.name,'Data'),lty = c(1,1), col = c('black','red'),bty = )
    mtext('ZG',3)
    
    dev.off()
    
  }
  
  if(plot.spatial.biomass.seasonal){
    source(here::here('R','plot_biomass_box_summary.R'))
    
    plot_biomass_box_season(bio.box = readRDS(paste0(out.dir,'biomass_box.rds')),
                           bio.box.invert = readRDS(paste0(out.dir,'biomass_box_invert.rds')),
                           fig.dir = fig.dir,
                           species.list = NULL,
                           plot.presence = T,
                           save.fig = T)
  }
}
