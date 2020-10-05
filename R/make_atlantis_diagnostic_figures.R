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
#'@plot.LTL logical. Plots comparing LTL groups (domain-wide) to data
#'
#'@return A series of figures and tables based on output grouping flags
#' 
#' Author: Ryan Morse, modified by J. Caracappa
#' 

# atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/'
# out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/Post_Processed/'
# param.dir = here::here('CurrentVersion')
# run.prefix = 'neus_output_test'
# run.name = 'NutrientForcing'
# 
# 
# source(here::here('R','get_atl_paramfiles.R'))
# source(here::here('R','process_atl_output.R'))
# param.ls= get_atl_paramfiles(param.dir,atl.dir,include_catch=T)
# 
# # process_atl_output( param.dir = here::here('CurrentVersion'),
# #   atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/',
# #   out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Atlantis_Output_NutrientForcing/Post_Processed/',
# #   run.prefix = 'neus_output_test',
# #   include_catch = T,
# #   save.out = T,
# #   bgm.file = param.ls$bgm,
# #   groups.file = param.ls$func.groups,
# #   init.file = param.ls$init.nofill,
# #   param.ls$biol.prm = param.ls$param.ls$biol.prm,
# #   run.prm = param.ls$run.prm,
# #   main.nc = param.ls$main.nc,
# #   prod.nc = param.ls$prod.nc,
# #   dietcheck = param.ls$dietcheck,
# #   ssb = param.ls$ssb,
# #   yoy = param.ls$yoy,
# #   catch.file = param.ls$catch,
# #   totcatch.file = param.ls$catchtot,
# #   spatial.overlap = F
# # )
# 
# load(paste0(out.dir,'neus_output_test_postprocessed.rdata'))
# 
# fig.width = 10
# fig.height = 8
# benthic.box = 10
# benthic.level = 4
# 
# bgm.file = param.ls$bgm
# param.ls$func.groups = param.ls$func.groups
# run.prm = param.ls$run.prm
# param.ls$biol.prm = param.ls$biol.prm
# phytopl.history = here::here('R','phytoplankton_timeseries_biomass_tonnes_1998_2016.csv')
# zoopl.history = here::here('R','Zooplankton_total_biomass_tonnes_N_20yrs.csv')
# 
# plot.benthic = T
# plot.overall.biomass = T
# plot.biomass.timeseries = T
# plot.length.age=T
# plot.biomass.box=T
# plot.c.mum=T
# plot.sn.rn=T
# plot.recruits=T
# plot.numbers.timeseries=T
# plot.physics=T
# plot.growth.cons=T
# plot.cohort=T
# plot.diet=T
# plot.spatial.biomass=T
# plot.LTL=T


make_atlantis_diagnostic_figures = function(
  out.dir,
  atl.dir,
  param.dir,
  run.prefix,
  run.name,
  
  result,
  param.ls,
  
  # fig.width,
  # fig.height,
  benthic.box,
  benthic.level= 4,
  
  phytopl.history,
  zoopl.history,
                                            
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
  plot.spatial.biomass,
  plot.LTL
  ){
  
  `%>%` = dplyr::`%>%`
  #Utility function
  add.title = function(plot,title){
    plot = plot + ggplot2::ggtitle(title)+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    return(plot)
  }
  
  #Load groups data
  group.code = atlantistools::get_age_acronyms(param.ls$func.groups)
  group.data = atlantistools::load_fgs(param.ls$func.groups)
  group.index = dplyr::select(group.data,c(Code,LongName))
  
  
  #Load BGM data
  box.bgm = atlantistools::load_box(param.ls$bgm)
  
  #plot parameters
  plot.labels = list(x = 'Time (years)',y = 'Biomass (tonnes)')
  

# Benthic box timeseries --------------------------------------------------

  
  #Select box for timeseries of all benthic groups
  if(plot.benthic){
    benthic.biomass.spatial = dplyr::filter(result$biomass.spatial.stanza, layer == benthic.level & polygon == benthic.box)
    benthic.spp = unique(benthic.biomass.spatial$species)
    box.area = box.bgm$boxes[[benthic.box+1]]$area
    
    pdf(file = paste0(out.dir,run.name, ' Box_',benthic.box,'_benthos.pdf'))
    for(i in 1:length(benthic.spp)){
      spp.ind = benthic.spp[i]
      benthic.biomass.spp = dplyr::filter(benthic.biomass.spatial,species == spp.ind)
      plot(benthic.biomass.spp$atoutput/box.area~benthic.biomass.spp$time,
           type= 'l',main = paste(spp.ind,'Box',benthic.box),ylab = 'Mg N m-2',xlab = 'Time')
    }
    dev.off()
  }
  

# Overall biomass ---------------------------------------------------------

  
  #Make overall biomass plot (stacked barplot of total biomass domain-wide)
  if(plot.overall.biomass){
    #combine threshold = 10
    biomass.df.10 = atlantistools::combine_groups(result$biomass, group_col = 'species', combine_thresh = 10)
    temp.plot.1 = atlantistools::plot_bar(biomass.df.10)
    temp.plot.1 = temp.plot.1 + ggplot2::ggtitle('Top 10 Groups')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1, labels = plot.labels)
    
    #Combine threshold = 20
    biomass.df.20 = atlantistools::combine_groups(result$biomass, group_col = 'species', combine_thresh = 20)
    temp.plot.2 = atlantistools::plot_bar(biomass.df.20)
    temp.plot.2 = temp.plot.2 + ggplot2::ggtitle('Top 20 Groups')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, labels = plot.labels)
    
    pdf(file = paste0(out.dir,run.name,' overall biomass.pdf'),width = 12, height = 6, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    dev.off()
  }
  

# Biomass Timeseries ------------------------------------------------------

  
  #Make biomass timeseries plots
  if(plot.biomass.timeseries){
    
    #biomass by species timeseries
    temp.plot.1 = atlantistools::plot_line(result$biomass)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = plot.labels)
    temp.plot.1 = add.title(temp.plot.1,'Biomass')
    
    #biomass at age timeseries
    temp.plot.2 = atlantistools::plot_line(result$biomass.age, col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(p = temp.plot.2, labels = c(plot.labels, list(colour = 'Ageclas')))
    temp.plot.2 = add.title(temp.plot.2,'Biomass at Age')
    
    #biomass at age relative to initial biomass timeseries
    rel.biomass.age = atlantistools::convert_relative_initial(result$biomass.age)
    temp.plot.3 = atlantistools::plot_line(rel.biomass.age, col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3,list(x = 'Time (years)', y = expression(biomass/bio[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3,'Biomass at Age Relatative to Initial Biomass')

    #Biomass vs Bio init
    rel.biomass = atlantistools::convert_relative_initial(result$biomass)
    temp.plot.4 = atlantistools::plot_line(rel.biomass)
    temp.plot.4 = ggplot2::update_labels(temp.plot.4,list(x = 'Time (years)',y = expression (Biomass/Biomass[init])))
    temp.plot.4 = atlantistools::plot_add_box(temp.plot.4)
    temp.plot.4 = add.title(temp.plot.4,'Biomass Relatative to Initial Biomass')

    #Invert bio timeseries
    temp.plot.5 = atlantistools::plot_line(result$biomass.age.invert)
    temp.plot.5 = ggplot2::update_labels(temp.plot.5, labels = plot.labels)
    temp.plot.5 = add.title(temp.plot.5,'Invert Biomass')
    
    #Bio at age
    bio.age.pct = atlantistools::agg_perc(result$biomass.age, groups = c('time','species'))
    temp.plot.6 = atlantistools::plot_bar(bio.age.pct, fill = 'agecl', wrap = 'species')
    temp.plot.6 = ggplot2::update_labels(temp.plot.6,labels = list(x='Time (years)', y = 'Numbers (%)'))
    temp.plot.6 = add.title(temp.plot.6, 'Biomass at age - Percent')
    
    pdf(paste0(out.dir,run.name,' biomass timeseries.pdf'),width = 20, height = 20, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    gridExtra::grid.arrange(temp.plot.5)
    gridExtra::grid.arrange(temp.plot.6)
    dev.off()
  }
  

# Length-age plots --------------------------------------------------------

  
  #Make length.age plots
  if(plot.length.age){
    
    #Length at age ts by spp
    init.length = read.csv(paste0(param.dir,'/vertebrate_init_length_cm.csv'),header =T, stringsAsFactors = F)
    init.length = init.length[order(init.length$Long.Name),]
    spp.names = unique(result$length.age$species)
    
    pdf(file = paste0(out.dir,run.name,' tuning length at age by species.pdf'),width = 12, height = 6, onefile = T)
    for(x in 1:length(spp.names)){
      spp.id = spp.names[x]
      length.age.spp = dplyr::filter(result$length.age, species == spp.id & time >0)
      init.length.age.spp = dplyr::filter(init.length[,3:13],Long.Name == spp.id)
      boxplot(length.age.spp$atoutput ~ length.age.spp$agecl, ylab = 'cm',xlab = 'cohort',
              main = spp.id, ylim = c(0,max(length.age.spp$atoutput)))
      points(seq(1:10),init.length.age.spp[2:11],col = 'red',pch= 17)
    }
    dev.off()
    
    #Length age together
    temp.plot.1 = atlantistools::plot_line(result$length.age,col = 'agecl')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = c(x = 'Time (years)',y = 'Length (cm)', colour = 'Ageclass'))
    temp.plot.1 = add.title(temp.plot.1,'Length-at-age')

    #Length at age vs. length init
    rel.length.age = atlantistools::convert_relative_initial(result$length.age)
    temp.plot.2 = atlantistools::plot_line(rel.length.age,col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,list(x='Time (years)',y = expression(length/length[init])))
    atlantistools::plot_add_box(temp.plot.2)
    temp.plot.2 = add.title(temp.plot.2,'Length at Age vs. Initial Length')
    
    pdf(file = paste0(out.dir,run.name,' Length at Age Timeseries.pdf'),width = 16, height = 16, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    dev.off()
  }
  

# Biomass box plots -------------------------------------------------------

  
  #Make Biomass Box plots
  if(plot.biomass.box){
    
    #Bio per box
    temp.plot.1 = atlantistools::plot_line(result$biomass.box)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,plot.labels)
    temp.plot.1 = atlantistools::custom_grid(temp.plot.1, grid_x = 'polygon',grid_y = 'species')
    temp.plot.1 = add.title(temp.plot.1,'Biomass by Box')
    
    #Invert bio by box
    temp.plot.2 = atlantistools::plot_line(result$biomass.box.invert)
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,plot.labels)
    temp.plot.2 = atlantistools::custom_grid(temp.plot.2,grid_x = 'polygon',grid_y = 'species')
    temp.plot.2 = add.title(temp.plot.2, 'Invert Biomass by Box')
    
    pdf(file = paste0(out.dir,run.name,' Biomass Box Timeseries.pdf'),width = 48, height = 60, onefile =T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    dev.off()
  }
  

# C/Mum tuning ------------------------------------------------------------

  #C_Mum tuning
  if(plot.c.mum){
    
    #Data processing

    #mum by age
    mum.age = atlantistools::prm_to_df_ages(param.ls$biol.prm, param.ls$func.groups,group = group.code,parameter = 'mum')
    mum.age = tidyr::spread(mum.age,agecl,mum)
    mum.age = dplyr::left_join(mum.age,group.index, by = c('species'='LongName'))
    
    #C by age
    C.age = atlantistools::prm_to_df_ages(param.ls$biol.prm,param.ls$func.groups,group = group.code,parameter = 'C')
    C.age = tidyr::spread(C.age,agecl,c)
    C.age = dplyr::left_join(C.age,group.index,by = c('species' = 'LongName'))
    
    #Initial length
    init.length = read.csv(paste0(param.dir,'/vertebrate_init_length_cm.csv'),header =T, stringsAsFactors = F)
    init.length = init.length[order(init.length$Long.Name),]
    
    ## RM "scale mum and C to length at age relative to initial conditions"
    length.age.mn = dplyr::group_by(result$length.age,species,agecl)
    length.age.mn = dplyr::summarise(length.age.mn, avg = mean(atoutput))
    length.age.mn = tidyr::spread(length.age.mn,agecl,avg)
    
    #Mean length at age divided by initial length at age
    #Used to scale mum and C
    length.v.length.init = length.age.mn[,2:11]/init.length[,4:13]
    row.names(length.v.length.init) =mum.age$Code
    
    #Scale mum and C by difference between length at age relative to initial conditions
    mum.scale = mum.age[,2:11]/length.v.length.init
    rownames(mum.scale) = mum.age$Code
    mum.scale = mum.scale[order(row.names(mum.scale)),]
    
    C.scale = C.age[,2:11]/length.v.length.init
    row.names(C.scale) = C.age$Code
    C.scale = C.scale[order(row.names(C.scale)),]
    
    #Write length-scaled C and mum to file
    write.csv(mum.scale, file = paste0(out.dir,run.name,'newMum_lengthbased.csv'),row.names =T)
    write.csv(C.scale,file = paste0(out.dir,run.name,'newC_lengthbased.csv'),row.names = T)
            
    ### Also scale mum/C relative to RN vs RN init
    RN.mn = dplyr::group_by(result$RN.age,species,agecl)
    RN.mn = dplyr::summarize(RN.mn,avg = mean(atoutput))
    RN.mn = tidyr::spread(RN.mn,agecl,avg)
    
    RN.init = dplyr::filter(result$RN.age,time == 0)
    RN.init = tidyr::spread(RN.init,agecl,atoutput)  
    
    RN.v.RN.init = round(RN.mn[,2:11]/RN.init[,3:12], digits = 2)
    row.names(RN.v.RN.init) = mum.age$Code
    
    #Test to compare
    RN.rel = atlantistools::convert_relative_initial(result$RN.age)
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
    write.csv(growth.scalar, file = paste0(out.dir,run.name,'_RNbased_growth_scalar.csv'),row.names = T)
    write.csv(mum.scale, file = paste0(out.dir,run.name,'_newMum_RNbased.csv'),row.names =T)
    write.csv(C.scale,file = paste0(out.dir,run.name,'_newC_RNbased.csv'),row.names =T)
    
    #
    mum.age = mum.age[order(mum.age$Code),]
    C.age = C.age[order(C.age$Code),]
    write.csv(mum.age, file = paste0(out.dir,run.name,'_Mum_used.csv'),row.names =T)
    write.csv(C.age,file = paste0(out.dir,run.name,'_C_used.csv'),row.names = T)
    
    #
    mum.C = round(mum.age[,2:11]/C.age[,2:11],digits = 2)
    row.names(mum.C) = mum.age$Code
    mum.C = mum.C[order(row.names(mum.C)),]
    write.csv(mum.C, file = paste0(out.dir,run.name,'_mum_to_C_ratio.csv'),row.names = T)

    #SN check
    SN.init = dplyr::filter(result$SN.age,time ==0 )
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
    
    write.csv(SN.mum.C,file = paste0(out.dir,run.name,'_SN_sanity_check_on_mum_and_C.csv'),row.names = F)
    
  }
  

# SN/RN plots -------------------------------------------------------------

  #SN/RN plots
  if(plot.sn.rn){
    
    #SN per box
    temp.plot.1 = atlantistools::plot_line(result$SN.box)
    temp.plot.1 = atlantistools::custom_grid(temp.plot.1,grid_x = 'polygon', grid_y = 'species')
    temp.plot.1 = add.title(temp.plot.1,'SN by Box')
    
    #RN per box
    temp.plot.2 = atlantistools::plot_line(result$RN.box)
    temp.plot.2 = atlantistools::custom_grid(temp.plot.2,grid_x = 'polygon', grid_y = 'species')
    temp.plot.2 = add.title(temp.plot.2,'RN by Box')
    
    #SN vs SN init
    SN.rel = atlantistools::convert_relative_initial(result$SN.age)
    temp.plot.3 = atlantistools::plot_line(SN.rel, col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3,list(x = 'Time (years)', y= expression(SN/SN[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3,'SN vs SN Init')
    
    #RN vs RN init
    RN.rel = atlantistools::convert_relative_initial(result$RN.age)
    temp.plot.4 = atlantistools::plot_line(RN.rel, col = 'agecl')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4,list(x = 'Time (years)', y= expression(RN/RN[init])))
    temp.plot.4 = atlantistools::plot_add_box(temp.plot.4)
    temp.plot.4 = add.title(temp.plot.4,'RN vs RN Init')
    
    #SN/RN domain-wide
    RN.SN = result$SN.box %>% 
      dplyr::rename('SN' = atoutput) %>%
      dplyr::left_join(result$RN.box) %>%
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
    
    pdf(file = paste0(out.dir,run.name,' SN RN Timeseries.pdf'),width = 60, height = 60, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    gridExtra::grid.arrange(temp.plot.5)
    dev.off()
  }
  

# Recruitment/SSB plots ---------------------------------------------------

  
  #Plot recruits
  if(plot.recruits){
    
    # Recruits TS
    temp.plot.1 = atlantistools::plot_line(result$ssb.recruits, y = 'rec')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = list(x = 'Time (days)', y = 'Numbers'))
    temp.plot.1 = add.title(temp.plot.1, 'Recruits')
    
    # SSB TS
    temp.plot.2 = atlantistools::plot_line(result$ssb.recruits, y = 'ssb')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2,labels = list(x = 'Time (days)', y = 'Numbers'))
    temp.plot.2 = add.title(temp.plot.2, 'SSB')
    
    # Recruit per SSB
    result$ssb.recruits$rec.per.sbb = result$ssb.recruits$rec/result$ssb.recruits$ssb
    temp.plot.3 = atlantistools::plot_line(result$ssb.recruits, y= 'rec.per.sbb', yexpand = T)
    temp.plot.3 = ggplot2::update_labels(temp.plot.3,labels = list(x = 'Time (days)',y = 'Numbers'))
    temp.plot.3 = add.title(temp.plot.3, 'Recruits per SSB')
    
    pdf(file = paste0(out.dir,run.name,' Recruitment SSB Timeseries.pdf'),width = 14, height = 14,onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    dev.off()
    
  }
  

# Numbers timeseries ------------------------------------------------------

  
  #Plot Numbers timeseries
  if(plot.numbers.timeseries){
    
    #Numbers TS
    temp.plot.1 = atlantistools::plot_line(result$numbers)
    temp.plot.1 = ggplot2::update_labels(temp.plot.1,labels = list(x='Time (years)', y = 'Numbers'))
    temp.plot.1 = add.title(temp.plot.1,'Numbers')
    
    #Numbers at age
    temp.plot.2 = atlantistools::plot_line(result$numbers.age, col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, labels = list(x='Time (years)',y = 'Numbers', colour = 'Ageclass'))
    temp.plot.2 = add.title(temp.plot.2, 'Numbers at age')
    
    
    #Num age vs num init
    nums.rel = atlantistools::convert_relative_initial(result$numbers.age)
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
    
    nums.init = dplyr::filter(result$numbers.age, time == 0 & agecl == 1)
    
    RN.age = dplyr::filter(result$RN.age, time == 0 & agecl == 1)
    SN.age = dplyr::filter(result$SN.age, time == 0 & agecl == 1)
    
    nums.RN.SN = dplyr::left_join(nums.init, group.index, by = c('species' = 'LongName'))
    nums.RN.SN$SN_RN = RN.age$atoutput + SN.age$atoutput
    nums.RN.SN$totalN = nums.RN.SN$atoutput*nums.RN.SN$SN_RN
    numscale.f = dplyr::left_join(nums.RN.SN, nums.c, by = 'species')
    
    write.csv(numscale.f, file = paste0(out.dir,run.name,'_init_nums_scalar_for_recruits.csv'),row.names = T)
    
    #num at age %
    num.pct = atlantistools::agg_perc(result$numbers.age, groups = c('time','species'))
    temp.plot.4 = atlantistools::plot_bar(num.pct, fill = 'agecl', wrap = 'species')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4, labels = list(x= "Time (years0",y = 'Numbers (%)'))
    temp.plot.4 = add.title(temp.plot.4, 'Numbers at age - Percent')
    
    #Biomass Pool Grazers
    #Biomass pool grazing
    temp.plot.5 = atlantistools::plot_line(result$grazing)
    temp.plot.5 = ggplot2::update_labels(temp.plot.5,list(x = 'Time (years)',y='Numbers'))
    temp.plot.5 = add.title(temp.plot.5, 'Biomass Pool Grazers')
    
    pdf(file=paste0(out.dir,run.name,' Numbers Timeseries.pdf'), width = 24, height = 24, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    gridExtra::grid.arrange(temp.plot.5)
    dev.off()
    
    
  }
  

# Physics plots -----------------------------------------------------------

  
  #plot physics variables
  if(plot.physics){
    
    #Physics snapshot
    temp.plot.1 = atlantistools::plot_line(result$physics.statevars, wrap = NULL)
    temp.plot.1 = atlantistools::custom_grid(temp.plot.1, grid_x = 'polygon', grid_y = 'variable')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1, list(y = ''))
    temp.plot.1 = add.title(temp.plot.1, 'Physics Snapshot')
    
    #Phys plots
    physics = atlantistools::flip_layers(result$physics.statevars)
    physics = split(physics,physics$variable)
    phys.plots = list()
    for(v in 1:length(physics)){
      phys.plots[[v]] = atlantistools::plot_line(physics[[v]],wrap = NULL)
      phys.plots[[v]] = atlantistools::custom_grid(phys.plots[[v]],grid_x = 'polygon', grid_y = 'layer')
      phys.plots[[v]] = add.title(phys.plots[[v]],names(physics)[v])
      phys.plots[[v]] = ggplot2::update_labels(phys.plots[[v]], labels = list(x = 'time', y= names(phys.plots)[v]))
    }
        
    #fluxes 1
    temp.plot.2 = atlantistools::flip_layers(result$flux)
    temp.plot.2 = atlantistools::plot_line(temp.plot.2,wrap = NULL, col = 'variable')
    temp.plot.2 = atlantistools::custom_grid(temp.plot.2, grid_x = 'polygon', grid_y = 'layer')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, list(y = ''))
    temp.plot.2 = add.title(temp.plot.2, 'Fluxes')
    
    #fluxes 2
    temp.plot.3 = atlantistools::flip_layers(result$source.sink)
    temp.plot.3 = atlantistools::plot_line(temp.plot.3,wrap = NULL, col = 'variable')
    temp.plot.3 = atlantistools::custom_grid(temp.plot.3, grid_x = 'polygon', grid_y = 'layer')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3, list(y = ''))
    temp.plot.3 = add.title(temp.plot.3, 'Source-Sink')
    
    #Changes in wc w/ rel dz
    check.dz = dplyr::left_join(result$dz,result$nominal.dz, by = c('polygon','layer'))
    check.dz = dplyr::mutate(check.dz, check.dz = atoutput.x/atoutput.y)
    check.dz = dplyr::filter(check.dz, !is.na(check.dz))    
    
    temp.plot.4 = atlantistools::plot_line(check.dz,x = 'time',y = 'check.dz',wrap = 'polygon',col = 'layer')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4,list(x='Time (years)',y= expression(dz/nominal_dz)))
    temp.plot.4 = add.title(temp.plot.4,'Change in Water Column Height')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4, list(y = ''))
    
    pdf(file = paste0(out.dir,run.name, ' Physics Timeseries.pdf'),width = 60, height = 18, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    for(i in 1:length(phys.plots)){
      gridExtra::grid.arrange(phys.plots[[i]])
      }
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    dev.off()
      
  }
  

# Growth and Consumption --------------------------------------------------

  
  #plot growth and consumption
  if(plot.growth.cons){
    
    #Growth at ageclass v growth init
    growth.rel = atlantistools::convert_relative_initial(result$growth.age)
    temp.plot.1 = atlantistools::plot_line(growth.rel, col = 'agecl')
    temp.plot.1 = ggplot2::update_labels(temp.plot.1, list(x= 'Time (years)', y = expression(Growth/Growth[init])))
    temp.plot.1 = atlantistools::plot_add_box(temp.plot.1)
    temp.plot.1 = add.title(temp.plot.1, 'Growth at Age vs. Iniital Growth')
    
    #Grwoth vs growth init
    temp.plot.2 = atlantistools::plot_line(result$growth.rel.init, y = 'gr_rel', col = 'agecl')
    temp.plot.2 = ggplot2::update_labels(temp.plot.2, list(y = expression((Growth-Growth[req]/Growth[req]))))
    temp.plot.2 = add.title(temp.plot.2,'Growth vs. Initial Growth')
    
    #Consumptions at age vs. initial
    cons.rel = atlantistools::convert_relative_initial(result$eat_age)
    temp.plot.3 = atlantistools::plot_line(cons.rel,col = 'agecl')
    temp.plot.3 = ggplot2::update_labels(temp.plot.3, list(x = 'Time (years)', y= expression (Consumption/Consumption[init])))
    temp.plot.3 = atlantistools::plot_add_box(temp.plot.3)
    temp.plot.3 = add.title(temp.plot.3, 'Consumption at Age vs. Initial Consumption')
    
    #Consumption at age timeseries
    temp.plot.4 = atlantistools::plot_line(result$eat_age, col = 'agecl')
    temp.plot.4 = ggplot2::update_labels(temp.plot.4, list(x='Time (years)',y='Biomass (tonnes)',color = 'Ageclass'))
    temp.plot.4 = add.title(temp.plot.4, 'Consumption at Age')
    
    pdf(file = paste0(out.dir,run.name,' Growth and Consumption.pdf'),width = 24, height = 24, onefile = T)
    gridExtra::grid.arrange(temp.plot.1)
    gridExtra::grid.arrange(temp.plot.2)
    gridExtra::grid.arrange(temp.plot.3)
    gridExtra::grid.arrange(temp.plot.4)
    dev.off()
    

  }
  

# Cohort timeseries -------------------------------------------------------

  
  #plot cohort timeseries
  if(plot.cohort){
    
    pdf(file = paste0(out.dir, run.name, ' Cohort Timeseries.pdf'),width = 24, height = 18, onefile =T)
    for(i in 1:10){
      age.sub = dplyr::filter(result$numbers.age, agecl == i)
      temp.plot = atlantistools::plot_line(age.sub)
      temp.plot = ggplot2::update_labels(temp.plot,list(x='Time (years)',y = 'Numbers'))
      temp.plot = add.title(temp.plot,paste0('Age-',i))
      gridExtra::grid.arrange(temp.plot)
    }
    dev.off()
    
    
  }
  

# Diet --------------------------------------------------------------------

  
  #Diet figures
  if(plot.diet){
    source(here::here('R','plot_overall_predation.R'))
    
    if(!is.na(result$biomass.consumed)){
      
      diet.plots = atlantistools::plot_diet(result$biomass.consumed, wrap_col =  'agecl', combine_thresh =  3)
      
      pdf(file = paste0(out.dir,run.name, ' Diet Proportions.pdf'),paper = 'A4r',width = 22, height = 16, onefile = T)
      for(i in seq_along(diet.plots)){
        gridExtra::grid.arrange(diet.plots[[i]])
      }
      dev.off()
    }
    
    consumption = get_consumption(prod.file = param.ls$prod.nc,
                                  fgs.file = param.ls$func.groups)
    data.sub = subset_diet(diet.file = param.ls$dietcheck,
                           consumption = consumption,
                           spp.names  = group.index$Code)
    plot_overall_predation(data = data.sub,
                           bioindex.file = paste0(atl.dir,'neus_outputBiomIndx.txt'),
                           min.fract = 0.1,
                           fig.dir = out.dir,
                           file.prefix = run.name)
  }
  

# Spatial biomass ---------------------------------------------------------

  
  #Spatial biomass
  if(plot.spatial.biomass){
    
    temp.plots = atlantistools::plot_spatial_box(result$biomass.spatial.stanza,
                                                 bgm_as_df = atlantistools::convert_bgm(bgm = param.ls$bgm), timesteps = 7)
    pdf(file = paste0(out.dir, run.name, ' Spatial Biomass Box Distribution.pdf'),width = 24, height =18 )
    for( i in seq_along(temp.plots)){
      gridExtra::grid.arrange(temp.plots[[i]])
    }
    dev.off()
    
    temp.plots.2 = atlantistools::plot_spatial_ts(result$biomass.spatial.stanza,
                                                  bgm_as_df = atlantistools::convert_bgm(bgm = param.ls$bgm), vol = result$volume )
    pdf(file = paste0(out.dir, run.name, ' Spatial Biomass Distribution Timeseries.pdf'),width =24, height =18 )
    for(i in seq_along(temp.plots.2)){
      gridExtra::grid.arrange(temp.plots.2[[i]])
    }
    dev.off()
    
  }
  

# LTL plots ---------------------------------------------------------------

  
  #LTL plots
  if(plot.LTL){
    
    #Read in bio data
    biom = read.table(paste0(atl.dir,run.prefix,'BiomIndx.txt'),header= T)
    phyto = read.table(phytopl.history,header =T,sep = ',')
    zoo = read.table(zoopl.history,header = T, sep = ',')
    
    pdf(file = paste0(out.dir,run.name,' LTL Timeseries.pdf'),width = 12, height = 12, onefile =T)
    
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
}
