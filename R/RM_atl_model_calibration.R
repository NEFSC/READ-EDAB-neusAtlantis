# # Using Atlantistools to create plots for model calibration and comparison, 
# # code devloped from the vignette - load preprocessed data then make plots/pdfs
# # RM 20170328
# 
# 
# library("atlantistools")
# library("ggplot2")
# library("gridExtra")
# 
# # Windows
# setwd(choose.dir(default=getwd())) # where run data are saved
# d2=getwd()
# d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved
# 
# #linux
# d1='/home/ryan/Git/atneus_RM'
# d2='/home/ryan/AtlRuns/20170503b'
# #d2='/media/ryan/TOSHIBA EXT/1 RM/10 ATLANTIS transfer/20170413'
# setwd(d2)
# 
# devtools::install('~/Git/atlantistools') #local repo
# ### DIET PLOTS  NO LONGER NECESSARY on desktop, see workaround below
# # DO THIS FIRST...
# MyCol=topo.colors(30)
# trace('get_colpal', edit=T) # Manually add more colors to make this work...
# # # #  get_colpal <-function ()
# {
#   MyCol=topo.colors(30)
#   greys <- c(51, 128, 204, 71, 148, 224, 91, 168, 244, 58,
#              122, 209, 79, 140, 45, 136, 71, 247, 250, 250,
#              250, 250, 250, 250, 250, 250, 250, 250, 250, 250)
#   greys <- grDevices::rgb(cbind(greys, greys, greys), maxColorValue = 255)
#   # col_pal = c(MyCol, greys)
#   col_pal <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"), RColorBrewer::brewer.pal(n = 12, name = "Paired"),
#                greys)
#   return(col_pal)
# }

### copy this into 'get_colpal.r' and commit change, then reinstall atlantistools from local repo
## modify function on disk, then reinstall atlantistools from local repp
# devtools::install_git("C://Users/ryan.morse/Documents/GitHub/atlantistools/.git")
# get_colpal <-function (){
#   greys <- c(51, 128, 204, 71, 148, 224, 91, 168, 244)
#   greys <- grDevices::rgb(cbind(greys, greys, greys), maxColorValue = 255)
#   col_pal <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"), RColorBrewer::brewer.pal(n = 12, name = "Paired"),
#                greys, greys)
#   return(col_pal)
# }


# 
filename=sapply(strsplit(as.character(d2), "/"), tail, 1) # grab last chars of folder
# #
# # # USE TO LOAD Result from atlantistools preprocess (created in 'RM_preprocess_v2.R')
# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# prepr=list.files(d2, pattern=".rdata") # get name
# result<- loadRData(prepr) # load


fig_height2 <- 11
gen_labels <- list(x = "Time [years]", y = "Biomass [t]")

files=list.files(path=d2, pattern='.nc')
nc.str=strsplit(files, '.nc')
lncstr=nchar(nc.str)
ncbase=nc.str[which(lncstr==min(lncstr))] #get base nc file name
# nc_gen    <- file.path(d2, paste(ncbase, '.nc', sep=""))
# nc_prod   <- file.path(d2, paste(ncbase, 'PROD.nc', sep=""))
# dietcheck <- file.path(d2, paste(ncbase, 'DietCheck.txt', sep=""))
# yoy       <- file.path(d2, paste(ncbase, 'YOY.txt', sep=""))
# ssb       <- file.path(d2, paste(ncbase, 'SSB.txt', sep=""))

# External recruitment data
# ex_rec_ssb <- read.csv(file.path(d, "setas-ssb-rec.csv"), stringsAsFactors = FALSE)
# ex_rec_ssb <- read.csv(ssb, stringsAsFactors = F)

# External biomass data NOT CURRENTLY USED
# ex_bio <- read.csv(file.path(d, "setas-bench.csv"), stringsAsFactors = FALSE)

# bgm file
bgm       <- file.path(d1, "neus_tmerc_RM2.bgm") #30_v15.bgm")


### select box plot time series of benthic 

test=result$biomass_spatial_stanza[which(result$biomass_spatial_stanza$layer==ll & result$biomass_spatial_stanza$polygon==tb),]
ii=unique(test$species)
pdf(file=paste(d4,filename,'_Box_',tb,'_bottom.pdf', sep=''))
boxbgm=load_box(bgm)
barea=boxbgm$boxes[[tb+1]]$area # get area, add 1 boxes start at 0
for (x in 1:length(ii)){
  iii=ii[x]
plot(test$atoutput[which(test$species==iii)]/barea~test$time[which(test$species==iii)], 
     type='l',main=paste(iii, 'Box', tb, 'layer', ll, sep=' '), ylab='Mg N m-2', xlab='time')
}
dev.off()

####__________Overall Biomass_____________
df_bio <- combine_groups(result$biomass, group_col = "species", combine_thresh = 10)
plot <- plot_bar(df_bio)
update_labels(plot, labels = gen_labels)
ggsave(paste(d4,filename," overall biomass.png", sep=''), width=7, height=4, scale=1, dpi=96)

df_bio <- combine_groups(result$biomass, group_col = "species", combine_thresh = 20)
plot <- plot_bar(df_bio)
update_labels(plot, labels = gen_labels)
ggsave(paste(d4,filename," overall biomass2.png", sep=''), width=20, height=11, scale=1, dpi=96)
###_________ Biomass timeseries#_______________________
# pdf(file=paste(d4,filename, '_biomassTS.pdf', sep='')) # does not work as configured
plot <- plot_line(result$biomass)
update_labels(plot, labels = gen_labels)
ggsave(paste(d4, filename," biomass timeseries2.png", sep=''), width=20, height=17, dpi=96)

# dev.off()
### Biomass at age timeseries#________________________
plot <- plot_line(result$biomass_age, col = "agecl")
update_labels(p = plot, labels = c(gen_labels, list(colour = "Ageclass")))
ggsave(paste(d4,filename," biomass at age timeseries.png", sep=''), width=20, height=17, dpi=96)

### Biomass at age relative to initial biomass timeseries#________________________
df_rel <- convert_relative_initial(result$biomass_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(biomass/bio[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," biomass at age rel init.png", sep=''), width=20, height=17, dpi=96)


# plot length at age - may need to call: result$biomass_age2 added 20181102, result$length_age modified 20181127
init_length=read.csv(file=paste(d1, '/vertebrate_init_length_cm.csv', sep=''), header = T, stringsAsFactors = F)
init_length=init_length[order(init_length$Long.Name),]
ii=unique(result$length_age$species)
pdf(file=paste(d4,filename,'_tuning_length_age2.pdf', sep=''))
for (x in 1:length(ii)){
  iii=ii[x]
  test=result$length_age %>% filter(species == iii, time > 0)
  test2=init_length[,3:13] %>% filter(Long.Name == iii)
  boxplot(test$atoutput ~ test$agecl, ylab='cm', xlab='cohort', main=iii)
  points(seq(1:10), test2[2:11], col='red', pch=17)
  }
dev.off()

### Length at age timeseries#________________________
plot <- plot_line(result$length_age, col = "agecl")
gen_labels <- list(x = "Time [years]", y = "Length [cm]")
update_labels(p = plot, labels = c(gen_labels, list(colour = "Ageclass")))
ggsave(paste(d4,filename," length at age timeseries3.png", sep=''), width=20, height=17, dpi=96)



#### ________ load biol info for mum and C -> compare length at age to init___________
gps=get_age_acronyms(fgs)
fgs_data=load_fgs(fgs)
code_relations=fgs_data[,c('Code', 'LongName')]
mum_age=prm_to_df_ages(prm_biol, fgs, group=gps, parameter = "mum") %>%   spread(agecl, mum)
mum_age=left_join(mum_age, code_relations, by=c('species'='LongName'))
C_age=prm_to_df_ages(prm_biol, fgs, group=gps, parameter = "C") %>% spread(agecl, c)
C_age=left_join(C_age, code_relations, by=c('species'='LongName'))
# 
# set_single_prm <- function(group, parameter) {
#   if (parameter %in% c("AgeClassSize", "age_mat")) {
#     paste(group, parameter, sep = "_")
#   } else {
#     paste(parameter, group, sep = "_")
#   }
# }
# #get biomass pool values
# prm_to_df_RM <- function(prm_biol, fgs, group, parameter) {
#   # Extract data!
#   prms <- lapply(parameter, set_single_prm, group = group)
#   prm_biol_new <- readLines(con = prm_biol, warn = FALSE)
#   test=data.frame(prms)
#   test[,1]=paste(test[,1], "_T15", sep="")
#   # test=test[1:27,]
#   t=data.frame(test)
#   prms=t
#   # prms=list(test)
#   # prm_t  <- do.call(rbind, prms)[,1]
#   prm_t=test
#   no_prm <- which(is.na(charmatch(prm_t, prm_biol_new)))
#   if (sum(no_prm) > 1 && unlist(strsplit(prm_t[no_prm], '_'))[2] == 'AgeClassSize') {
#     prms2  <- prms[-no_prm]
#     values <- lapply(prms2, extract_prm, prm_biol = prm_biol)
#     sps    <- which(load_fgs(fgs = fgs)$Code %in% group)
#     extr   <- load_fgs(fgs = fgs)$NumAgeClassSize[sps]
#     values[[length(values) + 1]] <- extr
#     parameter                    <- c(parameter[-no_prm], parameter[no_prm])
#   } else {
#     values <- lapply(prms, extract_prm, prm_biol = prm_biol)
#   }
#   # Combine to df!
#   df         <- as.data.frame(do.call(cbind, values))
#   names(df)  <- tolower(parameter)
#   df$species <- group#[1:27]
#   df$species <- convert_factor(data_fgs = load_fgs(fgs = fgs), col = df$species)
#   df <- dplyr::select_(df, .dots = c("species", sort(names(df)[-ncol(df)])))
#   return(df)
# }
# get_Pred_nonage_acronyms <- function(fgs){
#   fgs_df <- load_fgs(fgs = fgs)
#   result <- fgs_df$Code[which(fgs_df$NumCohorts <= 2 & fgs_data$isPredator==1)]
#   return(result)
# }
# get_plant_nonage_acronyms <- function(fgs){
#   fgs_df <- load_fgs(fgs = fgs)
#   result <- fgs_df$Code[which(fgs_df$NumCohorts <= 2 & fgs_data$isPredator==0)]
#   return(result)
# }
# gps2=get_Pred_nonage_acronyms(fgs)
# gps_plant=get_plant_nonage_acronyms(fgs)
# gps_plant=gps_plant[1:7]
# mum_bio=prm_to_df_RM(prm_biol, fgs, group=gps2, parameter = "mum") #%>%   spread(mum)
# mum_bio=left_join(mum_bio, code_relations, by=c('species'='LongName'))
# C_bio=prm_to_df_RM(prm_biol, fgs, group=gps2, parameter = "C")
# C_bio=left_join(C_bio, code_relations, by=c('species'='LongName'))
# mum_plant=prm_to_df_RM(prm_biol, fgs, group=gps_plant, parameter = "mum")
# mum_plant=left_join(mum_plant, code_relations, by=c('species'='LongName'))
# test=result$biomass
# test=test[which(test$species %in% mum_bio$species),]
# test.init=test[which(test$time==0),] # intial biomass values
# # test.ratio=test/test.init

# df_rel <- convert_relative_initial(result$biomass) %>%
#   group_by(species) %>%
#   summarise(avg=mean(atoutput)) %>%
#   spread(avg)  %>% filter (mum_bio$species)
# mum.scale=mum_age[,2:11]*1/RN_RNinit; row.names(mum.scale)=mum_age$Code
# mum.scale=mum.scale[order(row.names(mum.scale)),]
# C.scale=C_age[,2:11]*1/RN_RNinit; row.names(C.scale)=C_age$Code
# C.scale=C.scale[order(row.names(C.scale)),]
# write.csv(mum.scale, file='newMum_RNbased.csv', row.names = T)
# write.csv(C.scale, file='newC_RNbased.csv', row.names = T)


### scale mum and C to length at age relative to initial conditions ###
len_age_mn=result$length_age %>% group_by(species, agecl) %>%
  summarise(avg=mean(atoutput)) %>%
  spread(agecl, avg)
lng.lng_int=len_age_mn[,2:11]/init_length[,4:13] # mean length at age divided by initial lenght at age, use to scale mum and C
row.names(lng.lng_int)=mum_age$Code
# Now scale mum and C by difference between length at age relative to initial conditions
mum.scale=mum_age[,2:11]/lng.lng_int; row.names(mum.scale)=mum_age$Code
mum.scale=mum.scale[order(row.names(mum.scale)),]
C.scale=C_age[,2:11]/lng.lng_int; row.names(C.scale)=C_age$Code
C.scale=C.scale[order(row.names(C.scale)),]
write.csv(mum.scale, file='newMum_lengthbased.csv', row.names = T)
write.csv(C.scale, file='newC_lengthbased.csv', row.names = T)

### AND/OR... Scale to RN relative to RN Init ###
RN_mn=result$resn_age %>% group_by(species, agecl) %>%
  summarise(avg=mean(atoutput)) %>%
  spread(agecl, avg)
RN_init=result$resn_age %>% filter(time==0) %>%
  spread(agecl, atoutput)
RN_RNinit=round(RN_mn[,2:11]/RN_init[,3:12], digits = 2); row.names(RN_RNinit)=mum_age$Code
## test to compare (yes, same as below)
df_rel <- convert_relative_initial(result$resn_age) %>%
  group_by(species, agecl) %>%
  summarise(avg=mean(atoutput)) %>%
  spread(agecl, avg)
mum.scale=mum_age[,2:11]/RN_RNinit; row.names(mum.scale)=mum_age$Code
mum.scale=mum.scale[order(row.names(mum.scale)),] # reorder to code not long name
# mum.scale=round(mum.scale, digits=2)
C.scale=C_age[,2:11]/RN_RNinit; row.names(C.scale)=C_age$Code
C.scale=C.scale[order(row.names(C.scale)),]
# C.scale=round(C.scale, digits=2)
growth_scalar=1/RN_RNinit
growth_scalar=growth_scalar[order(row.names(growth_scalar)),]
write.csv(growth_scalar, file=paste(d4,filename,'RNbased_growth_scalar.csv', sep=''), row.names = T)
write.csv(mum.scale, file=paste(d4,filename,'newMum_RNbased.csv', sep=''), row.names = T)
write.csv(C.scale, file=paste(d4,filename,'newC_RNbased.csv', sep=''), row.names = T)
mum.age=mum_age[order(mum_age$Code ),]
C.age=C_age[order(C_age$Code ),]
write.csv(mum.age, file=paste(d4,filename,'_Mum_used.csv', sep=''), row.names = T)
write.csv(C.age, file=paste(d4,filename,'_C_used.csv', sep=''), row.names = T)
mum.C=round(mum_age[,2:11]/C_age[,2:11], digits=2); row.names(mum.C)=mum_age$Code # ratio of mum:C (originally ~ 10)
mum.C=mum.C[order(row.names(mum.C)),]
write.csv(mum.C, file=paste(d4,filename,'_mum_to_C_ratio.csv', sep=''), row.names=T)


### get SN only, apply values for reality check of mum and C used (Pethybridge et al 2019)
SN=result$structn_age %>% filter(time==0)
SN$highMum=SN$atoutput*0.1
SN$lowMum=SN$atoutput*0.05
SN$lowC=SN$atoutput*0.01
SN$highC=SN$atoutput*0.06
test2=left_join(SN, code_relations, by=c('species'='LongName'))
## add used mum and C values, transform wide to long
test=mum_age[,2:12]
library(reshape2)
t=melt(test)
t$variable=as.numeric(t$variable)
test3=left_join(test2, t, by=c('Code', 'agecl'='variable'))
#add C
test=C_age[,2:12]
t=melt(test)
t$variable=as.numeric(t$variable)
test4=left_join(test3, t, by=c('Code', 'agecl'='variable'))
## check values compared to reference plus/minus 5%
test4$mumbelowhigh=test4$value.x<(test4$highMum*1.05)
test4$mumoverlow=test4$value.x>(test4$lowMum*0.95)
test4$Cbelowhigh=test4$value.y<(test4$highC*1.05)
test4$Coverlow=test4$value.y>(test4$lowC*0.95)
write.csv(test4, file=paste(runfile,'_SN_sanity_check_on_mum_and_C.csv', sep=''), row.names = F)


### get initial conditions values RN+SN*nums, dont forget about scalar in run file...
# test=nums_age %>% filter(time==0, agecl==1)
# gps=get_age_acronyms(fgs)
# RN=result$resn_age %>% filter(time==0, agecl==1)
# SN=result$structn_age %>% filter(time==0, agecl==1)
# fgs_data=load_fgs(fgs)
# code_relations=fgs_data[,c('Code', 'LongName')]
# test2=left_join(test, code_relations, by=c('species'='LongName'))
# test2$SN_RN=RN$atoutput+SN$atoutput
# test2$totalN=test2$atoutput*test2$SN_RN
# write.csv(test2, file='initial_cond_ageclass_1.csv', row.names =F)

### load pPrey matrix; ADJUST if necessary
# dm <- load_dietmatrix(prm_biol, fgs)
# test=dm %>% filter(prey=='HAL')
# new_diet <- write_diet(dietmatrix, prm_biol, save_to_disc = T) # overwrite diet matrix after modifications


###_________Number timeseries#________________________
plot <- plot_line(result$nums)
update_labels(p = plot, labels = list(x = "Time [years]", y = "Numbers"))
ggsave(paste(d4,filename," numbers timeseries.png", sep=''), width=20, height=17, dpi=96)

### _______ Number of recruits #__________________________
plot <- plot_line(result$ssb_rec, y='rec')
update_labels(p = plot, labels = list(x = "Time [days]", y = "Numbers"))
ggsave(paste(d4,filename," recruits timeseries.png", sep=''), width=20, height=17, dpi=96)

### _______ Number of recruits #__________________________
plot <- plot_line(result$ssb_rec, y='ssb')
update_labels(p = plot, labels = list(x = "Time [days]", y = "Numbers"))
ggsave(paste(d4,filename," ssb timeseries.png", sep=''), width=20, height=17, dpi=96)

### ______ recruits per spawner_______ added 20181129 RM
result$ssb_rec$rec.ssb=result$ssb_rec$rec/result$ssb_rec$ssb
plot <- plot_line(result$ssb_rec, y='rec.ssb', yexpand = T)
update_labels(p = plot, labels = list(x = "Time [days]", y = "Numbers"))
ggsave(paste(d4,filename," Rec_per_SSB timeseries.png", sep=''), width=20, height=17, dpi=96)



### Numbers at age timeseries#________________________
plot <- plot_line(result$nums_age, col = "agecl")
update_labels(p = plot, labels = list(x = "Time [years]", y = "Numbers", colour = "Ageclass"))
ggsave(paste(d4,filename," numbers at age timeseries.png", sep=''), width=20, height=17, dpi=96)
###____________SSB and recruitment NEED External Data input for this to work
# plot_rec(result$ssb_rec, ex_data = ex_rec_ssb)

### Age class 1-10 numbers timeseries ###
for (ii in 1:10){
test=filter(result$nums_age, agecl==ii)
plot=plot_line(test)
update_labels(p = plot, labels = list(x = "Time [years]", y = "Numbers"))
ggsave(paste(d4,filename," Age-",ii, " Cohort timeseries.png", sep=''), width=20, height=17, dpi=96)
}

##____________PHYSICS____________________________
plot <- plot_line(result$physics, wrap = NULL)
custom_grid(plot, grid_x = "polygon", grid_y = "variable")
ggsave(paste(d4,filename," physics snapshot.png", sep=''), width=10, height=7, dpi=96)

physics <- result$physics %>%
  flip_layers() %>%
  split(., .$variable)

plots <- lapply(physics, plot_line, wrap = NULL) %>% 
  lapply(., custom_grid, grid_x = "polygon", grid_y = "layer")

for (i in seq_along(plots)) {
  cat(paste0("## ", names(plots)[i]), sep = "\n")
  plot <- update_labels(plots[[i]], labels = list(y = names(plots)[i]))
  print(plot)
  cat("\n\n")
  ggsave(paste(d4,filename,' ',names(plots)[i]," physics.png", sep=''), width=10, height=7, dpi=96)
}
### Fluxes 1
plot <- flip_layers(result$flux) %>% 
  plot_line(wrap = NULL, col = "variable")
custom_grid(plot, grid_x = "polygon", grid_y = "layer")
ggsave(paste(d4,filename," fluxes 1.png", sep=''), width=10, height=7, dpi=96)

### Fluxes 2
plot <- flip_layers(result$sink) %>% 
  plot_line(wrap = NULL, col = "variable")
custom_grid(plot, grid_x = "polygon", grid_y = "layer")
ggsave(paste(d4,filename," fluxes 2.png", sep=''), width=10, height=7, dpi=96)

### Change in wc height relative to nominal dz
check_dz <- result$dz %>% 
  dplyr::left_join(result$nominal_dz, by = c("polygon", "layer")) %>% 
  dplyr::mutate(check_dz = atoutput.x / atoutput.y) %>% 
  dplyr::filter(!is.na(check_dz)) # remove sediment layer

plot <- plot_line(check_dz, x = "time", y = "check_dz", wrap = "polygon", col = "layer")
update_labels(plot, list(x = "Time [years]", y = expression(dz/nominal_dz)))
ggsave(paste(d4,filename," change in wc height.png", sep=''), width=10, height=7, dpi=96)


###____________________CALIBRATION PLOTS_____________________________

### SN
df_rel <- convert_relative_initial(result$structn_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(SN/SN[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," SN_SN init.png", sep=''), width=20, height=17, dpi=96)

### RN
df_rel <- convert_relative_initial(result$resn_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(RN/RN[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," RN_RN init.png", sep=''), width=20, height=17, dpi=96)

### Length at age relative to initial conditions; RM added 20181127
df_rel <- convert_relative_initial(result$length_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(length/length[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," length_age lenght_init.png", sep=''), width=20, height=17, dpi=96)

### Biomass per ageclass
df_rel <- convert_relative_initial(result$biomass_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Biomass/Biomass[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," Biomass at age_Bio age init.png", sep=''), width=20, height=17, dpi=96)

### biomass per box timeseries
plot <- plot_line(result$bio_box)
custom_grid(plot, grid_x = "polygon", grid_y = "species")
ggsave(paste(d4,filename," Biomass per box timeseries.png", sep=''), width=30, height=34, dpi=96)

### Numbers per box timeseries
plot <- plot_line(result$nums_box)
custom_grid(plot, grid_x = "polygon", grid_y = "species")
ggsave(paste(d4,filename," Numbers per box timeseries.png", sep=''), width=30, height=34, dpi=96)

plot <- plot_line(result$strucn_box)
custom_grid(plot, grid_x = "polygon", grid_y = "species")
ggsave(paste(d4,filename," SN per box timeseries.png", sep=''), width=30, height=34, dpi=96)

plot <- plot_line(result$resn_box)
custom_grid(plot, grid_x = "polygon", grid_y = "species")
ggsave(paste(d4,filename," RN per box timeseries.png", sep=''), width=30, height=34, dpi=96)


### Eat per ageclass
df_rel <- convert_relative_initial(result$eat_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Cons./Cons.[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," Consumption at age_Cons init.png", sep=''), width=20, height=17, dpi=96)

## Now plot without comparing to initial values... just timeseries
plot <- plot_line(result$eat_age, col = "agecl")
gen_labels <- list(x = "Time [years]", y = "Biomass [t]")
update_labels(p = plot, labels = c(gen_labels, list(colour = "Ageclass")))
ggsave(paste(d4,filename," Consumption at age timeseries.png", sep=''), width=20, height=17, dpi=96)

### Plot biomass pool grazing timeseries
plot <- plot_line(result$grazing)
update_labels(p = plot, labels = list(x = "Time [years]", y = "Numbers"))
ggsave(paste(d4,filename," Bio pool grazing timeseries.png", sep=''), width=20, height=17, dpi=96)

### Plot biomass pool biomass timeseries
plot <- plot_line(result$biomass_inverts)
update_labels(p = plot, labels = list(x = "Time [years]", y = "Biomass"))
ggsave(paste(d4,filename," Invert biomass timeseries.png", sep=''), width=20, height=17, dpi=96)

## Plot biomass pool biomass by box timeseries
plot <- plot_line(result$bio_invert_box)
custom_grid(plot, grid_x = "polygon", grid_y = "species")
ggsave(paste(d4,filename," Invert biomass by box.png", sep=''), width=30, height=20, dpi=96)

### Growth per ageclass
df_rel <- convert_relative_initial(result$growth_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Growth/Growth[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," Growth at age_growth init.png", sep=''), width=20, height=17, dpi=96)

### Growth relative to initial conditions
plot <- plot_line(result$growth_rel_init, y = "gr_rel", col = "agecl")
update_labels(plot, list(y = expression((Growth - Growth[req])/Growth[req])))
ggsave(paste(d4,filename," Growth_growth init.png", sep=''), width=20, height=17, dpi=96)

### Numbers per ageclass
df_rel <- convert_relative_initial(result$nums_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Numbers/Numbers[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," Numbers at age_Num init.png", sep=''), width=20, height=17, dpi=96)

### Numbers per ageclass, use to scale recruitment values from initial conditions ageclass 1 (nums*RN+SN)
df_rel <- convert_relative_initial(result$nums_age)%>%
  group_by(species, agecl) %>%
  summarise(avg=mean(atoutput)) %>%
  spread(agecl, avg)
numscale=1/rowMeans(df_rel[,2])
# numscale=1/(df_rel[,2]) # scale to mean of all cohorts
numsc=data.frame(df_rel[,1]) # scale to first year only
numsc$scale=numscale
# write.csv(numsc, file='new_num_scalar_recruits.csv', row.names = T)
test=nums_age %>% filter(time==0, agecl==1)
gps=get_age_acronyms(fgs)
RN=result$resn_age %>% filter(time==0, agecl==1)
SN=result$structn_age %>% filter(time==0, agecl==1)
fgs_data=load_fgs(fgs)
code_relations=fgs_data[,c('Code', 'LongName', 'Index')]
test2=left_join(test, code_relations, by=c('species'='LongName'))
test2$SN_RN=RN$atoutput+SN$atoutput
test2$totalN=test2$atoutput*test2$SN_RN
# write.csv(test2, file=paste(runfile,'initial_cond_ageclass_1.csv', sep=''), row.names =F)
numscale.f=left_join(test2, numsc, by='species')
write.csv(numscale.f, file=paste(runfile,'Init_nums_scalar_for_recruits.csv', sep=''), row.names = F)


### Biomass
df_rel <- convert_relative_initial(result$biomass)
plot <- plot_line(df_rel)
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Biomass/Biomass[init])))
plot_add_box(plot)
ggsave(paste(d4,filename," Biomass_Bio init.png", sep=''), width=20, height=17, dpi=96)

### Numbers at age
df <- agg_perc(result$nums_age, groups = c("time", "species"))
plot <- plot_bar(df, fill = "agecl", wrap = "species")
update_labels(plot, labels = list(x = "Time [years]", y = "Numbers [%]"))
ggsave(paste(d4,filename," Numbers at age percent.png", sep=''), width=20, height=17, dpi=96)

### Biomass at age
df <- agg_perc(result$biomass_age, groups = c("time", "species"))
plot <- plot_bar(df, fill = "agecl", wrap = "species")
update_labels(plot, labels = list(x = "Time [years]", y = "Biomass [%]"))
ggsave(paste(d4,filename," Biomass at age percent.png", sep=''), width=20, height=17, dpi=96)

# ### DIET PLOTS
# # DO THIS FIRST...
# trace('get_colpal', edit=T) # Manually add more colors to make this work...
# # #  get_colpal <-function ()
# {
#   greys <- c(51, 128, 204, 71, 148, 224, 91, 168, 244, 58,
#              122, 209, 79, 140, 45, 136, 71, 247, 250, 250,
#              250, 250, 250, 250, 250, 250, 250, 250, 250, 250)
#   greys <- grDevices::rgb(cbind(greys, greys, greys), maxColorValue = 255)
#   col_pal <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired"),
#                greys)
#   return(col_pal)
# }
if (make_diet_pdf == 1){
  if(!is.na(result$biomass_consumed)){
    plots <- plot_diet(result$biomass_consumed, wrap_col = "agecl", combine_thresh = 3)
    pdf(file=paste(d4,filename, '_diet_proportions.pdf', sep=''),paper='A4r',width=11, height=8)
    for (i in seq_along(plots)) {
      cat(paste0("## Diet plot ", i, ": ", names(plots)[i]), sep = "\n")
      gridExtra::grid.arrange(plots[[i]])
      cat("\n\n")
    }
    dev.off()
  }
}

if (make_spatial_plots == 1){
  ####__________ SPATIAL DISTRIBUTION PLOTS____________________________________
  ### Spatial Plots 1
  plots <- plot_spatial_box(result$biomass_spatial_stanza, bgm_as_df = convert_bgm(bgm = bgm), timesteps = 7)
  pdf(file=paste(d4,filename, '_spatial biomass box distribution.pdf', sep=''), paper='A4r', width=11, height=8)
  for (i in seq_along(plots)) {
    cat(paste0("## Spatial Plot ", i, ": ", names(plots)[i]), sep = "\n")
    gridExtra::grid.arrange(plots[[i]])
    cat("\n\n")
  }
  dev.off()
  
  ### Spatial Plots 2
  plots <- plot_spatial_ts(result$biomass_spatial_stanza, bgm_as_df = convert_bgm(bgm = bgm), vol = result$vol)
  pdf(file=paste(d4,filename, '_spatial biomass distribution timeseries.pdf', sep=''),paper='A4r', width=11, height=8)
  for (i in seq_along(plots)) {
    cat(paste0("## Spatial Plot ", i, ": ", names(plots)[i]), sep = "\n")
    gridExtra::grid.arrange(plots[[i]])
    cat("\n\n")
  }
  dev.off()
}


### Plot Spatial Overlap Schoener Index of diet matchups STILL WORKING ON THIS
# 386:length(sp_overlap) 20180927dta not complete
# pdf(file=paste(d4,filename, '_spatial overlap of predator and prey.pdf', sep=''),paper='A4r', width=11, height=8)
# for (i in 1:length(sp_overlap)) {
#   print(plot_spatial_overlap(sp_overlap[i]))
# }
# dev.off()


#_______________________________________________________________
if (make_LTL_plots==1){
  #biom=read.table('/home/ryan/AtlRuns/20170914a/atneus_v10_newcodebaseAgeBiomIndx.txt', header=T) # v1.0 on new codebase
  biom=read.table(paste(d3, 'atneus_v10_newcodebaseBiomIndx.txt', sep=''), header=T) # v1.0 on new codebase
  bio1=read.table(paste(d3, 'neusDynEffort_Base_Effort_BiomIndx.txt', sep=''), header=T) # v1.0 on old codebase
  bio2=read.table(paste(d2, '/', ncbase, 'BiomIndx.txt', sep=''), header=T) # v1.5 run
  phyto=read.table(paste(d3, 'phytoplankton_timeseries_biomass_tonnes_1998_2016.csv', sep=''),header=T, sep=',')
  zoo=read.table(paste(d3, 'Zooplankton_total_biomass_tonnes_N_20yrs.csv', sep=''), header =T, sep=',')
  
  # #diatom
  # plot(biom$PL~biom$Time, type='l') # v1.0 new code
  # lines(bio1$PL~bio1$Time, type='l', col='red') # v1.0 old code
  # lines(bio2$PL~bio2$Time, type='l', col='blue') #v1.5
  # lines(phyto$PL.ts~phyto$days, type='l', col='green') # measured
  # 
  # #pico
  # plot(biom$PS~biom$Time, type='l') # v1.0 new code
  # lines(bio1$PS~bio1$Time, type='l', col='red') # v1.0 old code
  # lines(bio2$PS~bio2$Time, type='l', col='blue') #v1.5
  # lines(phyto$PS.ts~phyto$days, type='l', col='green') # measured
  # 
  #picoplankton (v1.5 first)
  png(filename=paste(d4,filename,'PS.png', sep=''))
  mmax=max(c(bio2$PS, phyto$PS))
  plot(bio2$PS~bio2$Time, type='l', ylim=c(0,max(c(bio2$PS, phyto$PS))))
  lines(phyto$PS.ts~phyto$days, type='l', col='red')
  lines(bio1$PS~bio1$Time, type='l', col='blue')
  lines(biom$PS~biom$Time, type='l', col='green')
  legend('topleft', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
  
  #diatom (v1.5 first)
  png(filename=paste(d4,filename,'PL.png', sep=''))
  plot(bio2$PL~bio2$Time, type='l', ylim=c(0,max(c(bio2$PL, phyto$PL))))
  lines(phyto$PL.ts~phyto$days, type='l', col='red')
  lines(bio1$PL~bio1$Time, type='l', col='blue')
  lines(biom$PL~biom$Time, type='l', col='green')
  legend('topright', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
  
  #dinoflag (v1.5 first)
  png(filename=paste(d4,filename,'DF.png', sep=''))
  plot(bio2$DF~bio2$Time, type='l', ylim=c(0,max(c(bio2$DF, phyto$DF))))
  lines(phyto$DF.ts~phyto$days, type='l', col='red')
  lines(bio1$DF~bio1$Time, type='l', col='blue')
  lines(biom$DF~biom$Time, type='l', col='green')
  legend('topright', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
  
  #Carn Zoo (v1.5 first)
  png(filename=paste(d4,filename,'ZL.png', sep=''))
  plot(bio2$ZL~bio2$Time, type='l', ylim=c(0,max(c(bio2$ZL, phyto$ZL))))
  lines(zoo$ZL~zoo$Time, type='l', col='red')
  lines(bio1$ZL~bio1$Time, type='l', col='blue')
  lines(biom$ZL~biom$Time, type='l', col='green')
  legend('topright', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
  
  #Copepod (v1.5 first)
  png(filename=paste(d4,filename,'ZM.png', sep=''))
  plot(bio2$ZM~bio2$Time, type='l', ylim=c(0,max(c(bio2$ZM, phyto$ZM))))
  lines(zoo$ZM~zoo$Time, type='l', col='red')
  lines(bio1$ZM~bio1$Time, type='l', col='blue')
  lines(biom$ZM~biom$Time, type='l', col='green')
  legend('topleft', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
  
  #Small Zoo (v1.5 first)
  png(filename=paste(d4,filename,'ZS.png', sep=''))
  plot(bio2$ZS~bio2$Time, type='l', ylim=c(0,max(c(bio2$ZS, phyto$ZS))))
  lines(zoo$ZS~zoo$Time, type='l', col='red')
  lines(bio1$ZS~bio1$Time, type='l', col='blue')
  lines(biom$ZS~biom$Time, type='l', col='green')
  legend('topright', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
  
  #GelatZoo (v1.5 first)
  png(filename=paste(d4,filename,'ZG.png', sep=''))
  plot(bio2$ZG~bio2$Time, type='l', ylim=c(0,max(c(bio2$ZG, phyto$ZG))))
  lines(zoo$ZG~zoo$Time, type='l', col='red')
  lines(bio1$ZG~bio1$Time, type='l', col='blue')
  lines(biom$ZG~biom$Time, type='l', col='green')
  legend('topright', legend = c(filename, 'data', 'v1.0 old', 'v1.0 new'), lty=c(1,1,1,1),col=c('black', 'red', 'blue','green'), bty='n')
  dev.off()
}
