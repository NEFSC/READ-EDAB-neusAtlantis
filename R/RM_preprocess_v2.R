# library("atlantistools")
# library("ggplot2")
# library("gridExtra")
# library("dplyr")
# library("tidyr")
# # # # 
# # # # #_____________________________
# # # ## WINDOWS
# # # setwd(choose.dir(default=getwd())) # where run data are saved
# # setwd('E:/AtlantisRun/20161103/tes/20181110c')
# # d2=getwd()
# # d1='C:/Users/ryan.morse/Documents/GitHub/atneus_RM' #where (PRM, bgm, group data) are saved
# # # d1="C:/Users/ryan.morse/Documents/GitHub/atneus_RM_20180827"
# # 
# # # #linux
# # d1='/home/ryan/Git/atneus737e3d' # for NEUS 1.0 on new code base RM
# d1='/home/ryan/Git/atneus_RM'
# d2='/home/ryan/AtlRuns/20181110b'
# setwd(d2)

files=list.files(path=d2, pattern='.nc')
nc.str=strsplit(files, '.nc')
lncstr=nchar(nc.str)
ncbase=nc.str[which(lncstr==min(lncstr))] #get base nc file name
nc_gen    <- file.path(d2, paste(ncbase, '.nc', sep=""))
nc_prod   <- file.path(d2, paste(ncbase, 'PROD.nc', sep=""))
dietcheck <- file.path(d2, paste(ncbase, 'DietCheck.txt', sep=""))
yoy       <- file.path(d2, paste(ncbase, 'YOY.txt', sep=""))
ssb       <- file.path(d2, paste(ncbase, 'SSB.txt', sep=""))

xml.files=list.files(path=d2, pattern='.xml')
xml.str=strsplit(xml.files, '.xml')
prm_run <- file.path(d1, paste(xml.str[[2]], '.prm',sep=''))
prm_run #make sure
prm_biol <- file.path(d1, paste(xml.str[[1]], '.prm',sep=''))
prm_biol #make sure

fgs       <- file.path(d1, paste(xml.str[[3]], '.csv',sep='')) #file.path(d1, "NeusGroups_v15_LTLonly.csv") #unix.csv")
fgs #make sure
# bgm.files=list.files(path=d1, pattern='.bgm')
bgm.files="neus_tmerc_RM.bgm"
bgm       <- file.path(d1, bgm.files) #"neus_tmerc_RM.bgm")

## read log file to get input command for Atlantis run
con <- file("log.txt","r")
logfile_lines <- readLines(con,n=15)
close(con)
run.cmd=logfile_lines[14]
run.cmd #print to make sure
test=strsplit(run.cmd, split = ' ')
init.file=test[[1]][3]
init = file.path(d1, init.file)
init # make sure
# init      <- file.path(d1, 'RMinit_notsohighvertmix.nc')# "RMinit_newvalues2017.nc")
init=paste(d1,'/RMinitnofill.nc', sep='') # dropped _FillValue to make init_growth work 20180412 (see history in cdf version for command)
# ran this command: ncatted -O -a _FillValue,,d,, RMinit_2018.nc RMinitnofill_2018.nc
# ncks -d time,0,9 in.nc out.nc to remove time steps from v1.0 (use 't' not 'time') 

# ### SANITY CHECK ON INITIAL CONDITIONS
# data1 <- sc_init(init, prm_biol, fgs, bboxes)


#__________________________________
bboxes <- get_boundary(boxinfo = load_box(bgm))
bps <- load_bps(fgs, init)
bio_conv <- get_conv_mgnbiot(prm_biol)

# By default data from all groups within the simulation is extracted!
groups <- get_groups(fgs)
groups_age <- get_age_groups(fgs)
groups_rest <- groups[!groups %in% groups_age]
codes_age=get_age_acronyms(fgs)
data_fgs=load_fgs(fgs)

# Read in raw untransformed data from nc_gen
vars <- list("Nums", "StructN", "ResN", "N")
grps <- list(groups_age, groups_age, groups_age, groups_rest)
dfs_gen <- Map(load_nc, select_variable = vars, select_groups = grps,
               MoreArgs = list(nc = nc_gen, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes))

# Read in raw untransformed data from nc_prod
vars <- list("Eat", "Grazing", "Growth")
grps <- list(groups_age, groups_rest, groups_age)
dfs_prod <- Map(load_nc, select_variable = vars, select_groups = grps,
                MoreArgs = list(nc = nc_prod, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes))

# trying to load diatom production, etc... (things in bps do not work here)
fgs_data=load_fgs(fgs)
grps_noPred <- fgs_data$Name[fgs_data[, names(fgs_data)[names(fgs_data) %in% c("isPredator", "IsPredator")]] == 0]
grps_noPred=grps_noPred[4:6]
vars <- list("Prodn")
# grps <- list(groups_rest)
dfs_noPred_prod <- Map(load_nc, select_variable = vars, select_groups = grps_noPred,
                MoreArgs = list(nc = nc_prod, bps = bps, fgs = fgs, prm_run = prm_run, bboxes = bboxes))


# Read in physics
flux <- load_nc_physics(nc = nc_gen, select_physics = c("eflux", "vflux"),
                        prm_run = prm_run, bboxes = bboxes)

sink <- load_nc_physics(nc = nc_gen, select_physics = c("hdsource", "hdsink"),
                        prm_run = prm_run, bboxes = bboxes)

physics <- load_nc_physics(nc = nc_gen, 
                           select_physics = c("salt", "NO3", "NH3", "Temp", "Chl_a", "Denitrifiction"),
                           prm_run = prm_run, bboxes = bboxes)

# exclude sediment layer from salinity
physics <- filter(physics, !(variable == "salt" & layer == max(layer) & time == min(time)))
# exlucde water column from Denitrifiction
physics <- filter(physics, !(variable == "Denitrifiction" & layer != max(layer) & time == min(time)))

vol_dz <- load_nc_physics(nc = nc_gen, select_physics = c("volume", "dz"),
                          prm_run = prm_run, bboxes = bboxes)

dz <- dplyr::filter(vol_dz, variable == "dz")
vol <- dplyr::filter(vol_dz, variable == "volume")

nominal_dz <- load_init(init = init, vars = "nominal_dz") %>% 
  as.data.frame() %>% 
  dplyr::filter(!is.na(layer))

# Read in Dietcheck
df_dm <- load_dietcheck(dietcheck = dietcheck, fgs = fgs, prm_run = prm_run, convert_names = T)

# Read in SSB/R
ssb_rec <- load_rec(yoy = yoy, ssb = ssb, prm_biol = prm_biol)
# result$ssb_rec$rec.ssb=result$ssb_rec$rec/result$ssb_rec$ssb # recruits per spawner

# Read in misc  
df_agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs, group = get_age_acronyms(fgs), parameter = "age_mat")
dietmatrix <- load_dietmatrix(prm_biol, fgs, convert_names = TRUE)


# Calculate biomass spatially
bio_sp <- calculate_biomass_spatial(nums = dfs_gen[[1]], sn = dfs_gen[[2]], rn = dfs_gen[[3]], n = dfs_gen[[4]],
                                    vol_dz = vol_dz, bio_conv = bio_conv, bps = bps)

# Aggregate spatial biomass to based on stanzas
bio_sp_stanza <- combine_ages(bio_sp, grp_col = "species", agemat = df_agemat)
# Aggregate biomass
biomass <- bio_sp %>%
  agg_data(groups = c("species", "time"), fun = sum)

# biomass by box
bio_box=agg_data(bio_sp, groups= c('species', 'polygon', 'time'), fun=sum)

biomass_age <- bio_sp %>%
  filter(species %in% df_agemat$species) %>% #filter(agecl > 2) %>%
  agg_data(groups = c("species", "agecl", "time"), fun = sum)

# only vertebrates - convert to weight in grams, use for length at age plots
# biomass_age2 <- biomass_age #bio_sp %>%
  # filter(species %in% df_agemat$species) %>%
  # agg_data(groups = c("species", "agecl", "time"), fun = sum)

# Aggregate Numbers! This is done seperately since numbers need to be summed!
nums     <- agg_data(data = dfs_gen[[1]], groups = c("species", "time"), fun = sum)
nums_age <- agg_data(data = dfs_gen[[1]], groups = c("species", "agecl", "time"), fun = sum)
nums_box <- agg_data(data = dfs_gen[[1]], groups = c("species", "polygon", "time"), fun = sum)
RN_box = agg_data(data=dfs_gen[[3]], groups=c("species", "polygon","time"), fun=sum)
SN_box = agg_data(data=dfs_gen[[2]], groups=c("species", "polygon","time"), fun=sum)

### updated 20190404, use mean RN+SN per age for each species, convert to wgt, get length w/ Von Bert fn
RN_age = agg_data(data=dfs_gen[[3]], groups=c("species", "agecl","time"), fun=mean)
SN_age = agg_data(data=dfs_gen[[2]], groups=c("species", "agecl","time"), fun=mean)
RN_age=left_join(RN_age, SN_age, by=c('species', 'agecl', 'time'))
colnames(RN_age)=c('species', 'agecl', 'time', 'RN', 'SN')
bfile <- read.table(prm_biol,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)
pick <- grep("li_a_",bfile[,1])
xx <- bfile[pick,1:20]
tempmat <- matrix(NA,nrow=nrow(xx),ncol=3)
for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(as.character(xx[igroup,1]),"li_a_")[[1]][2]
tempmat[,2] <- as.numeric(as.character(xx[,2]))
pick <- grep("li_b_",bfile[,1])
tempmat[,3] <- as.numeric(as.character(bfile[pick,2]))
fgs2 <- load_fgs(fgs)
fgs2=fgs2[,c('Code', 'LongName')]
tempmat2=as.data.frame(tempmat[2:dim(tempmat)[1],]) #drop inverts
colnames(tempmat2)=c('Code', 'li_a', 'li_b')
tempmat3=left_join(tempmat2, fgs2, by='Code')
biomass_age2=left_join(RN_age, tempmat3[,2:4], by=c('species'='LongName'))
biomass_age2$grams_N_Ind=(biomass_age2$RN+biomass_age2$SN)*5.7*20/1000
biomass_age2$length_age=(as.numeric(as.character(biomass_age2$grams_N_Ind))/as.numeric(as.character(biomass_age2$li_a)))^(1/as.numeric(as.character(biomass_age2$li_b)))
length_age=biomass_age2[,c('species', 'agecl', 'time', 'length_age')]
colnames(length_age)[4]='atoutput'


# Aggregate the rest of the dataframes by mean!
structn_age <- agg_data(data = dfs_gen[[2]],  groups = c("species", "time", "agecl"), fun = mean)
resn_age    <- agg_data(data = dfs_gen[[3]],  groups = c("species", "time", "agecl"), fun = mean)
eat_age     <- agg_data(data = dfs_prod[[1]], groups = c("species", "time", "agecl"), fun = mean)
grazing     <- agg_data(data = dfs_prod[[2]], groups = c("species", "time"), fun = mean)
growth_age  <- agg_data(data = dfs_prod[[3]], groups = c("species", "time", "agecl"), fun = mean)
growth_PL  <- agg_data(data = dfs_noPred_prod[[1]], groups = c("species", "time"), fun = mean)
growth_PS  <- agg_data(data = dfs_noPred_prod[[2]], groups = c("species", "time"), fun = mean)
# growth_PB  <- agg_data(data = dfs_noPred_prod[[3]], groups = c("species", "time", "agecl"), fun = mean)

# Calculate consumed biomass - updated to avoid memory allocation error with short summary times in run file
safe_diet=purrr::possibly(calculate_consumed_biomass, otherwise = NA)
bio_cons <- safe_diet(eat = dfs_prod[[1]], grazing = dfs_prod[[2]], dm = df_dm, vol = vol, bio_conv = bio_conv)
if(!is.na(bio_cons)){
bio_cons=agg_data(bio_cons, groups = c("pred", "agecl", "time", "prey"), fun = sum)
}
## original call:
# bio_cons <- calculate_consumed_biomass(eat = dfs_prod[[1]], grazing = dfs_prod[[2]], dm = df_dm, vol = vol, bio_conv = bio_conv) %>% 
#   agg_data(groups = c("pred", "agecl", "time", "prey"), fun = sum)



#### see 'RM_preprocess_v2_workarounds.R' in dropbox if this does not work ###
## note - also did not work when output and time steps are not in sync in run file - 
# toutinc, toutfinc, tsumout must be same otherwise:
# 0% matching timesteps between PROD.nc and DietCheck.txt
# 100% data is lost due to missing eat data despite available diet data.

# Calculate spatial overlap
sp_overlap=NA
# sp_overlap <- calculate_spatial_overlap(biomass_spatial = bio_sp, dietmatrix = dietmatrix, agemat = df_agemat)

# Growth relative to initial conditions
rec_weight <- prm_to_df(prm_biol = prm_biol, fgs = fgs, 
                        group = get_age_acronyms(fgs = fgs), 
                        parameter = c("KWRR", "KWSR", "AgeClassSize"))

pd <- load_init_weight(init = init, fgs = fgs, bboxes = bboxes) %>%
  left_join(rec_weight) %>%
  split(.$species)
 

# Calculate weight difference from one ageclass to the next!
for (i in seq_along(pd)) {
  pd[[i]]$wdiff <- c((pd[[i]]$rn[1] + pd[[i]]$sn[1]) - (pd[[i]]$kwrr[1] + pd[[i]]$kwsr[1]),
                     diff(pd[[i]]$rn + pd[[i]]$sn))
}
pd <- do.call(rbind, pd)
pd$growth_req <- pd$wdiff / (365 * pd$ageclasssize)
if (any(pd$growth_req < 0)) {
  warning("Required growth negative for some groups. Please check your initial conditions files.")
  print(unique(pd$species[which(pd$growth_req<0)]))
}
unique(pd$species[which(pd$growth_req<0)])
gr_req <- pd %>%
  select(species, agecl, growth_req)

gr_rel_init <- growth_age %>%
  left_join(gr_req) %>%
  mutate(gr_rel = (atoutput - growth_req) / growth_req)

# specmort <- file.path(d2, paste(ncbase, 'SpecificPredMort.txt', sep=''))
# mort2=load_spec_mort(specmort, prm_run, fgs, convert_names = F,version_flag = 2)
# ggplot2::ggplot(subset(mort2, pred == "COD" & prey == "COD"), ggplot2::aes(x = factor(time), y = atoutput, fill = stanza)) +
#   ggplot2::geom_boxplot(position = "dodge") +
#   # ggplot2::geom_point()
#   ggplot2::facet_wrap(~prey, scale = "free")


# Aggregate volume vertically. bio_cons
vol_ts <- agg_data(vol, groups = c("time", "polygon"), fun = sum, out = "volume")
result <- list(
  "biomass"                = biomass,       #1
  "biomass_age"            = biomass_age,
  "biomass_consumed"       = bio_cons,
  "biomass_spatial_stanza" = bio_sp_stanza,
  "diet"                   = df_dm,         #5 
  "dz"                     = dz,
  "eat_age"                = eat_age,       
  "flux"                   = flux,
  "grazing"                = grazing,
  "growth_age"             = growth_age,    #10
  "growth_rel_init"        = gr_rel_init,
  "length_age"             = length_age,
  "nominal_dz"             = nominal_dz,
  "nums"                   = nums,
  "nums_age"               = nums_age,      #15
  "nums_box"               = nums_box,      
  "physics"                = physics,
  "resn_box"               = RN_box,
  "resn_age"               = resn_age,
  "strucn_box"             = SN_box,        #20
  "sink"                   = sink,
  "spatial_overlap"        = sp_overlap,    
  "ssb_rec"                = ssb_rec,       
  "structn_age"            = structn_age,    
  "vol"                    = vol_ts         #25
)
filename=sapply(strsplit(as.character(d2), "/"), tail, 1) # grab last chars of folder
save(result, file=paste(filename, '_prepro.rdata',sep=''))
save.image(paste(d2, '/ws.RData', sep='')) #"~/AtlRuns/20190111a/ws.RData")

# USE TO LOAD Result for other fn calls in atlantistools
# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# d <- loadRData("~/blah/ricardo.RData")
