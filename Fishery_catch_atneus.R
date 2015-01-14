#Fishery_catch_atneus.R
#Fishery catch history for Atlantis NEUS v1.5
#1/15
#SML

#User parameters
window <- T
if(window == T){
  data.dir   <- "C:\\Users\\Sean.Lucey\\Desktop\\Atlantis\\atneus\\tsfiles\\catches\\"
  data.dir.2 <- "C:\\Users\\Sean.Lucey\\Desktop\\Atlantis\\NEUS_v15\\"
  out.dir    <- "C:\\Users\\Sean.Lucey\\Desktop\\Atlantis\\atneus\\plots\\"
}
if(window == F){
  data.dir <- "slucey/EcoAP/Data/survey/"
  out.dir  <- "slucey/EcoAP/misc/"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table)

#-------------------------------------------------------------------------------
#User created functions
read.ts <- function(x, head = 244, col.names = NA){
  out <- as.data.table(read.table(x, sep = ' ', skip = head))
  out[, V2 := NULL]
  setnames(out, paste('V', c(1, 3:(ncol(out) + 1)), sep = ''), col.names)
  return(out)
}

B2N <- function(x){
  N2C   <- 114
  mg2mt <- 10e9
  sec   <- 86400 #seconds in a day
  out   <- x / ((N2C * sec) / mg2mt) 
  return(out)
} 

#-------------------------------------------------------------------------------
atneus1 <- c('Time', 'FPL', 'FPO', 'FPS', 'FVD', 'FVV', 'FVS',
             'FVB',  'FVT', 'FVO', 'FMM', 'FMN', 'FBP', 'FDD',
             'FDE',  'FDS', 'FDM', 'FDP', 'FDB', 'FDC', 'FDO',
             'FDF',  'SHB', 'SHD', 'SHC', 'SHP', 'SHR', 'SSK',
             'SB',   'SP',  'PIN', 'REP', 'WHB', 'WHS', 'WHT', 
             'WDG',  'CEP', 'BFS', 'BFF', 'BFD', 'BG',  'BMD',
             'BML',  'BMS', 'PWN', 'ZL',  'BD',  'MA')

#Merge all boxes together
nbox <- 22
all.boxes <- c()
for(i in 1:nbox){
  boxi <- read.ts(paste(data.dir, 'catch', i, '.ts', sep = ''), col.names = atneus1)
  boxi[, box := i]
  all.boxes <- rbindlist(list(all.boxes, boxi))
}

#Get proportional catch per box
all.sp.box <- c()
for(i in 2:length(atneus1)){
  species <- all.boxes[, list(box, Time, get(atneus1[i]))]
  sp.total <- data.table(Time = 1:max(species[, Time]))
  for(j in 1:nbox){
    sp.box <- species[box == j, ]
    setnames(sp.box, 'V3', paste('Box', j, sep = ''))
    sp.box[, box := NULL]
    sp.total <- merge(sp.total, sp.box, by = 'Time', all = T)
  }
  sp.total[, Atgroup := atneus1[i]]
  all.sp.box <- rbindlist(list(all.sp.box, sp.total))
}

setcolorder(all.sp.box, c('Atgroup', 'Time', paste('Box', 1:nbox, sep = '')))
setkey(all.sp.box, 'Atgroup', 'Time')

box.names <- paste('Box', 1:nbox, sep = '')
all.sp.box[, Totcatch := rowSums(.SD, na.rm = T), .SDcols = box.names]

for(i in 1:nbox){
  all.sp.box[, V1 := get(paste('Box', i, sep = '')) / Totcatch]
  all.sp.box[is.nan(V1), V1 := 0]
  setnames(all.sp.box, 'V1', paste('Box', i, '.prop', sep = ''))
}

neus1.prop <- all.sp.box[, c(box.names) := NULL]
neus1.prop[, Totcatch := NULL]

#Plot
box.names.prop <- paste(box.names, '.prop', sep = '')
#box.col <- colorRampPalette()
box.col <- terrain.colors(nbox)
for( i in 2:length(atneus1)){
  group <- neus1.prop[Atgroup == atneus1[i], ]
  jpeg(file = paste(out.dir, 'Fishing_history_', atneus1[i], '.jpg', sep = ''))
  plot(group[, Time], group[, Box1.prop], typ = 'n', xlab = 'Time', ylab = 'Proportion',
       main = atneus1[i], ylim = c(0, 1))
  for(j in 1:nbox){
    lines(group[, Time], group[, get(box.names.prop[j])], typ = 'l', col = box.col[j])
  }
  dev.off()
}

#determine periodicity of non full year fishing seasons
FPL <- neus1.prop[Atgroup == 'FPL', list(Time, Box1.prop)]
FPL[, DOY := Time %% 365]
plot(FPL[Box1.prop > 0, DOY])
FPL.season <- FPL[Box1.prop > 0, range(DOY)]
FPL.season <- FPL.season[2] - FPL.season[1]

FPS <- neus1.prop[Atgroup == 'FPS', list(Time, Box1.prop)]
FPS[, DOY := Time %% 365]
plot(FPS[Box1.prop > 0, DOY])
FPS.season <- FPS[Box1.prop > 0, range(DOY)]
FPS.season <- FPS.season[2] - FPS.season[1]

FVT <- neus1.prop[Atgroup == 'FVT', list(Time, Box1.prop)]
FVT[, DOY := Time %% 365]
plot(FVT[Box1.prop > 0, DOY])
FVT.season <- FVT[Box1.prop > 0, range(DOY)]
FVT.season <- FVT.season[2] - FVT.season[1]

SHB <- neus1.prop[Atgroup == 'SHB', list(Time, Box1.prop)]
SHB[, DOY := Time %% 365]
plot(SHB[Box1.prop > 0, DOY])
SHB.season <- SHB[Box1.prop > 0, range(DOY)]
SHB.season <- SHB.season[2] - SHB.season[1]


#Bring in new groups
neus15 <- as.data.table(read.csv(paste(data.dir.2, 'coderelations.csv', sep = '')))
neus15 <- neus15[, list(Child, Parent)]
setnames(neus15, c('Child', 'Parent'), c('Atgroup.new', 'Atgroup'))
neus15.prop <- merge(neus15, neus1.prop, by = 'Atgroup', all = T, allow.cartesian = T)
#Add years and remove old codes to facilitate merge with landings
neus15.prop[, Year := 1964 + floor(Time / 365)]
neus15.prop[, Atgroup := NULL]

#Read in landings
load(paste(data.dir.2, 'Landings_atcodes_1_5.RData', sep = ''))
#Sum landings across gears
landings <- c()
for(i in 2:ncol(atl.landings)){
  species.gear <- atl.landings[, c(1, i), with = F]
  species <- substr(names(atl.landings)[i], 1, 3)
  species.gear[, Atgroup.new := species]
  setnames(species.gear, names(atl.landings)[i], 'Landings')
  landings <- rbindlist(list(landings, species.gear))
}
setkey(landings, Atgroup.new, Year)
landings <- landings[, sum(Landings, na.rm = T), by = key(landings)]
setnames(landings, 'V1', 'Landings')
#Fix two letter group so it will map correctly
landings[Atgroup.new == 'SK_', Atgroup.new := 'SK']

#Map new codes to old codes
landings <- merge(landings, neus15, by = 'Atgroup.new', all = T)

#Switch to daily landings
landings[!Atgroup %in% c('FPL', 'FPS', 'FVT', 'SHB'), DLand := Landings / 365]
landings[Atgroup == 'FPL', DLand := Landings / FPL.season]
landings[Atgroup == 'FPS', DLand := Landings / FPS.season]
landings[Atgroup == 'FVT', DLand := Landings / FVT.season]
landings[Atgroup == 'SHB', DLand := Landings / SHB.season]

#Convert daily landings to mg N s^-1
landings[, Atland := B2N(DLand)]

#Merge landings with proportions
landings[, c('Atgroup', 'Landings', 'DLand') := NULL]
setkey(landings, Atgroup.new, Year)
neus15.prop <- merge(landings, neus15.prop, by = key(landings))

#Multiply proportions by landings
box <- as.data.table(matrix(rep(NA, nbox * nrow(neus15.prop)), 
                            nrow(neus15.prop), nbox))
setnames(box, paste('V', 1:nbox, sep = ''), paste('Box', 1:nbox, sep = ''))
neus15.prop <- cbind(neus15.prop, box)

for(i in 1:nbox){
  current.box  <- paste('Box', i, sep = '')
  current.prop <- paste('Box', i, '.prop', sep = '')
  setnames(neus15.prop, c(current.box, current.prop), c('V1', 'V2'))
  neus15.prop[, V1 := V2 * Atland]
  neus15.prop[, V2 := NULL]
  setnames(neus15.prop, 'V1', current.box)
}

#Clean up for output
neus15.prop[, c('Year', 'Atland') := NULL]
neus15.prop <- neus15.prop[!is.na(Time), ]

#Switch back to groups as columns and boxes as rows
atrows <- unique(neus15.prop[, Atgroup.new])
neus15.catch <- data.table(Time = 1:max(neus15.prop[, Time]), Box = 1)
for(i in 1:length(atrows)){
  spp.catch <- neus15.prop[Atgroup.new == atrows[i], ]
  spp.all.box <- c()
  for(j in 1:nbox){
    boxj <- paste('Box', j, sep = '')
    setnames(spp.catch, boxj, 'V1')
    spp.box <- spp.catch[, list(Time, V1)]
    setnames(spp.catch, 'V1', boxj)
    setnames(spp.box, 'V1', as.character(atrows[i]))
    spp.box[, Box := j]
    spp.all.box <- rbindlist(list(spp.all.box, spp.box))
  }
  setkey(spp.all.box, Time, Box)
  neus15.catch <- merge(neus15.catch, spp.all.box, by = key(spp.all.box), all = T)
}
