



outFile <- here::here("temp.txt")
con <- file(outFile,open="a")
boxes <- c(0:29)


bgmfile <- here::here("currentVersion/neus_tmerc_RM2.bgm")
bgmdata <- atlantistools::load_box(bgmfile)

for (ibox in boxes) {
  writeLines(paste0("Effortts",ibox,".name boxEffort",ibox),con)
  writeLines(paste0("Effortts",ibox,".location ",paste(bgmdata$boxes[[as.character(ibox)]]$inside,collapse = " ")," ",ibox),con)
  writeLines(paste0("Effortts",ibox,".data CatchFiles/effort_box",ibox,".ts"),con)
  writeLines(paste0("Effortts",ibox,".rewind 0"),con)
  writeLines(" ",con)
}
# Effortts0.name boxEffort0
# Effortts0.location 536357.5 2472123.3 0
# Effortts0.data CatchFiles/effort_box0.ts
# Effortts0.rewind 0

close(con)