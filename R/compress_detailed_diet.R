#script to read in and compress detailed diet data
#Needed for large DetailedDietCheck.txt

atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/Obs_Hindcast_DetailedDiet2/'
diet.file = paste0(atl.dir,'neus_outputDetailedDietCheck.txt')

data = fread(diet.file)
data = data[1:2000,]

gc()

#Try reading 1000 rows
head = fread(diet.file,nrows = 1000)
test =sapply(1:1000,function(x) return(all(data[x,]==head[x,])))
sum(test)

head2 = fread(diet.file,skip = 1000,nrows = 1000)
test =sapply(1:1000,function(x) return(all(data[999+x,]==head2[x,])))

testcon <- file(diet.file,open="r")
readsizeof <- 20000
nooflines <- 0
( while((linesread <- length(readLines(testcon,readsizeof))) > 0 )
  nooflines <- nooflines+linesread )
close(testcon)
nooflines

test.dat = NA
ind = 1
inc = 100000
nrow = 1
while(nrow>0){
  dat = fread(diet.file,skip = inc*(ind-1), nrows = inc)
  nrow = nrow(dat)
  test.dat[ind] = nrow
  ind = ind+1
  
}
