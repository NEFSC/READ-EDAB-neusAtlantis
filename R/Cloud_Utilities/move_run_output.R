## Utilities for moving around data

## Moving run output from managed disk to bucket

# sudo mv /atlantisdisk2/eof_targeting_4 /atlantisarchive/Joseph.Caracappa

## Synching Data between disk and bucket

# rsync -rlpt --no-perms --no-owner --no-group /atlantisdisk2/eof_targeting_4/ /atlantisarchive/Joseph.Caracappa/eof_targeting_4/
# rsync -rlptv --size-only --no-perms --no-owner --no-group /atlantisdisksmall/catch_thresholds_eof_uniform_standard_2/ /atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_uniform_standard_2/

library(dplyr)

#_________________________________
### Checking Post Processed Output

## Moving Setup File to output
experiment.id = 'eof_targeting_4'
disk.dir = file.path('/atlantisdisk2',experiment.id)
bucket.dir = file.path('/atlantisarchive','Joseph.Caracappa',experiment.id)
export.dir = file.path('/atlantisarchive','export',experiment.id)

#move setup file
system(paste0('sudo cp ',here::here('Setup_Files',paste0(experiment.id,'_setup.csv')),' /atlantisarchive/Joseph.Caracappa/'))

## check that all file are present in destination folder
nfiles = function(x){
  files = list.files(x)
  return(length(files))
}

disk.dirs = list.files(disk.dir,include.dirs = T, full.names =T)
bucket.dirs = list.files(bucket.dir,include.dirs = T,full.names = T)
export.dirs = list.files(export.dir,include.dirs = T,full.name =T)

#Populate tracking for transfer data
new.runs = data.frame(run.name = basename(bucket.dirs),
                      nfiles.new= sapply(paste0(bucket.dirs,'/data/'),nfiles))



#copy essential files to /atlantisarchive/export
# Define expected files to be present in each run
if(!dir.exists(export.dir)){dir.create(export.dir)}
disk.names = basename(disk.dirs)
files2move = c('biomass.rds',
               'biomass_age.rds',
               'biomass_box.rds',
               'biomass_box_invert.rds',
               'catch.rds',
               'eco_indicators_mean.rds',
               'eco_indicators_ts.rds',
               'length_age.rds',
               'neus_outputDetDiet_processed.gz',
               'numbers_age.rds',
               'ppc.rds')

system(paste0('sudo chmod 777 -R ',export.dir))
system(paste0('sudo chmod 777 -R ',bucket.dir))

library(dplyr)
for(i in 1:length(disk.names)){
  source.dir = file.path(bucket.dir,disk.names[i])
  dest.dir = file.path(export.dir,disk.names[i])
  if(!dir.exists(new.dir)){dir.create(new.dir)}
  files.from = paste0(source.dir,'/data/',files2move)
  
  if(!all(file.exists(files.from))){
    message('Missing files in: ',disk.names[i])
    # next()
  }
  file.check = data.frame(file.name = files2move) %>% 
      dplyr::group_by(file.name) %>% 
      dplyr::mutate(base.name = list.files(source.dir,pattern = file.name,recursive = T, full.names = T)[1],
                    export.name = file.path(new.dir,basename(base.name)),
                    in.base = file.exists(base.name),
                    in.export = file.exists(export.name),
                    base.size = file.size(base.name),
                    export.size = file.size(export.name),
                    tomove = ifelse(in.base ==T & (in.export == F | base.size != export.size), T, F)
      ) 
  
  file.check.move = 
    file.check %>% 
    dplyr::filter(tomove == T)
    
    
    
    if(nrow(file.check.move) > 0){
      message(paste0('Moving ',nrow(file.check.move),' file to ',disk.names[i]))
      # file.copy(file.check$base.name,file.check$export.name,overwrite = T)  
      copy_commands <- paste(
        "sudo cp -f", 
        shQuote(file.check.move$base.name), 
        shQuote(file.check.move$export.name)
      )
      lapply(copy_commands,system)
    }
    
  
  int = floor(((i/length(disk.names))*100 ))%% 10
  if( int == 0){print(int)}
}

##### Checks if the number of files in the source location is expected
new.runs = data.frame(run.name = basename(export.dirs),
                      nfiles.new= sapply(export.dirs,nfiles)) %>% 
  dplyr::arrange(run.name) %>% 
  dplyr::mutate(correct.n = ifelse(nfiles.new == length(files2move),T,F)) %>% 
  dplyr::filter(correct.n == F)


#Checks if files for export matche expected
for(i in 1:length(export.dirs)){
  this.export.dir = basename(export.dirs[i])
  this.base.dir = bucket.dirs[which(basename(bucket.dirs) == this.export.dir)]
  
  export.files = list.files(export.dirs[i], full.name = T,recursive = T)
  base.files =  list.files(this.base.dir, full.name = T,recursive = T)
  base.files = base.files[which(basename(base.files) %in% basename(export.files))]
  base.files = base.files[match(basename(export.files),basename(base.files))]
  
  export.size = file.size(export.files)
  base.size = file.size(base.files)
  
  length.match = length(base.files) == length(export.files)
  size.match = which(abs(export.size-base.size) > 0)
  
  if(length(export.size)>0 & any(export.size==0)){
    zero.files = basename(export.files)[export.size ==0]
    message(paste0('Zero size files in: ',this.export.dir,' ',paste(zero.files, collapse = ', ' )))
  }
  
  if(length.match == F){
    message(paste0('Number of files different in run', this.export.dir))
  }
  
  if(length(size.match)>0){
    message(paste0('Files different sizes in run ', this.export.dir,': ',paste(basename(export.files[size.match]), collapse = ', ')))
  }
}
