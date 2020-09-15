# Utility functions to read and edit migration related parameters in biology.prm
# bio.file = here::here('currentVersion','at_biology.prm')
# groups.file = here::here('currentVersion','neus_groups.csv')
# spp.names = c('BFT','TUN')

#Retreives migration related lines as a list. Line numbers and line contents
get_migration_lines = function(bio.file){
   bio.lines = readLines(bio.file)
   mig.vars = c('flag.*Migrate','.*_Migrate_Time','.*_Migrate_Period','MigIOBox_','.*_Migrate_Return','.*_FSM','.*_ReturnStock')
   mig.lines.ls = list()
   mig.vals.ls = list()
   for(v in 1:length(mig.vars)){
     match = grep(mig.vars[v],bio.lines)  
     # print(length(match))
     mig.lines.ls[[v]] = match
     mig.vals.ls[[v]] = bio.lines[match]
   }
   return(list(mig.lines.ls = mig.lines.ls, mig.vals.ls = mig.vals.ls,mig.lines.all = bio.lines))
}

#Formats migration table object
get_param_migration = function(bio.file, groups.file, spp.names = NULL,write.output = F, out.dir = NA, out.prefix = NA){
  
   #Retreives migration parameter lines with get_migration_lines
   get.lines = get_migration_lines(bio.file)
   mig.line.nums = get.lines[[1]]
   mig.line.vals = get.lines[[2]]
   mig.lines = get.lines[[3]]
   
   #Retreive Functional Group Codes
   fgs = read.csv(groups.file,as.is = T)
   codes = fgs$Code
   
   if(is.null(spp.names)){
      group.lines = mig.line.vals[[1]][-grep('#',mig.line.vals[[1]])]
      codes =unname(sapply(group.lines,function(x)return(strsplit(x,'flag|Migrate| |  ')[[1]][2])))
      
   }else{
      spp.num = as.numeric(sapply(spp.names,function(x) return(grep(paste0('\\b',x,'\\b'),codes)))   )
      codes = codes[spp.num]
   }
   
   #Loop through each species, generating a Migration table
   mig.param.df.ls = list()
   mig.io.box.ls = list()
   
   for(i in 1:length(codes)){
      
      spp.i = codes[i]
      
      #flagXXXMig
      flag.mig = mig.line.vals[[1]]
      flag.mig.id = grep(paste0('flag',spp.i,'Migrate'),flag.mig)
      flag.mig.str = flag.mig[flag.mig.id]
      flag.mig.vals = strsplit(flag.mig.str,' |  ')[[1]][2]
      
      #XXX_Migrate_Time
      mig.time = mig.line.vals[[2]]
      mig.time.line.num = mig.line.nums[[2]]
      mig.time.id = grep(paste0('\\b',spp.i,'_Migrate_Time'),mig.time)
      if(length(mig.time.id)==0){
         mig.time.val = NA
      }else{
         mig.time.val = mig.lines[mig.time.line.num[mig.time.id]+1]   
      }
      
      #jXXX_Migrate_Time
      jmig.time.id = grep(paste0('j',spp.i,'_Migrate_Time'),mig.time)
      if(length(jmig.time.id)==0){
         jmig.time.val = NA
      }else{
         jmig.time.val = mig.lines[mig.time.line.num[jmig.time.id]+1]
      }
      
      #XXX_Migrate_Period
      mig.period = mig.line.vals[[3]]
      mig.period.line.num = mig.line.nums[[3]]
      mig.period.id = grep(paste0('\\b',spp.i,'_Migrate_Period'),mig.period)   
      if(length(mig.period.id) == 0){
         mig.period.val = NA
      }else{
         mig.period.val = mig.lines[mig.period.line.num[mig.period.id]+1]   
      }
      
      #jXXX_Migrate_Period
      jmig.period.id = grep(paste0('j',spp.i,'_Migrate_Period'),mig.period)   
      if(length(jmig.period.id)==0){
         jmig.period.val = NA
      }else{
         jmig.period.val = mig.lines[mig.period.line.num[jmig.period.id]+1]
      }
      
      #MigIOBox_XXXad
      mig.io.box = mig.line.vals[[4]]
      mig.io.box.line.num = mig.line.nums[[4]]
      mig.io.box.id = grep(paste0('MigIOBox_',spp.i,'ad'),mig.io.box)
      mig.io.box.val = mig.lines[mig.io.box.line.num[mig.io.box.id]+1]
      if(length(mig.io.box.val)==0){
         mig.io.box.val = rep(NA,30)
      }else{
         mig.io.box.val = strsplit(mig.io.box.val,' ')[[1]]   
      }
      
      #MigIOBox_XXXjuv
      mig.io.box.juv.id = grep(paste0('MigIOBox_',spp.i,'juv'),mig.io.box)
      if(length(mig.io.box.juv.id)==0){
         mig.io.box.juv.val = rep(NA,30)
      }else{
         mig.io.box.juv.val = mig.lines[mig.io.box.line.num[mig.io.box.juv.id]+1]
         mig.io.box.juv.val = strsplit(mig.io.box.juv.val,' ')[[1]]
      }
      
      #XXX_Migrate_Return
      mig.return = mig.line.vals[[5]]
      mig.return.line.num = mig.line.nums[[5]]
      mig.return.id = grep(paste0('\\b',spp.i,'_Migrate_Return'),mig.return)
      if(length(mig.return.id)==0){
         mig.return.val = NA
      }else{
         mig.return.val = mig.lines[mig.return.line.num[mig.return.id]+1]   
      }
      
      #jXXX_Migrate_Return
      mig.return.juv.id = grep(paste0('j',spp.i,'_Migrate_Return'),mig.return)
      if(length(mig.return.juv.id)==0){
         mig.return.juv.val = NA
      }else{
         mig.return.juv.val = mig.lines[mig.return.line.num[mig.return.juv.id]+1]
      }
      
      #XXX_FSM
      mig.fsm = mig.line.vals[[6]]
      mig.fsm.line.num = mig.line.nums[[6]]
      mig.fsm.id = grep(paste0('\\b',spp.i,'_FSM','\\b'),mig.fsm)
      if(length(mig.fsm.id) == 0){
         mig.fsm.val = NA
      }else{
         mig.fsm.val = mig.lines[mig.fsm.line.num[mig.fsm.id]+1]   
      }
      
      
      #jXXX_FSM
      mig.fsm.juv.id = grep(paste0('j',spp.i,'_FSM','\\b'),mig.fsm)
      if(length(mig.fsm.juv.id)==0){
         mig.fsm.juv.val = NA
      }else{
         mig.fsm.juv.val = mig.lines[mig.fsm.line.num[mig.fsm.juv.id]+1]
      }
      
      #XXX_FSMG
      mig.fsmg.id = grep(paste0('\\b',spp.i,'_FSMG','\\b'),mig.fsm)
      if(length(mig.fsmg.id)==0){
         mig.fsmg.val = NA
      }else{
         mig.fsmg.val = mig.lines[mig.fsm.line.num[mig.fsmg.id]+1]   
      }
      
      #jXXX_FSMG
      mig.fsmg.juv.id = grep(paste0('j',spp.i,'_FSMG','\\b'),mig.fsm)
      if(length(mig.fsmg.juv.id)==0){
         mig.fsmg.juv.val = NA
      }else{
         mig.fsmg.juv.val = mig.lines[mig.fsm.line.num[mig.fsmg.juv.id]+1]
      }
      
      #XXX_ReturnStock
      ret.stock = mig.line.vals[[7]]
      ret.stock.line.num = mig.line.nums[[7]]
      ret.stock.id = grep(paste0('\\b',spp.i,'_ReturnStock'),ret.stock)
      if(length(ret.stock.id) == 0){
         ret.stock.val = NA
      }else{
         ret.stock.val = mig.lines[ret.stock.line.num[ret.stock.id]+1]   
      }
      
      
      #jXXX_ReturnStock
      ret.stock.juv.id = grep(paste0('j',spp.i,'_ReturnStock'),ret.stock)
      if(length(ret.stock.juv.id) == 0){
         ret.stock.juv.val = NA
      }else{
         ret.stock.juv.val = mig.lines[ret.stock.line.num[ret.stock.juv.id]+1]
      }
      
      #Build Dataframe for main params
      mig.param.df.ls[[i]] = data.frame(spp.name = spp.i, 
                                        flag_X_Mig = flag.mig.vals,
                                        X_Mig_Time = mig.time.val,
                                        jX_Mig_Time = jmig.time.val,
                                        X_Mig_Period = mig.period.val,
                                        jX_Mig_Period = jmig.period.val,
                                        X_Mig_Return = mig.return.val,
                                        jX_Mig_Return = mig.return.juv.val,
                                        X_FSM = mig.fsm.val,
                                        jX_FSM = mig.fsm.juv.val,
                                        X_FSMG = mig.fsmg.val,
                                        jX_FSMG = mig.fsmg.juv.val,
                                        X_ReturnStock = ret.stock.val,
                                        jX_ReturnStock = ret.stock.juv.val)
      #Build Dataframe for MigIOBox params
      mig.io.box.names = c(spp.i,paste0('j',spp.i))
      mig.io.box.df.vals = rbind(mig.io.box.val,mig.io.box.juv.val)
      
      mig.io.box.ls[[i]] = data.frame(spp.name = mig.io.box.names,
                                      MigIOBox_X = mig.io.box.df.vals
                                      )
   }
   
   #Bind lists into dataframe
   mig.param.df = dplyr::bind_rows(mig.param.df.ls)
   mig.io.box.df = dplyr::bind_rows(mig.io.box.ls)
   
   #Package for return
   
   #Write output
   if(write.output){
      write.csv(mig.param.df,file = paste0(out.dir,out.prefix,'_Migration_Params.csv'), row.names = F)
      write.csv(mig.io.box.df, file = paste0(out.dir,out.prefix,'_Migration_IO_Box.csv'), row.names =F)
   } else{
      out.ls = list(Migration_Params = mig.param.df,MigIOBox = mig.io.box.df)
      return(out.ls)
   }
   
}

#Test function

##with specified spp names
 get_param_migration(bio.file = here::here('currentVersion','at_biology.prm'),
                    groups.file = here::here('currentVersion','neus_groups.csv'),
                    spp.names = c('BFT','TUN'),
                    write.output = T,
                    out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/',
                    out.prefix = 'Tuna_Test'
)
##With all groups
get_param_migration(bio.file = here::here('currentVersion','at_biology.prm'),
                               groups.file = here::here('currentVersion','neus_groups.csv'),
                               spp.names = NULL,
                               write.output = T,
                               out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/',
                               out.prefix = 'All_Groups'
)
