init = ncdf4::nc_open('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_init.nc')

spp = 'Herring'
code = 'HER'
varnames = paste0(spp,1:10,'_Nums')

bio.file = here::here('currentVersion','at_biology.prm')
biolines = readLines(bio.file)
spp.spat.ad = paste0('F',code,'_S',1,'\\b')
spp.spat.juv =paste0('F',code,'_S',1,'juv\\b')

#age at maturity
mat.age = as.numeric(strsplit(biolines[grep(paste0(code,'_age_mat'),biolines)],'\t| ')[[1]][2])+1

#get initial scalar
source(here::here('R','edit_param_init_scalar.R'))
init.scalar = get_param_init_scalar(run.prm = here::here('currentVersion','at_run.prm'),
                                    groups.file = here::here('currentVersion','neus_groups.csv'),
                                    write.output = F)
spp.scalar = as.numeric(as.character(init.scalar[which(init.scalar$group == code),2]))

out.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Diagnostic_Data/Init/'

for(i in 1:length(varnames)){
  
  x = ncvar_get(init,varnames[i])[1,]
  # x.tot = sum(x,na.rm=T)
  # x.prop = x/x.tot
  # 
  # new.tot = x.tot * spp.scalar
  # 
  # if(i < mat.age){
  #   line.match = grep(spp.spat.juv,biolines)+1
  #     
  #   line.string = biolines[line.match]
  #   line.split = as.numeric(strsplit(line.string,'\t| ')[[1]])
  # }  

  new.x = x*spp.scalar
  new.x = paste0(new.x,',')
  new.x[which(new.x == 'NA,')] = '_,'
  
  out.mat = matrix('_,',nrow = 30, ncol = 5)
  out.mat[,1] = new.x
  out.mat[30,5] = '_ ;'
  
  write.table(out.mat,file = paste0(out.dir,varnames[i],'.txt'),quote = F, row.names = F)

}

curve( (13.4-127*(x+1))/-0.75 - 463, 0, 2.65)
