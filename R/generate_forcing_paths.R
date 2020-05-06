# Script to generate the paths and formatting needed for atlantis force.prm file

force.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/ROMS_COBALT/Forcing_Files/Annual_Output/'
ts.dir = "\\tsfiles\\Annual_Files\\"
years = 1964:2014

flow.files = list.files(force.dir,pattern = '^flow.*\\.nc$',full.names = F)
temp.files = list.files(force.dir,pattern = '^temp.*\\.nc$',full.names = F)
salt.files = list.files(force.dir,pattern = '^salt.*\\.nc$',full.names = F)


flow.full = paste0('hd',(1:length(years))-1,'.name ',ts.dir,flow.files)
temp.full = paste0('Temperature',(1:length(years))-1,'.name ',ts.dir,temp.files)
salt.full = paste0('Salinity',(1:length(years))-1,'.name ',ts.dir,salt.files)

out.df = data.frame(flow.files = flow.full, temp.files = temp.full, salt.files = salt.full,ltl.files = ltl.full)

write.csv(out.df,file = paste0(force.dir,'force_file_names.csv'),row.names = F)

difftime(as.Date('1964-01-01'),as.Date('2014-12-12'),'days')
