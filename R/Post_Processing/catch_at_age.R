atl.dir = 'C:/Users/joseph.caracappa/Documents/Atlantis/Obs_Hindcast/Atlantis_Runs/'

run.name = 'HER_Fix_1e'

catch.file = paste0(atl.dir,run.name,'/neus_outputCATCH.nc')

library(ncdf4)
library(ggplot2)

catch.nc = nc_open(catch.file)

group = 'HER'
group.varname = 'Herring'

catch.names = paste0(group.varname,1:10,'_Catch')
dis.names = paste0(group.varname,1:10,'_Discards')

time.raw = catch.nc$dim$t$vals
time.date = as.Date(as.POSIXct(time.raw, origin = '1964-01-01 00:00:00', tz = 'UTC'))

out.ls = list()

for(i in 1:length(var.names)){
  
  catch.age = ncvar_get(catch.nc,catch.names[i])
  catch.age = apply(catch.age,2,sum,na.rm=T)
  
  discard.age = ncvar_get(catch.nc,dis.names[i])
  discard.age = apply(discard.age,2,sum,na.rm=T)
  
  out.ls[[i]] = data.frame(
    Time = time.date,
    group = group,
    age = i,
    catch = catch.age,
    discard = discard.age
  )
}

out.df = dplyr::bind_rows(out.ls)

fig.dir = paste0(atl.dir,run.name,'/Post_Processed/')
out.df2 = dplyr::filter(out.df, Time < as.Date('1975-01-01'))
ggplot(out.df2, aes(x= Time, y = catch, fill = as.factor(age)))+
  geom_bar(position = 'fill',stat = 'identity')

ggplot(out.df2, aes(x= Time, y = discard, fill = as.factor(age)))+
  geom_bar(position = 'fill',stat = 'identity')

  ggsave(paste0(fig.dir,run.name,'_catch_at_age.png'),width = 12, height = 8, units = 'in', dpi= 300)
  

