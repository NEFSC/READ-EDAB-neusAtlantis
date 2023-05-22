library(dplyr)
library(ggplot2)
set.seed(13)
catch.orig = round(rnorm(20, mean = 100, sd = 10))
r = 1.1
start.bio = 1000
bio.orig = start.bio
for(i in 1:length(catch.orig)){bio.orig[i+1] = bio.orig[i]*r - catch.orig[i]}
t = 1:length(bio.orig)

catch.ls = list()
bio.ls = list()
out.df = list()

#number of iterations
N = 1000
#number of years held constant 

for(i in 1:N){
  
  set.seed(i)
  catch.new = sample(catch.orig,replace = F)
  bio.new = start.bio
  for(j in 1:length(catch.orig)){ bio.new[j+1] = bio.new[j]*r - catch.new[j]}
  bio.ls[[i]] = round(bio.new)
  catch.ls[[i]] = catch.new
  out.df[[i]] = data.frame(i =i, t = 1:length(bio.new),bio = bio.new,group = 'sim')
}

out.df = bind_rows(out.df)
out.df2 = bind_rows(out.df, data.frame(i = N+1, t = 1:length(bio.orig),bio = bio.orig, group = 'real' ))


ggplot(out.df2, aes(x=t,y=bio,color = group,group =i))+
  geom_line()

out.df.last = out.df %>%
  filter(t == max(t))
bio.lim = quantile(out.df.last$bio,c(0.05,0.95))
bio.range = range(out.df.last$bio)
orig.range = range(bio.orig)

# ggplot(out.df.last,aes(x=bio))+
#   geom_histogram()+
#   geom_vline(xintercept = bio.orig[length(bio.orig)])+
#   geom_vline(xintercept = bio.lim,color = c('red','blue'))

(bio.range[2] - bio.range[1])/(orig.range[2]-orig.range[1])


