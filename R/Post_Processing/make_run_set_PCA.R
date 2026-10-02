library(ggplot2)
library(ggbiplot)
library(dplyr)

spp2guild = read.csv(here::here('diagnostics','functional_groups_match.csv'),stringsAsFactors = F)%>%
  select(Code,FuncGroup)

data = readRDS(here::here('diagnostics','run_set_test_data_raw.RDS'))%>%
  # filter(set.name == 'Run_Set_1')%>%
  left_join(spp2guild)%>%
  group_by(set.name,run.name,FuncGroup)%>%
  dplyr::summarise(Value = sum(Value,na.rm=T))

data.vals = data %>%
  tidyr::spread(FuncGroup,Value)%>%
  ungroup()
  # select(-set.name,-run.name)

run.names = data.vals$run.name
set.names = data.vals$set.name
data.vals = select(data.vals,-set.name,-run.name)  

runs.pca = prcomp(data.vals)

plot(runs.pca$x[,1],runs.pca$x[,2])

png(here::here('Figures','PCA_Run_Set_Test.png'),width = 8,height = 8, units = 'in', res = 300)
ggbiplot(runs.pca,groups = set.names,ellipse = T)
dev.off()
