#Compares fleet revenue between multiple runs
library(dplyr)
library(ggplot2)
source(here::here('R','Post_Processing','make_catch_revenue.R'))

run.names = c('SCA_port_scale_1', 'dev_07152025')
run.long.names = c('75% New Bedford','45% New Bedford')
run.dirs = here::here('Atlantis_Runs',run.names,'/')
out.dirs = paste0(run.dirs,'Post_Processing/data')
price.reference.orig = read.csv(here::here('data-raw','species_price_reference.csv'))
price.reference = price.reference.orig %>% 
  dplyr::group_by(Code, Year) %>% 
  dplyr::summarise(Price = mean(Price,na.rm=T))

revenue.ls = list()

for(i in 1:length(run.names)){
  
  revenue.ls[[i]] = make_catch_revenue(run.name = run.names[i],
                                    run.dir = run.dirs[i],
                                    price.reference = price.reference,
                                    write =F,
                                    plot = F
                                    ) %>% 
    dplyr::mutate(run.name= run.names[i],
                  run.long.name = run.long.names[i])
  
}

revenue.df = dplyr::bind_rows(revenue.ls) %>% 
  filter(grepl('^SCA',Fishery))%>%
  tidyr::separate(Fishery, c('dum','Port'), sep = 'SCA', remove =F) %>% 
  mutate(revenue.million = Revenue*1E-6)

ggplot(revenue.df, aes( x= Year, y = revenue.million, color = run.long.name))+
  geom_line()+
  facet_wrap(~Port)+
  theme_bw()+
  scale_color_manual(name = 'Scenario', values = c('red3','navy'))+
  ylab('Revenue (Millions USD)')+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures','SCA_NewBedford_Consolidation_Revenue_Fleet.png'),width = 12, height = 12, units = 'in', dpi = 300)

revenue.total = revenue.df %>%
  group_by(Year, run.long.name) %>% 
  summarise(revenue.million = sum(Revenue)*1E-6)

ggplot(revenue.total, aes(x = Year, y = revenue.million, fill = run.long.name))+
  geom_bar(stat = 'identity',position = 'dodge')+
  theme_bw()+
  scale_fill_manual(name = 'Scenario', values = c('red3','blue2'))+
  ylab('Revenue (Millions USD)')+
  theme(legend.position = 'bottom')
ggsave(here::here('Figures','SCA_NewBedford_Consolidation_Revenue_Total.png'),width = 12, height = 6, units = 'in', dpi = 300)
