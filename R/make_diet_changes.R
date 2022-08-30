#Script to use the diet matrix change log to generate new pprey values.
library(dplyr)
source(here::here('R','edit_param_pprey.R'))

#Read in changelog data
#0 = 0
#1 = no change
#-1 = turn to 0
#2 = 1/3 of max PPREY
#3 = max PPREY
change.code = data.frame(change.flag = c(-1,2,3), change.scalar = c(0,1/3,1))

diet.change = read.csv(here::here('diagnostics','diet_changes','diet_changes_no_predation.csv'))

diet.change.long = reshape2::melt(diet.change, id.vars = c('PPREY.Max','code','Priority','pprey.name'), variable.name = 'prey', value.name = 'change.flag')%>%
  select(-code,-Priority)%>%
  filter(!(change.flag %in% c(0,1)))%>%
  left_join(change.code)%>%
  mutate(PPREY.new = signif(PPREY.Max * change.scalar,2))
  
diet.change.final = diet.change.long %>%
  select(pprey.name,prey,PPREY.new)

edit_param_pprey(
  atl.dir = here::here('currentVersion',''),
  biol.file = here::here('currentVersion','at_biology.prm'),
  fgs.file = here::here('currentVersion','neus_groups.csv'),
  pred.list = diet.change.final$pprey.name,
  prey.list = diet.change.final$prey,
  pprey.vals = diet.change.final$PPREY.new,
  overwrite = F,
  new.file.name = 'at_biology_nopredation.prm'
)
