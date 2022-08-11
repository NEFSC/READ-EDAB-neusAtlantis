


nums.csv <- read_csv(here("Atlantis_mv_1_6645_5yr_fprintfsp34_out1dy","outputFolder","Nums.csv"))

chc.nums <- nums.csv %>% 
  filter(code=="CHC")

write_csv(chc.nums, here("Atlantis_mv_1_6645_5yr_fprintfsp34_out1dy","outputFolder","CHC_nums.csv"))

chc.nums.plot <- chc.nums %>% 
  mutate(year_sim = rep(1:5,each = 365, times = 5),
         day_year = rep(1:365,each = 1, times = (5*5)),
         age = as.factor(age)) %>% 
  ggplot(aes(x= day_year, y = variable, colour = age)) +
  geom_line()+
  facet_wrap(year_sim ~ age, scales = "free_y") +
  labs(title="Hood Canal Chinook", subtitle = "Rows are simulation years,columns are age classes \n 
       Lines red = juvenile migration, green = age 5 return, blue = upriver migration, orange = spawning \n
       Migration  to ocean = 227-272, to Puget Sound = 151-196, upriver = 196-243; spawning = 152-198; Migiobox = 70%") +
  scale_y_continuous(limits = c(0,NA)) +
  geom_vline(xintercept = 227,  
             color = "coral4", size=0.7) +
  geom_vline(xintercept = 272,  
             color = "coral4",  size=0.7) +
   geom_vline(xintercept = 151,  
               color = "forestgreen",  size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 196,  
             color = "forestgreen",  size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 197,  
           color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 243,  
             color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 152,  
             color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 198,  
             color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)
  
ggsave(here("chc_daily_numbers.png"),chc.nums.plot, dpi= 300, width = 12, height = 10)

hep.nums <- nums.csv %>% 
  filter(code=="HEP")



hep.nums.plot <- hep.nums %>% 
  mutate(year_sim = rep(1:5,each = 365, times = 7),
         day_year = rep(1:365,each = 1, times = (7*5)),
         age = as.factor(age)) %>% 
  ggplot(aes(x= day_year, y = variable, colour = age)) +
  geom_line()+
  facet_wrap(year_sim ~ age, scales = "free_y", ncol = 7) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(title="Pacific herring", subtitle = "Rows are simulation years,columns are age classes \n 
       Lines red = migration out of Puget Sound, blue = return to Puget Sound, orange = spawning \n
       Migration to ocean = 182-197, to Puget Sound = 258-273; spawning = 15-105; Migiobox = 100%") +
  geom_vline(xintercept = 182,  
             color = "coral4", size=0.7) +
  geom_vline(xintercept = 197,  
             color = "coral4",  size=0.7) +
  geom_vline(xintercept = 258,  
             color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 273,  
             color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 15,  
             color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 105,  
             color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)

ggsave(here("hep_daily_numbers.png"), hep.nums.plot, dpi= 300, width = 14, height = 10)


csl.nums <- nums.csv %>% 
  filter(code=="CSL")

csl.nums.plot <- csl.nums %>% 
  mutate(year_sim = rep(1:5,each = 365, times = 10),
         day_year = rep(1:365,each = 1, times = (10*5)),
         age = as.factor(age)) %>% 
  ggplot(aes(x= day_year, y = variable, colour = age)) +
  geom_line()+
  facet_wrap(year_sim ~ age, scales = "free_y", ncol = 10) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(title="California sea lion", subtitle = "Rows are simulation years,columns are age classes \n 
       Lines red = migration out of Puget Sound, blue = return to Puget Sound, orange = spawning \n
       Migration to ocean = 69-99, to Puget Sound = 244-274; spawning = 210-330; Migiobox = 95%") +
  geom_vline(xintercept = 69,  
             color = "coral4", size=0.7) +
  geom_vline(xintercept = 99,  
             color = "coral4",  size=0.7) +
  geom_vline(xintercept = 244,  
             color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 274,  
             color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 210,  
             color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 330,  
             color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)

ggsave(here("csl_daily_numbers.png"), csl.nums.plot, dpi= 300, width = 14, height = 10)


pin.nums <- nums.csv %>% 
  filter(code=="PIN")

pin.nums.plot <- pin.nums %>% 
  mutate(year_sim = rep(1:5,each = 365, times = 10),
         day_year = rep(1:365,each = 1, times = (10*5)),
         age = as.factor(age)) %>% 
  ggplot(aes(x= day_year, y = variable, colour = age)) +
  geom_line()+
  facet_wrap(year_sim ~ age, scales = "free_y", ncol = 10) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(title="Steller sea lion", subtitle = "Rows are simulation years,columns are age classes \n 
       Lines red = migration out of Puget Sound, blue = return to Puget Sound, orange = spawning \n
       Migration to ocean = 69-99, to Puget Sound = 244-274; spawning = 153-173; Migiobox = 65%") +
  geom_vline(xintercept = 69,  
             color = "coral4", size=0.7) +
  geom_vline(xintercept = 99,  
             color = "coral4",  size=0.7) +
  geom_vline(xintercept = 244,  
             color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 274,  
             color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 153,  
             color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 173,  
             color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)

ggsave(here("pin_daily_numbers.png"), pin.nums.plot, dpi= 300, width = 14, height = 10)


be.nums <- nums.csv %>% 
  filter(code=="BE")

be.nums.plot <- be.nums %>% 
  mutate(year_sim = rep(1:5,each = 365, times = 10),
         day_year = rep(1:365,each = 1, times = (10*5)),
         age = as.factor(age)) %>% 
  ggplot(aes(x= day_year, y = variable, colour = age)) +
  geom_line()+
  facet_wrap(year_sim ~ age, scales = "free_y", ncol = 10) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(title="Raptors", subtitle = "Rows are simulation years,columns are age classes \n 
       Lines red = migration out of Puget Sound, blue = return to Puget Sound, orange = spawning \n
       Migration to ocean = 206-213 & 30-90, to Puget Sound = 251-257 & 305-365; spawning = 72-79; Migiobox = 29% & 71%, Recruit = 112") +
  geom_vline(xintercept = 206,  
             color = "coral4", size=0.7) +
  geom_vline(xintercept = 213,  
             color = "coral4",  size=0.7) +
  geom_vline(xintercept = 30,  
             color = "coral4", size=0.7) +
  geom_vline(xintercept = 90,  
             color = "coral4",  size=0.7) +
  geom_vline(xintercept = 251,  
             color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 257,  
             color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 305,  
             color = "dodgerblue3",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 365,  
             color = "dodgerblue3", linetype = "dashed", size=0.7, alpha = 0.5) +
  
  geom_vline(xintercept = 72,  
             color = "darkorange2",  linetype = "dashed", size=0.7, alpha = 0.5) +
  geom_vline(xintercept = 79,  
             color = "darkorange2", linetype = "dashed", size=0.7, alpha = 0.5)

ggsave(here("be_daily_numbers.png"), be.nums.plot, dpi= 300, width = 14, height = 10)


migration.array <- read_delim(here("Atlantis_mv_1_6645_5yr_fprintfsp34_out1dy","outputFolder","AMPS_OUTMigrationArray.txt"))

hep.nums.mig <- migration.array %>% 
  filter(Species=="HEP") %>% 
  filter(DEN!=0)


chc.nums.plot <- hep.nums.mig %>% 
  mutate(Time = as.factor(Time)) %>% 
  filter(Cohort ==0)

chs.nums.mig <- migration.array %>% 
  filter(Species=="CHC") %>% 
  filter(DEN!=0)


chc.nums.plot <- chs.nums.mig %>% 
  mutate(Time = as.factor(Time)) %>% 
  filter(Cohort ==0)
  
# mutate(year_sim = rep(1:5,each = 365, times = 5),
 #        day_year = rep(1:365,each = 1, times = (5*5)),
         mutate(Time = as.factor(Time)) %>% 
  ggplot(aes(x= Time, y = DEN, colour = Cohort)) +
  geom_line()+
  facet_wrap(. ~ Cohort, scales = "free_y") +
  
  mutate(year_sim = rep(1:5,each = 365, times = 5*28),
                               day_year = rep(1:365,each = 1, times = (5*5*28)),
                               age = as.factor(Cohort))

write_csv(chs.nums.mig, here("Atlantis_mv_1_6645_5yr_fprintfsp34_out1dy","outputFolder","CHC_mig_array.csv"))
