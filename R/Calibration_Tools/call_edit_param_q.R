source(here::here('R','Calibration_Tools','edit_param_q.R'))

q.corr = read.csv(here::here('Setup_Files','sca_fleets_revised_3_q_corr.csv'))

for(i in 1:nrow(q.corr)){
  
  edit_param_q(harvest.file = here::here('currentVersion','at_harvest.prm'),
               Code = q.corr$Code[i],
               Fleet = q.corr$Fleet[i],
               fleets.file = here::here('currentVersion','neus_fisheries.csv'),
               Type= 'value',
               Value =q.corr$Value[i] ,
               overwrite = T
  )
}
