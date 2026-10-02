#Template for creating setupfile 

set1 = data.frame(Run.ID = NA,
                  Group = 'Set1',
                  Code = 'SB',
                  Type = 'mQa',
                  Unit = 'scalar',
                  Value = seq(1,1E6,length.out = 20))


set2 = data.frame(Run.ID = NA,
                  Group = 'Set2',
                  Code = 'SB',
                  Type = 'KDENR',
                  Unit = 'scalar',
                  Value = seq(0.001,1,length.out = 20))

set3 = data.frame(Run.ID = NA,
                  Group = 'Set3',
                  Code = 'SB',
                  Type = 'mLa',
                  Unit = 'scalar',
                  Value = seq(1,1E6,length.out = 20))

out.df = dplyr::bind_rows(set1,set2,set3)

out.df$Run.ID = 1:nrow(out.df)

write.csv(out.df,here::here('Setup_Files','cloud_SB_2.csv'),row.names = F)
