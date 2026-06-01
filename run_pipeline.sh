#!/bin/bash
zcat /atlantisdisk2/eof_targeting_3/eof_targeting_3_646/neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > /atlantisdisk2/eof_targeting_3/eof_targeting_3_646/neus_outputDetDiet_nz.gz
