#!/bin/bash
zcat /atlantisoutput/eof_targeting_1/eof_targeting_1_593/neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > /atlantisoutput/eof_targeting_1/eof_targeting_1_593/neus_outputDetDiet_nz.gz
