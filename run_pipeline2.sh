#!/bin/bash
sudo zcat /atlantisoutput/eof_targeting_1/eof_targeting_1_593/neus_outputDetailedDietCheck.txt.gz | head -n1 | sudo gzip > /atlantisoutput/eof_targeting_1/eof_targeting_1_593/neus_outputDetDietHead.gz 2>/dev/null
