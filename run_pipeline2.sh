#!/bin/bash
sudo zcat /atlantisdisk2/eof_targeting_3/eof_targeting_3_646/neus_outputDetailedDietCheck.txt.gz | head -n1 | sudo gzip > /atlantisdisk2/eof_targeting_3/eof_targeting_3_646/neus_outputDetDietHead.gz 2>/dev/null
