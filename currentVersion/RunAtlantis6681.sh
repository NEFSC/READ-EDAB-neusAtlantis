#!/bin/bash
find /app/model -type f | xargs dos2unix
#6681
atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX.prm -p at_physics.prm -b at_biology.prm -m neus_migrations_6681.csv -h at_harvest.prm -e at_economics.prm -s neus_groups_6665.csv -q neus_fisheries.csv -t . -d output