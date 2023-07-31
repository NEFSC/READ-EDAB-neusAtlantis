#!/bin/bash
find /app/model -type f | xargs dos2unix
atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX.prm -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output
#atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX_climate_100yr.prm -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output