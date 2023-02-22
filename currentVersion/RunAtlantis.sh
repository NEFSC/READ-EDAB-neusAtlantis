#!/bin/bash
find /app/model -type f | xargs dos2unix
#v6656
#atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f at_force_LINUX.prm -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output 
# v6645
#atlantisMerged -i neus_init_6645.nc 0 -o neus_output.nc -r at_run_6645.prm -f at_force_LINUX_6645.prm -p at_physics.prm -b at_biology_6645.prm -h at_harvest_6645.prm -e at_economics.prm -m neus_migrations.csv -s neus_groups_6645.csv -q neus_fisheries.csv -t . -d output 
# v6536 with 6645 prm files
#atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run_6645.prm -f at_force_LINUX_6645.prm -p at_physics.prm -b at_biology_6645.prm -h at_harvest_6645.prm -e at_economics.prm -s neus_groups_6645.csv -q neus_fisheries.csv -t . -d output 
# v6665 prm files
atlantisMerged -i neus_init_6645.nc 0 -o neus_output.nc -r at_run_6645.prm -f at_force_LINUX_6645.prm -p at_physics.prm -b at_biology_6665.prm -h at_harvest_6645.prm -e at_economics.prm -m neus_migrations.csv -s neus_groups_6665.csv -q neus_fisheries.csv -t . -d output 
