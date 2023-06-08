#!/bin/bash
cd /app/model
atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run.prm -f fish_sens_catch_spike_species_1/at_force_LINUX_fish_sens_catch_spike_species_1_YTF_0.prm -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output
