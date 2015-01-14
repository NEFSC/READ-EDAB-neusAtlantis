#!/bin/bash
echo Running Atlantis
atlantisNew -i inneus_2012.nc 0 -o neusDynEffort_Test1_.nc -r at_run_neus_v15_DE.prm -f at_force_neus_v15_DE_LINUX.prm -p at_physics_nuts_neus_v15_DE.prm -b at_biol_neus_v15_DE.prm -h at_harvest_neus_v15_DE.prm  -e at_economics_neus_DE_New.prm -s NeusGroups.csv -q NeusFisheries.csv
