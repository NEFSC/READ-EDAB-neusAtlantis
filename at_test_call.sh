#!/bin/bash
echo Running Atlantis
atlantisNew -i new_init.nc 0 -o atneus_v15_Test1_.nc -r at_run_neus_v15.prm -f at_force_neus_v15_DE_LINUX.prm -p at_physics_nuts_neus_v15_DE.prm -b at_biol_neus_v15_working.prm -h at_harvest_neus_v15_working.prm  -e at_economics_neus_DE_New.prm -s NeusGroups_v15_unix.csv -q NeusFisheries_v15.csv -d test1 1>1.junk 2>2.junk
