# atneus
Atlantis Neus file repository v1.5

Physics based on Doppio Roms model hindcast 2008-2010 downscaled to NEUS boxes.

2019 run structure 

Now includes fishing, last updated 20190917

- Windows build:
atlantismain_6456 -i RMinit4_2019.nc 0 -o atneus_v15_test2008hydro_20180208.nc -r at_run_neus_v15_RM_scale_0503.prm -f at_force_neus_v15_DE_WIN_RM_newHydro_highnuts.prm -p at_physics_nuts_neus_v15_DE_eddys.prm -b at_biol_neus_v15_scaled_diet_20181126_2.prm -h at_harvest_neus_v15_FC_RG_5_16_19.prm -e at_economics_neus_DE_New.prm -s NeusGroups_v15_unix_RM.csv -q NeusFisheries_v15_5_17_19.csv -t C:\Users\ryan.morse\Documents\GitHub\atneus_RM -d 20190916dta 1>1a.junk 2>2a.junk

- Linux build:
atlantisMerged -i RMinit4_2019.nc 0 -o atneus_v15_test2008hydro_20180208.nc -r at_run_neus_v15_RM_scale_0503.prm -f at_force_neus_v15_DE_LINUX_RM_newHydro_highnuts.prm -p at_physics_nuts_neus_v15_DE_eddys.prm -b at_biol_neus_v15_scaled_diet_20181126_2.prm -h at_harvest_neus_v15_FC_RG_5_16_19.prm -e at_economics_neus_DE_New.prm -s NeusGroups_v15_unix_RM.csv -q NeusFisheries_v15_5_17_19.csv -t /home/ryan/Git/atneus_RM -d 20190626a  1>1a.junk 2>2a.junk



t: location of local git folder
