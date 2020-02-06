
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neus-atlantis

<!-- badges: start -->

<!-- badges: end -->

Atlantis Neus file repository v1.5

Physics based on Doppio Roms model hindcast 2008-2010 downscaled to NEUS
boxes.

### The spatial domain

![](https://raw.githubusercontent.com/NOAA-EDAB/neus-atlantis/master/Geometry/AtlantisNeusDomain.PNG)<!-- -->

### 2019 run structure

Now includes fishing, last updated 20190917

  - Windows build: atlantismain\_6456 -i RMinit4\_2019.nc 0 -o
    atneus\_v15\_test2008hydro\_20180208.nc -r
    at\_run\_neus\_v15\_RM\_scale\_0503.prm -f
    at\_force\_neus\_v15\_DE\_WIN\_RM\_newHydro\_highnuts.prm -p
    at\_physics\_nuts\_neus\_v15\_DE\_eddys.prm -b
    at\_biol\_neus\_v15\_scaled\_diet\_20181126\_2.prm -h
    at\_harvest\_neus\_v15\_FC\_RG\_5\_16\_19.prm -e
    at\_economics\_neus\_DE\_New.prm -s NeusGroups\_v15\_unix\_RM.csv -q
    NeusFisheries\_v15\_5\_17\_19.csv -t C:.morse\_RM -d 20190916dta
    1\>1a.junk 2\>2a.junk

  - Linux build: atlantisMerged -i RMinit4\_2019.nc 0 -o
    atneus\_v15\_test2008hydro\_20180208.nc -r
    at\_run\_neus\_v15\_RM\_scale\_0503.prm -f
    at\_force\_neus\_v15\_DE\_LINUX\_RM\_newHydro\_highnuts.prm -p
    at\_physics\_nuts\_neus\_v15\_DE\_eddys.prm -b
    at\_biol\_neus\_v15\_scaled\_diet\_20181126\_2.prm -h
    at\_harvest\_neus\_v15\_FC\_RG\_5\_16\_19.prm -e
    at\_economics\_neus\_DE\_New.prm -s NeusGroups\_v15\_unix\_RM.csv -q
    NeusFisheries\_v15\_5\_17\_19.csv -t /home/ryan/Git/atneus\_RM -d
    20190626a 1\>1a.junk 2\>2a.junk

t: location of local git folder

*This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.*
