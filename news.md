# neus-atlantis 1.0.0-9005 (in dev) 
(built with altantis rev 6490)

Major fixes

* Fixes incorrect conversion from COBALT (mol X/ kg H20) to Atlantis (mg X/m3) units
* Updated LTL-Forcing scripts
* Removed initial conditions scaling of forced LTL groups


As described in [#30]( https://github.com/NOAA-EDAB/neus-atlantis/pull/30)

# neus-atlantis 1.0.0-9004 (in dev) 
(built with altantis rev 6490)

Tested the run with the COBALT forcing for lower trophic levels
* Herring persisted at full fishing levels - but still drop after the 50th time step, not to as great a magnitude as previously
Increased herring recruit numbers
* Mackerel persist and probably need to have biological parameters reduced a bit to constrain growth

As described in [#31]( https://github.com/NOAA-EDAB/neus-atlantis/pull/31)

# neus-atlantis 1.0.0-9003 (in dev) 
(built with altantis rev 6490)

* Fixed a recruitment issue with Herring related to a bug in the code if it happens too late in the year

    + This is a fudge, and needs to be reverted once the bug is fixed in Atlantis
    
* Fixed the lower trophic levels that herring and mackerel rely on, to persist at levels at which starvation isn't an issue

    + Increased the initial biomasses of phytoplankton and zooplankton groups
    + Tweaked the pprey values between the lower trophic levels to keep species from being preyed upon enough to crash
    
* Increased initial biomass values for Mackerel and Herring
* Increased recruits per year for Mackerel and Herring
* Increased assimilation efficiency for Mackerel and Herring

    + Mackerel persists at resonable levels
    + Herring still collapses at time step 51 with normal fishing
    
* Changed the age structure of the catch for Mackerel and Herring to prevent overfishing at lower age classes
* Affects: at_biology_test.prm, at_run_test.prm, at_harvest_test.prm
 
As described in ([#28](https://github.com/NOAA-EDAB/neus-atlantis/pull/28))

# neus-atlantis 1.0.0-9002 (in dev) 
(built with altantis rev 6490)

Incorporates new LTL forcing, associated parameter file changes, and additional R scripts to process and interpret forcing data ([#26](https://github.com/NOAA-EDAB/neus-atlantis/pull/26))


# neus-atlantis 1.0.0-9001 (in dev)
(built with altantis rev 6490)

Dynamic and forced models diverge at this point. ROMS_COBALT-Forcing ([#25](https://github.com/NOAA-EDAB/neus-atlantis/pull/25))

## Breaking changes (major)




## New Features (minor)

Addition of new ROM_COBALT forcing for physics only. Includes annual forcing from 1964-2014.

## Bug Fixes (patch)

### Changes to: at_biology.prm, at_run_test.prm, at_harvest_test.prm 
* Reduces magnitude of Diatom (PL) bloom and stabilizes closer to initial biomass
    + Reduce mum_PL
    + Increase predation on PL by Meiobenthos (BO) in sediment (reduces sediment accumulation)
    + Decrease cannibalism of BO
* Slight increase in longevity of scallops (SCA) and Lobseter (LOB)
    + Decreased cannibalism in LOB (and other benthic carnivores)
    + Increase grazing of benthic filter feeders (CLA, SCA, QHG, BFF) on PL and decrease consumption of refractory detritus (DR)
* As described in ([#28](https://github.com/NOAA-EDAB/neus-atlantis/pull/28))

# neus-atlantis 1.0
built with atlantis 

Not released. Depreciated
