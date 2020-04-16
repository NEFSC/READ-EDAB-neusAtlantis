

# neus-atlantis 1.5.0-9003 (in dev) 
(built with altantis rev 6490)


# neus-atlantis 1.5.0-9002 (in dev) 
(built with altantis rev 6490)

Incorporates new LTL forcing, associated parameter file changes, and additional R scripts to process and interpret forcing data ([#26](https://github.com/NOAA-EDAB/neus-atlantis/pull/26))


# neus-atlantis 1.5.0-9001 
(built with altantis rev 6490)

Dynamic and forced models diverge at this point. ROMS_COBALT-Forcing ([#25](https://github.com/NOAA-EDAB/neus-atlantis/pull/25))

## Breaking changes (major)




## New Features (minor)

Addition of new ROM_COBALT forcing for physics only. Includes annual forcing from 1964-2014.

## Bug Fixes (patch)

### Changes to: at_biology.prm
* Reduces magnitude of Diatom (PL) bloom and stabilizes closer to initial biomass
    + Reduce mum_PL
    + Increase predation on PL by Meiobenthos (BO) in sediment (reduces sediment accumulation)
    + Decrease cannibalism of BO
* Slight increase in longevity of scallops (SCA) and Lobseter (LOB)
    + Decreased cannibalism in LOB (and other benthic carnivores)
    + Increase grazing of benthic filter feeders (CLA, SCA, QHG, BFF) on PL and decrease consumption of refractory detritus (DR)

# neus-atlantis 1.0
built with atlantis 

Not released. Depreciated
