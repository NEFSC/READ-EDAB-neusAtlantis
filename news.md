## Breaking changes (major)

## New Features (minor)

## Bug Fixes (patch)


# neus-atlantis 2.0.0 
(built with altantis rev 6536)

First official release.

* Reorganization of functional groups
* Forcing of model physics (advection, temperature, salinity)
* Forcing of phytoplankton groups
* Recalibrated so all species persist

See forthcoming publication for model details

# neus-atlantis 1.0.0-9056 (in dev) 
(built with altantis rev 6536)

Reorganization of data-raw and data products 

As described in [#125](https://github.com/NOAA-EDAB/neus-atlantis/pull/125) 

# neus-atlantis 1.0.0-9055 (in dev) 
(built with altantis rev 6536)

Spatial biomass diagnostic: Added code to post processing files to create presence/absence plots of species movement. 

As described in [#123](https://github.com/NOAA-EDAB/neus-atlantis/pull/123) 

# neus-atlantis 1.0.0-9054 (in dev) 
(built with altantis rev 6536)

Large Fish diagnostic: Added code to post processing files to create plots of the largest fish (by weight) in the model

As described in [#122](https://github.com/NOAA-EDAB/neus-atlantis/pull/122) 

# neus-atlantis 1.0.0-9053 (in dev) 
(built with altantis rev 6536)

Mortality diagnostics: Added code to post processing files to create mortality plots

As described in [#121](https://github.com/NOAA-EDAB/neus-atlantis/pull/121) 


# neus-atlantis 1.0.0-9052 (in dev) 
(built with altantis rev 6536)

ZM Spatial Patterns (issue: ZM depletion regionally)

* All groups persisting (besides nutrient/detritus/macro-algae)
* All but STB and SDF stable
* Fixed issues with regional depletion in ZM
* mum, C, FSP, mQ changes to planktivores.
* Added seasonal movements to ZM similar to existing ZL

As described in [#120](https://github.com/NOAA-EDAB/neus-atlantis/pull/120) 

# neus-atlantis 1.0.0-9051 (in dev) 
(built with altantis rev 6536)

Benthic & Zooplankton Rebalancing

* Decrease initial HER biomass
* Reduce biomass of LSQ, ISQ, OSH, NSH, BG, BD, BO
* More stable ZL and ZM timeseries
* Attempts to get flatter HER, MAK, MEN, and BUT timeseries (not completely successful)

As described in [#118](https://github.com/NOAA-EDAB/neus-atlantis/pull/118) + [#119](https://github.com/NOAA-EDAB/neus-atlantis/pull/119) 

# neus-atlantis 1.0.0-9050 (in dev) 
(built with altantis rev 6536)

Rebalance and Stability.

* General tuning of all groups not passing calibration criteria after changes to phytoplankton forcing. Started with focus on reaching stability with zooplankton, then planktivores, then rest.

* Reverted HERinitial conditions back to stock SMART levels
* Tuning of C, mum, mQ for Benthos, Demersals and Zoo


As described in [#117](https://github.com/NOAA-EDAB/neus-atlantis/pull/117) 

# neus-atlantis 1.0.0-9049 (in dev) 
(built with altantis rev 6536)

Updated phytoplankton (PL, PS, DF) forcing files to accurately convert from Chlorophyll. Established zooplankton and planktivore persistence.

As described in [#114](https://github.com/NOAA-EDAB/neus-atlantis/pull/114) 


# neus-atlantis 1.0.0-9048 (in dev) 
(built with altantis rev 6536)

Chlorophyll to Nitrogen ratio changed from 7 to 1/7. 

* New forcing files generated.
* Preliminary adjustments to biology.prm to get model to complete run. Needs further tuning.

As described in [#111](https://github.com/NOAA-EDAB/neus-atlantis/pull/111) 



# neus-atlantis 1.0.0-9047 (in dev) 
(built with altantis rev 6536)

Age structure fixes for WIF, SUF, RHK, POL, STB.
Stable and persisting, with good age structures.

* changes to c mum, FSP, mQ


As described in [#109](https://github.com/NOAA-EDAB/neus-atlantis/pull/109) + [#110](https://github.com/NOAA-EDAB/neus-atlantis/pull/110)




# neus-atlantis 1.0.0-9046 (in dev) 
(built with altantis rev 6536)

Improve Stability of REP, COD, CLA. (Remaining are other bivalves, WIF, and TAU.) 

* growth rates, mortality, reduced dominant predators on TAU and WIF.

As described in [#108](https://github.com/NOAA-EDAB/neus-atlantis/pull/108)


# neus-atlantis 1.0.0-9045 (in dev) 
(built with altantis rev 6536)

All persist, but not at "reasonable" levels

* Initial conditions changes and additional mum/C/mQ changes for MAK, WHK, BLF, MPF, BPF, FDE, and COD.
* Decreased FSP_BLF.
* Increased KDENR_BPF

As described in [#107](https://github.com/NOAA-EDAB/neus-atlantis/pull/107)



# neus-atlantis 1.0.0-9044 (in dev) 
(built with altantis rev 6536)

Persistence of fish group  GOO, COD, BUT, BLF, WHK

* Increase mum and C.
* Bracketed FSP based on mum and C

All pass persistence, only FDE doesnt persist

As described in [#106](https://github.com/NOAA-EDAB/neus-atlantis/pull/106)



# neus-atlantis 1.0.0-9043 (in dev) 
(built with altantis rev 6536)

Improves shellfish persistence of SCA, QHG, CLA, and LOB.

* Reduces DOG, LSK, and BC growth.
* Increase LOB growth. 
* Increased CLA, SCA, and QHG clearance rates.

Passes persistence, but not stability/reasonability

As described in [#105](https://github.com/NOAA-EDAB/neus-atlantis/pull/105)



# neus-atlantis 1.0.0-9042 (in dev) 
(built with altantis rev 6536)

Increase starting Herring biomass in at_run.prm. More herring actually dampens big peaks at start of run for ANC, HER, MAK, MEN, ZL, and ZM. Causes WIF to crash.

As described in [#104](https://github.com/NOAA-EDAB/neus-atlantis/pull/104)

# neus-atlantis 1.0.0-9041 (in dev) 
(built with altantis rev 6536)

FSP and mQ value changes to MPF, BUT, POL, RED, STB, YTF, INV

As described in [#103](https://github.com/NOAA-EDAB/neus-atlantis/pull/103)

# neus-atlantis 1.0.0-9040 (in dev) 
(built with altantis rev 6536)

### Summary 
Several changes related to the growth rates, clearance rates, mortalities, diets, and recruitment for HER, ZL, ZM, ZS. Reduced initial Herring biomass and catch to be similar to 1970s rather than peak in 1960s.

## Parameter Files
1. total_catch.ts - Reduced Herring catch to 25,000 mt per year during spinup
2. at_biology.prm:
  -- shifted from benthic grazers to zooplankton grazers
  -- Shifted ZL from eating PL to ZM
  -- Fixed mismatch in adult/juv order for pPREYs
  -- Balanced mum/C/mQ for ZL, ZM, ZS, and HER
  -- Shift HER growth towards reserves over structure
  -- Adjust assimilation efficiencies for ZL/ZM  
  -- Tune FSP_HER/ KSPA_HER
3. at_run.prm: Increase starting ZL, ZM, and ZS. Decreased starting HER


As described in [#102](https://github.com/NOAA-EDAB/neus-atlantis/pull/102)

# neus-atlantis 1.0.0-9039 (in dev) 
(built with altantis rev 6536)

* Changes to FSP, mQ and recruitmentFLag for MPF, WHK, BLF, MAK, WIF, BUT, COD, SHK,
POL, RHK, BSB, SCU, RED, STB, HAD, YTL


As described in [#99](https://github.com/NOAA-EDAB/neus-atlantis/pull/99)



# neus-atlantis 1.0.0-9038 (in dev) 
(built with altantis rev 6536)

* HER persist. 
* Benthic and pelagic grazers share prey 
* catch spin up added to time series
* changes to diets of HER, ZL, ZM (mum, C, pPREY) (at_biol.prm)
* smoothed seasonal movement for ZL and OSH  (at_biol.prm)
* Changes to HER_age_mat and FSP_HER  (at_biol.prm)
* Reduced RN:SN for Herring and reset initial numbers (neus_init.nc)
* changes to initial scalars for HER, QHG, CLA, SCA (at_run.prm)


As described in [#98](https://github.com/NOAA-EDAB/neus-atlantis/pull/98)


# neus-atlantis 1.0.0-9037 (in dev) 
(built with altantis rev 6536)

All but HER persist. All but WHK above 50% starting biomass. SDF,ZM, ZS > 10x starting biomass

* biomass recruitment for GOO
* diet changes
* reduce C, mum
* benthic invertebrates tuning. prevent BC, LOB, BMS growing to high levels

As described in [#93](https://github.com/NOAA-EDAB/neus-atlantis/pull/93)

# neus-atlantis 1.0.0-9036 (in dev) 
(built with altantis rev 6536)

Stable lower trophic system

* forage fish, flat fish, zoo diet change
* C, mum, mQ fine tuning for zoo
* C, mum, mQ fine tuning for HER
* spawn based recruitment for HER
* adjust catch-at-age distribution for HER (age class 2)

As described in [#92](https://github.com/NOAA-EDAB/neus-atlantis/pull/92)

# neus-atlantis 1.0.0-9035 (in dev) 
(built with altantis rev 6536)

* Add biomass based recruitment and quadratic mortality for SUF, DRM

As described in [#91](https://github.com/NOAA-EDAB/neus-atlantis/pull/91)

# neus-atlantis 1.0.0-9034 (in dev) 
(built with altantis rev 6536)

Improve biomass for fish groups over first 20 yrs by tuning zoo

* update initial scalars for ZL, ZM, ZS
* pPREY, C, mum for ZG, ZL, ZM, ZS

As described in [#90](https://github.com/NOAA-EDAB/neus-atlantis/pull/90)

# neus-atlantis 1.0.0-9033 (in dev) 
(built with altantis rev 6536)

forage fish and zoo rebalancing

* mum and C reduced for ZG, ZL, ZM, ZS
* mum and C tuned (between existing and Ryan M values) HER, MAK, MEN, ANC
* predation of forage fish on themselves and zoo reduced
* removed predators on ZL (GOO, WOL, POR, PSH)
* cut all pPREY on ZL in half

As described in [#89](https://github.com/NOAA-EDAB/neus-atlantis/pull/89)


# neus-atlantis 1.0.0-9032 (in dev) 
(built with altantis rev 6536)

* New Atlantis version fixed migration hack
* All migration parameters were reset


As described in [#88](https://github.com/NOAA-EDAB/neus-atlantis/pull/88)

# neus-atlantis 1.0.0-9031 (in dev) 
(built with altantis rev 6490.1)

* changed pPREY on SCA, BEF and LOB by main predators.
* reduced clearance rates for benthic predators. All benthics now persist
* increased growth rates and recruits for bencthic carnivores. All in reasonable ranges

As described in [#87](https://github.com/NOAA-EDAB/neus-atlantis/pull/87)


# neus-atlantis 1.0.0-9030 (in dev) 
(built with altantis rev 6490.1)

* increased clearance for benthic predators and zoo
* reduced growth for benthic predators
* reduced pPREY for zoo and shellfish
* diet changes for epibenthic groups

As described in [#86](https://github.com/NOAA-EDAB/neus-atlantis/pull/86)


# neus-atlantis 1.0.0-9029 (in dev) 
(built with altantis rev 6490.1)

* Atlantic salmon SAL, catch removed

As described in [#85](https://github.com/NOAA-EDAB/neus-atlantis/pull/85)


# neus-atlantis 1.0.0-9028 (in dev) 
(built with altantis rev 6490.1)

* Decreased quadratic mortality to TWH, SWH to allow higher levels
* mum, C, E changes to LOB, RCB, BMS, BC for persistence

As described in [#79](https://github.com/NOAA-EDAB/neus-atlantis/pull/79)

# neus-atlantis 1.0.0-9027 (in dev) 
(built with altantis rev 6490.1)

Added new catch data using [comlandr](https://github.com/NOAA-EDAB/comlandr) output. This led to:

* Updating  initial scalars
* Additional changes to mum, C, KENDR, mL, mQ and pPrey across many age-structured groups due to new catch time series
* Fisher active all day (at_harvest.prm)


As described in [#78](https://github.com/NOAA-EDAB/neus-atlantis/pull/78)

# neus-atlantis 1.0.0-9026 (in dev) 
(built with altantis rev 6490.1)

* Persistence (LOB, QHG, BC, SCA, CLA, BFF, BG, RCB, BMS)
* mum, clearance rates for filter feeders, assimilation efficiency, pprey for these groups and their predators.
* initial condition multiplier = 1


As described in [#76](https://github.com/NOAA-EDAB/neus-atlantis/pull/76)

# neus-atlantis 1.0.0-9025 (in dev) 
(built with altantis rev 6490.1)

* BWH, REP, RWH persist
* Quadratic mortality added to BWH, RWH, REP
* Assimilation efficiency, growth rates and pPREY values adjusted


As described in [#72](https://github.com/NOAA-EDAB/neus-atlantis/pull/72)

# neus-atlantis 1.0.0-9024 (in dev) 
(built with altantis rev 6490.1)


* mortality mL decreased for COD and MAK

* mQ decrease, change in KDENR for predators crashing
* mQ increase, change in KDENR for predators with rapis or sustained growth


As described in [#71](https://github.com/NOAA-EDAB/neus-atlantis/pull/71)




# neus-atlantis 1.0.0-9023 (in dev) 
(built with altantis rev 6490.1)

* Filter feeders, planktivores, and predators on bacteria pPREY reduced.

* All planktivores pPREY changed to prefere zooplankton over detritus and bacteria

* Assimilation efficiencies on live prey of HER, MAK,MEN,ANC,GOO reduced

* Recruitment rates on ISQ, LSQ, NSH reduced

* Recruitment for all higher predatory fish tuned to reduce all groups biomass to within 5x initial conditions for the first 15 years. Groups that had StockSmart recruitment timeseries available were tuned from median recruitment over all available data.


As described in [#70](https://github.com/NOAA-EDAB/neus-atlantis/pull/70)

# neus-atlantis 1.0.0-9022 (in dev) 
(built with altantis rev 6490.1)

* Retuned bluefin tuna (BFT). 
* Deal with growth, survivorship outside of the model (HMS)

As described in [#69](https://github.com/NOAA-EDAB/neus-atlantis/pull/69)

# neus-atlantis 1.0.0-9021 (in dev) 
(built with altantis rev 6490.1)

* Data pulled from [comlandr](https://github.com/NOAA-EDAB/comlandr) and [survdat](https://github.com/NOAA-EDAB/survdat) packages. Merged and associated with functional group names for use in calibration.

As described in [#68](https://github.com/NOAA-EDAB/neus-atlantis/pull/68)

# neus-atlantis 1.0.0-9020 (in dev) 
(built with altantis rev 6490.1)

* Parameter changes for migratory pelagics and plantivore persistence

As described in [#65](https://github.com/NOAA-EDAB/neus-atlantis/pull/65)
As described in [#66](https://github.com/NOAA-EDAB/neus-atlantis/pull/66)
As described in [#67](https://github.com/NOAA-EDAB/neus-atlantis/pull/67)


# neus-atlantis 1.0.0-9019 (in dev) 
(built with altantis rev 6490.1)

* Fix for migratory species not returning after leaving the domain

As described in [#61](https://github.com/NOAA-EDAB/neus-atlantis/pull/61)

# neus-atlantis 1.0.0-9018 (in dev) 
(built with altantis rev 6490.1)

* New detritus forcing

As described in [#54](https://github.com/NOAA-EDAB/neus-atlantis/pull/54)

# neus-atlantis 1.0.0-9017 (in dev) 
(built with altantis rev 6490.1)

* New forcing files for phytoplankton

As described in [#53](https://github.com/NOAA-EDAB/neus-atlantis/pull/53)

# neus-atlantis 1.0.0-9016 (in dev) 
(built with altantis rev 6490.1)

* Use GLORYS 12v1 for physics forcing due to ROMS bug.
(Temperature, Salinity, Horizontal advection

* Use ECC 4v4 for Vertical advection

As described in [#50](https://github.com/NOAA-EDAB/neus-atlantis/pull/50)


# neus-atlantis 1.0.0-9015 (in dev) 
(built with altantis rev 6490.1)

* Species persistence tuning

ZG: Reduced predation on, increased assimilation efficiency, increased growth
MEN: Increased assimilation efficiency and number of recruits
SUF: Increased assimilation efficiency and number of recruits
WPF: Increased number of recruits and growth rate
TYL: Increased number of recruits
DRM: Increased number of recruits
BC: Increased growth rate
LOB: Increased growth rate

As described in [#49](https://github.com/NOAA-EDAB/neus-atlantis/pull/49)


# neus-atlantis 1.0.0-9014 (in dev) 
(built with altantis rev 6490.1)

* New script to break up COBALT's **nlg** and **silg** into Atlantis' Diatom_N, Diatiom_S, and DinoFlag_N. Addition of new annual forcing files "roms_largephyto_force_*.nc"

As described in [#48](https://github.com/NOAA-EDAB/neus-atlantis/pull/48)

# neus-atlantis 1.0.0-9013 (in dev) 
(built with altantis rev 6490.1)

* Changes to at_biology parameter file to get all species persisting again after the last change to plankton levels. (RCB, LOB, BMS and MEN). Modified MEN assimilation efficiencies
* Changed growth rates and assimilation efficiencies for LOB, RCB and BMS. All previously persisting species continue to persist

As described in [#47](https://github.com/NOAA-EDAB/neus-atlantis/pull/47)

# neus-atlantis 1.0.0-9012 (in dev) 
(built with altantis rev 6490.1)

* Work aimed at investigating why the temperature in the forcing files did not mirror observations well. The source of the discrepancy was outside of our model development/processing but other changes were made to the forcing that should improve Atlantis' representation of the ROMS_COBALT output.

* Temperature Diagnostics. Addition of EPU shape files for SOE comparisons
Various scripts aimed at diagnosing temperature (or any forcing) issues with model (i.e. box-level_Scripts to plot NCEI temperature againts ROMS data on various temporal/spatial scales
* Changes to Aggregation. Established workflow for adding new depth layer. Did not substantially improve model temperature. Updated aggregating statistic/methods. Weighted mean of each layers within a cell.
*Updated Forcing Files

As described in [#46](https://github.com/NOAA-EDAB/neus-atlantis/pull/46)

# neus-atlantis 1.0.0-9011 (in dev) 
(built with altantis rev 6490.1)

* Increased Initial Biomass for Lobster and Red Crab
* Decreased predation between and cannibalism by Lobster and Red Crab
* Increased growth on Macroalgae - important food source for Benthic Grazers
* Decreased predation by LOB, RCB, BC, BMS (other crabs) on Benthic Grazers
* Increased growth on BG
* Increased assimilation efficiencies for BG, LOB, RCB
* All benthic species persist to the end. Clams appear to have too high a biomass, so the next step will be to try to bring them down 

As described in [#45](https://github.com/NOAA-EDAB/neus-atlantis/pull/45)


# neus-atlantis 1.0.0-9010 (in dev) 
(built with altantis rev 6490.1)

* update of the correct COBALT v10 output, and includes the ROMS-aggregations scripts, ROMS diagnostics, forcing generation, forcing diagnostics, and Atlantis integration steps. 
* Some scripts have just been updated for a more consistent file naming and directory organization. 
* The biggest change to the final forcing is that the alternative (non-Hydroconstruct) routine was used for temperature and salinity so that it's consistent with the other box variables.

As described in [#44](https://github.com/NOAA-EDAB/neus-atlantis/pull/44)


# neus-atlantis 1.0.0-9009 (in dev) 
(built with altantis rev 6490.1)

* Edited source code to turn on age-structured output flag. This makes the ANNAGEBIO.nc and ANNAGECATCH.nc. This is a temporary fix, and will need to be updated from the SVN when implemented officially.

As described in [#42](https://github.com/NOAA-EDAB/neus-atlantis/pull/42)


# neus-atlantis 1.0.0-9008 (in dev) 
(built with altantis rev 6490)

#### Post-processing Routine split across 3 functions

* get_atl_paramfiles.R: Generates paths for parameter file names
* process_atl_output.R: Creates "result" object similar to the one generated by atlantistools package (modified from RM_preprocess_v2.R)
* make_atlantis_diagnostic_figures.R: Gerenates figures (modified from RM_atl_model_calibration.R)
* test post-processing.R: example call of post-processing functions

#### New file names for R scripts
* Under format Action_Subject_Modifier (i.e. plot_ROMS_summary.R)
organized under common "action" terms

    > make_* = creates non-figure files
    >
    > plot_* = creates figures
    >
    > fix_* = modifies files to correct/adjust values
    >
    > flatten_* = Converts netCDF files into flat files/lists
    >
    > call_* = Scripts that call on existing functions
 
#### Slight modifications to ROMS processing script (make_ROMS_files_2.R)
* Generalized to allow for multi-day input files. Should work for arbitrary length but only tested for annual files.
* Changed call on above script for new COBALT files

As described in [#40](https://github.com/NOAA-EDAB/neus-atlantis/pull/40)


# neus-atlantis 1.0.0-9007 (in dev) 
(built with altantis rev 6490)

* Mackerel and Menhaden Update
* Increased Mackerel recruits
    + Mackerel persists to end now, though at perhaps too high a biomass - will need to be tuned further after next forcing files
* Increased Menhaden recruits, assimilation efficiency and modified age structure of the catch to allow larger age classes to persist better
* Increased growth on Menhaden's first 3 age classes
* Menhaden persists to end now and at about half of Mackerel's biomass

As described in [#37](https://github.com/NOAA-EDAB/neus-atlantis/pull/37) & 
[#39](https://github.com/NOAA-EDAB/neus-atlantis/pull/39)

# neus-atlantis 1.0.0-9006 (in dev) 
(built with altantis rev 6490)

#### Major fixes

* Fixes incorrect conversion from COBALT (mol X/ kg H20) to Atlantis (mg X/m3) units
* Updated LTL-Forcing scripts
* Removed initial conditions scaling of forced LTL groups

As described in [#30]( https://github.com/NOAA-EDAB/neus-atlantis/pull/30)

#### Minor fixes

Adds forcing files for NH3, NO3, O2, and Si. Includes:
* New scripts
* Pulling and aggregating data from ROMS_COBALT
* Converting data into Atlantis forcing files
* Plotting variables from nutrient forcing
* Added new nutrient forcing files
* Updated force_WIN_test.prm with nutrient forcing

As described in [#33](https://github.com/NOAA-EDAB/neus-atlantis/pull/33)


# neus-atlantis 1.0.0-9005 (in dev) 
(built with altantis rev 6490)

Tested the run with the COBALT forcing for lower trophic levels
* Herring persisted at full fishing levels - but still drop after the 50th time step, not to as great a magnitude as previously
Increased herring recruit numbers
* Mackerel persist and probably need to have biological parameters reduced a bit to constrain growth

As described in [#31]( https://github.com/NOAA-EDAB/neus-atlantis/pull/31)

# neus-atlantis 1.0.0-9004 (in dev) 
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

# neus-atlantis 1.0.0-9003 (in dev) 
(built with altantis rev 6490)

Incorporates new LTL forcing, associated parameter file changes, and additional R scripts to process and interpret forcing data ([#26](https://github.com/NOAA-EDAB/neus-atlantis/pull/26))


# neus-atlantis 1.0.0-9002 (in dev)
(built with altantis rev 6490)

Dynamic and forced models diverge at this point. ROMS_COBALT-Forcing ([#25](https://github.com/NOAA-EDAB/neus-atlantis/pull/25))

# neus-atlantis 1.0.0-9001 (in dev)
(built with altantis rev 6490)

* Reduces magnitude of Diatom (PL) bloom and stabilizes closer to initial biomass
    + Reduce mum_PL
    + Increase predation on PL by Meiobenthos (BO) in sediment (reduces sediment accumulation)
    + Decrease cannibalism of BO
* Slight increase in longevity of scallops (SCA) and Lobseter (LOB)
    + Decreased cannibalism in LOB (and other benthic carnivores)
    + Increase grazing of benthic filter feeders (CLA, SCA, QHG, BFF) on PL and decrease consumption of refractory detritus (DR)
* As described [commit](https://github.com/NOAA-EDAB/neus-atlantis/commit/b9ae18adc29c1f55b94da4b61eb9fb0ebadaae04)

Changes to: at_biology.prm


# neus-atlantis 1.0
(built with atlantis rev ??)

Not released. Depreciated

NEFSC [tech memo](https://nefsc.noaa.gov/publications/tm/tm218/): Jason S. Link, Robert J. Gamble, Elizabeth A. Fulton (2011). NEUS – Atlantis: Construction, Calibration, and Application of an Ecosystem Model with Ecological Interactions, Physiographic Conditions, and Fleet Behavior. 
