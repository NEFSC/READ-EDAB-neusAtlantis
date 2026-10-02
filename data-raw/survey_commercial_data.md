---
title: "Survey and Commercial Fishing"
output: 
  html_document:
    keep_md: true
---



## Description of data

The survey biomass and catch (landings) of the species modelled in Atlantis are extracted from the svdbs database (using survdat) cfdbs (using comlandr) and assembled as a single data frame in long format


```r
library(magrittr)
# Connect to the database
channel <- dbutils::connect_to_database("servername","user")
# list species of interest
speciesList <- map_functional_group(channel)
# grab data from databases and assemble
data <- extract_landings_biomass(channel,species = speciesList)
```

```
## # A tibble: 39,671 x 9
##    YEAR  Species     EPU   Code  Functional_Group      VALUE SEASON TYPE   UNITS
##    <chr> <chr>       <fct> <chr> <chr>                 <dbl> <chr>  <chr>  <chr>
##  1 1960  Acadian re~ GOM   RED   Acadian redfish           2 <NA>   landi~ mt   
##  2 1960  Acadian re~ SS    RED   Acadian redfish         121 <NA>   landi~ mt   
##  3 1960  Alewife     SS    FDE   Shallow demersal fish  1318 <NA>   landi~ mt   
##  4 1960  American e~ SS    SDF   Atlantic states deme~    13 <NA>   landi~ mt   
##  5 1960  American p~ GOM   PLA   American plaice           1 <NA>   landi~ mt   
##  6 1960  American p~ SS    PLA   American plaice         919 <NA>   landi~ mt   
##  7 1960  American s~ SS    FDE   Shallow demersal fish   123 <NA>   landi~ mt   
##  8 1960  Atlantic c~ GOM   COD   Atlantic cod            129 <NA>   landi~ mt   
##  9 1960  Atlantic c~ GB    COD   Atlantic cod             19 <NA>   landi~ mt   
## 10 1960  Atlantic c~ SS    COD   Atlantic cod          12123 <NA>   landi~ mt   
## # ... with 39,661 more rows
```


## Methods

Species assigned to the initial Atlantis functional group designations (from v1.0 - found in `initialFunctionalGroupNames.csv`) were used to look up SVSPP, NESPP3, Species_ITIS codes

The `map_functional_groups.r` function extends these tables (from Atlantis v1.0)

```r
readr::read_csv(here::here("data-raw","initialFunctionalGroupNames.csv"))
```

```
## # A tibble: 89 x 2
##    `Group Code` `Group Name`       
##    <chr>        <chr>              
##  1 MAK          Atlantic mackerel  
##  2 HER          Atlantic herring   
##  3 WHK          White hake         
##  4 BLF          Bluefish           
##  5 WPF          Windowpane flounder
##  6 SUF          Summer flounder    
##  7 WIF          Winter flounder    
##  8 WTF          Witch flounder     
##  9 HAL          Atlantic halibut   
## 10 PLA          American plaice    
## # ... with 79 more rows
```

```r
readr::read_csv(here::here("data-raw","Atlantis_1_5_groups_svspp_nespp3.csv"))
```

```
## # A tibble: 233 x 4
##    Code  Name                SVSPP NESPP3
##    <chr> <chr>               <dbl>  <dbl>
##  1 MAK   Atlantic mackerel     121    212
##  2 HER   Atlantic herring       32    168
##  3 WHK   White hake             76    153
##  4 WHK   White hake             76    154
##  5 BLF   Bluefish              135     23
##  6 WPF   Windowpane flounder   108    125
##  7 SUF   Summer flounder       103    121
##  8 WIF   Winter flounder       106    119
##  9 WIF   Winter flounder       106    120
## 10 WTF   Witch flounder        107    122
## # ... with 223 more rows
```
to this table 

```r
readr::read_csv(here::here("data-raw","FunctionalGroupNames.csv"))
```

```
## # A tibble: 235 x 7
##    Code  Functional_Group   Species    Scientific_Name SVSPP NESPP3 Species_Itis
##    <chr> <chr>              <chr>      <chr>           <dbl>  <dbl>        <dbl>
##  1 ANC   Anchovies          Bay ancho~ ANCHOA MITCHIL~    43      6       161839
##  2 ANC   Anchovies          Striped a~ <NA>               44     NA           NA
##  3 BB    Sediment Bacteria  Sediment ~ <NA>               NA     NA           NA
##  4 BC    Benthic Carnivore  Starfish   ASTEROIDEA         NA    828       156862
##  5 BC    Benthic Carnivore  Common oc~ <NA>              511     NA           NA
##  6 BC    Benthic Carnivore  Octopus u~ OCTOPODIDAE       510    786        82590
##  7 BC    Benthic Carnivore  Spoonarm ~ <NA>              512     NA           NA
##  8 BC    Benthic Carnivore  Unicorn o~ <NA>              513     NA           NA
##  9 BD    Deposit Feeder     Deposit F~ <NA>               NA     NA           NA
## 10 BFF   Other benthic fil~ Bay scall~ ARGOPECTEN IRR~   402    799        79737
## # ... with 225 more rows
```

Each species is cross-referenced in the svdbs and cfdbs databases to obtain Species_itis codes and scientific names

The list of species codes is then used to extract commercial landings (`comlandr`) and biomass (`survdat`) data using `extract_landings_biomass.r`

