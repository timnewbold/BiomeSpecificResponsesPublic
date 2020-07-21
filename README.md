# BiomeSpecificResponsesPublic
A public release of the code used in the paper "Tropical and Mediterranean biodiversity is disproportionately sensitive to land-use and climate change"

Produced by [Tim Newbold](mailto:t.newbold[AT]ucl.ac.uk), with inputs from Philippa Oppenheimer, Jessica Williams and Adrienne Etard.

## A note on data availability
All of the data required to replicate the main analyses are published (DOI: [10.6084/m9.figshare.12674372](http://doi.org/10.6084/m9.figshare.12674372)).

The raw range maps required to run stage 17 (CalculateMeanRangeMap) cannot be made publicly available. Therefore, for this public release the output of this stage (MeanRangeSizeMap.rds) has been included instead in the input data directory, and the code that requires this map has been amended accordingly. The code for stage 17 has nevertheless been included for inspection. If you wish to run this stage, please [email me](mailto:t.newbold[AT]ucl.ac.uk).

## Running the code
Prior to running each stage of the analysis, you need to create a folder with the same name as the R script (minus the extension). E.g. 1_PrepareDiversityData for the first stage

Most stages take less than 10 minutes to run, with the exception of the following (time estimates are based on run time for a very good laptop computer):
- 4_RunModels: 3 hours
- 5_RunFinalModels: 11 minutes
- 15_RunExplanatoryModels: 9 hours
- 18_RunExplanatoryModelsClimate: 1 hour

## Required packages
The following packages need to be installed prior to running this analysis pipeline (available on CRAN unless specified):
- arm
- Formula
- ggplot2
- Hmisc
- lattice
- lme4
- maptools
- MASS
- Matrix
- mgcv
- nlme
- predictsFunctions (available via [Github](https://github.com/timnewbold/predicts-demo))
- raster
- RColorBrewer
- rgdal
- sf
- snow
- sp
- spData
- spdep
- StatisticalModels (available via [Github](https://github.com/timnewbold/StatisticalModels))
- survival
