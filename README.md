# IBSS_climate

This is the code repository for computing precipitation extremes and their associated predictability scores at the chosen ethnographic sites (SCCS) for the Interdisciplinary Behavioral and Social Science (IBSS) project, based on data from the GHCN daily dataset. 

At each SCCS site, 9 GHCN stations were chosen based on the nearest distance approach to represent for gap-filling purpose. Only the closest GHCN station is gap-filled, based on the rest 8 stations. 

Extreme precipitation indices include:
R10S: number of days with >10 mm/d of precipitation within a season
R20S: number of days with >20 mm/d of precipitation within a season
R01S: number of days with <1 mm/d of precipitation within a season
R05S: number of days with <5 mm/d of precipitation within a season
R95P: the 95th percentile of daily precipitation
R99P: the 99th percentile of daily precipitation
PRCPTOT: annual total precipitation
SDII: 

Predictabiilty scores are calculated based on the Colwell index.

# Repository structure
To run the script, simply run the "Run_programs.R" in the repository.
All functions are stored in folder "modules".
All processing scripts are stored in folder "R".
Raw GHCN daily dataset must be downloaded from the GHCN website and store in  data/ghcnd_all/ghcnd_all



More to be written...


# References:
Colwell (1974)
Jiang et al. (2016) Journal of Climate
Jiang et al. (2016) Scientific Reports