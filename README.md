# IBSS Climate

This is the code repository for computing precipitation extremes and their associated predictability scores at the chosen ethnographic sites (SCCS) for the Interdisciplinary Behavioral and Social Science (IBSS) project, based on data from the GHCN daily precipitation dataset. 

At each SCCS site, 9 GHCN stations were chosen based on the nearest distance approach for gap-filling purpose. All 9 GHCN stations are gap-filled at the same time.

Extreme precipitation indices include:
R10S: number of days with >10 mm/d of precipitation within a season
R20S: number of days with >20 mm/d of precipitation within a season
R01S: number of days with <1 mm/d of precipitation within a season
R05S: number of days with <5 mm/d of precipitation within a season
R95P: the 95th percentile of daily precipitation
R99P: the 99th percentile of daily precipitation
PRCPTOT: annual total precipitation

Additionally, there are consecutive dry and wet day indices. 

Predictabiilty scores are calculated based on the Colwell index for each extreme index. 

# Repository structure
To run the script, simply run the "Run_programs.R" in the repository.
All functions are stored in folder "modules".
All pre-requisitory scripts are stored in folder "R".
Raw GHCN daily dataset must be downloaded from the GHCN website and store in  data/ghcnd_all/ghcnd_all (this directory is where the script is looking for input). 

# References:
Colwell, R.K., 1974. Predictability, constancy, and contingency of periodic phenomena. Ecology, 55: 1148-1153.
Jiang et al., 2016. Journal of Climate (doi: https://doi.org/10.1175/JCLI-D-15-0560.1).
Jiang et al., 2016. Scientific Reports (doi: 10.1038/srep29962).
