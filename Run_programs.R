################################### Script for calculating predictability of extremes   #######################
################################### Created by: Mingkai Jiang                           #######################
################################### First created on: 2015-03-10                        #######################
################################### Modified on: 2017-06-08                             #######################
##############################################################################################################
##############################################################################################################
##############################################################################################################
#### Prepare all functions and coordinate files
### clear workspace
rm(list=ls())

### Calls script containing all necessary functions
source("R/prepare_R.R")

### Get SCCS coordinates and Station ID Files
corDF <- read.csv("data/weight_dis_ht.csv")

### Get GHCN station list
gDF <- read.csv("data/ghcnd-stations.csv")

### Get growing season information
growDF <- read.csv("data/PlantSeasonality.csv")

### Process growing season data to have single entry for each SCCS society
growDF <- growing_season_single_entry(growDF)

### Obtain SCCS based GHCN stations that are closest to the SCCS point
stationDF <- select_9_ghcn_stations(corDF, gDF, growDF)

### Obtain GHCN station list to process
station.list <- c(stationDF$ghcn1, stationDF$ghcn2, stationDF$ghcn3,
                  stationDF$ghcn4, stationDF$ghcn5, stationDF$ghcn6,
                  stationDF$ghcn7, stationDF$ghcn8, stationDF$ghcn9)

##############################################################################################################
#### select on SCCS sites based on their information sheet
### Step 1: 
### Convert all files from .dly to .csv format, removed data quality flag
ConvertFiles(sourceDir = "data/ghcnd_all/ghcnd_all/",
             stations = station.list,
             destDir = "data/ghcnd_selected")

### Step 2:
### Restructure the files to continuous days, added leap years
### Replacing old with new files
ReStructureFile(sourceDir = "data/ghcnd_selected", destDir = "data/restructured")

### Step 3:
### Check year range quality - only include data with > 10 yrs of data
###                          - and data with < 50% missing values
station.list.upd <- Missing_check(station.list, 
                                  sourceDir = "data/restructured", 
                                  destDir = "data/ghcnd_gap_filled")

## update stationDF
stationDF.upd <- Update_station_list(station.list.upd, stationDF)

### Step 4: 
### Gap filling 1. - use statistical correlation among 9 stations to gap fill all 9 stations
###                - excluding big chunk of missing data which will be filled later use a different function

## the following sites are problematic, so excluded in the first run
## problem sites: 0 (non-NA) cases: non-missing data do not overlap across all sites
##                 dim(X) must hvae a postive length: two sites overlapping problem
##                lmCoef[j, i]: subscript out of bounds: 
##                error in modDF$date
stationDF2 <- stationDF.upd[-c(23,30,38,53,55,77,80,84,85,       # 0 (non-NA) cases
                               21,49,96,97,                  # dim(X) must have a positive length
                               24,51,62,69,70,78),]                          # lmCoef[j, i]: subscript out of bounds

#stationDF2 <- stationDF.upd[-c(1:85,96,97),]
Gap_Fill(stationDF2, 
         sourceDir = "data/ghcnd_gap_filled", 
         destDir = "data/ghcnd_gap_filled")

# Fill all remaining stations using data within the station
### Step 5:
### Gap filling 2. - use same period in other years to fill big chunk of missing data
###                - and the remaining unfilled sites
Gap_Fill_within_station(station.list.upd, 
                        sourceDir = "data/ghcnd_gap_filled",
                        destDir = "data/ghcnd_gap_filled")

### Step 6:
### Check year range quality - only include data with > 10 yrs of data
### commented out because Missing_check is doing this
YrRange10(sourceDir = "data/ghcnd_gap_filled")


### Step 7:
### Update stationDF2 to remove all removed stations from this list 
### and add the growing season information
final_station_DF <-Final_station_list(sourceDir = "data/ghcnd_gap_filled", stationDF.upd)

##############################################################################################################
#### Compute indices

### Step 1:
### Compute coefficient of variation for all GHCN stations
CoefVar(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices")

### Step 2: 
### Calculate seasonal 1D prcp and save into corresponding directory
RX1S(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/rx1s")

### Step 3:
##Calculate seasonal 5D prcp and save into corresponding directory
RX5S(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/rx5s")

### Step 4: 
### Calculate threshold based indices, R10, R20, R95P, R99P,PRCPTOT at seasonal timestep
ThrIndS(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/ThrIndS")

### Step 5:
### Calculate prcp/# of wet days over each season and save into corresponding directory
SDIIS(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/SDIIS")

### Step 6: 
### Calculate consecutive days indices
consecutive_day_indices(final_station_DF, 
                        sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/CDS")

### Calculate consecutive days indices for hunter/gatherer societies, i.e. sites without any plant grow information
consecutive_day_indices_hunter_gatherer(final_station_DF, 
                                        sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/CDS_hunter_gatherer")

##############################################################################################################
#### Calculate whole year range predictability

##Calculate R10 predictability
R10S_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate R20 predictability
R20S_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate PRCPTOT predictability
PRCPTOTS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate R95P predictability
R95PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


R99PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


R05PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


R01PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate RX1S predictability
RX1S_pred(sourceDir = "data/indices/rx1s", destDir = "data/predictability")


##Calculate RX5S predictability
RX5S_pred(sourceDir = "data/indices/rx5s", destDir = "data/predictability")

##Calculate SDII predictability
SDIIS_pred(sourceDir = "data/indices/SDIIS", destDir = "data/predictability")

## Calculate dry consecutive predictability
consec_dry_pred(sourceDir = "data/indices/CDS", destDir = "data/predictability")

## Calculate wet consecutive predictability
consec_wet_pred(sourceDir = "data/indices/CDS", destDir = "data/predictability")

## Calculate dry consecutive predictability for hunter and gatherer societies
consec_dry_pred_hunter_gatherer(sourceDir = "data/indices/CDS_hunter_gatherer", destDir = "data/predictability")

## Calculate wet consecutive predictability for hunter and gatherer societies
consec_wet_pred_hunter_gatherer(sourceDir = "data/indices/CDS_hunter_gatherer", destDir = "data/predictability")

##############################################################################################################
### End
rm(list=ls())
