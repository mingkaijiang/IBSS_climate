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
corDF <- read.csv("input/weight_dis_ht.csv")

### Get GHCN station list
gDF <- read.csv("input/ghcnd-stations.csv")

### Get growing season information
growDF <- read.csv("input/PlantSeasonality.csv")

### Process growing season data to have single entry for each SCCS society
growDF <- growing_season_single_entry(growDF)

### Obtain SCCS based GHCN stations that are closest to the SCCS point
stationDF <- select_5_ghcn_stations(corDF, gDF, growDF)

### Obtain GHCN station list to process
station.list <- c(stationDF$ghcn1,stationDF$ghcn2,stationDF$ghcn3,stationDF$ghcn4,stationDF$ghcn5)

##############################################################################################################
#### select on SCCS sites based on their information sheet
### Step 1: 
### Convert all files from .dly to .csv format, removed data quality flag
ConvertFiles_Temp(sourceDir = "data/ghcnd_all/ghcnd_all/",
             stations = station.list,
             destDir = "data/ghcnd_selected_tmax")

### Step 2:
### Restructure the files to continuous days, added leap years
### Replacing old with new files
ReStructureFile(sourceDir = "data/ghcnd_selected_tmax", destDir = "data/restructured_tmax")

### Step 3:
### Check year range quality - only include data with > 10 yrs of data
###                          - and data with < 50% missing values
station.list.upd <- Missing_check(station.list, 
                                  sourceDir = "data/restructured_tmax", 
                                  destDir = "data/ghcnd_gap_filled_tmax")

## update stationDF
stationDF.upd <- Update_station_list_5_stations(station.list.upd, stationDF)

### Step 4: 
### Gap filling 1. - use statistical correlation among 9 stations to gap fill all 9 stations
###                - excluding big chunk of missing data which will be filled later use a different function

## the following sites are problematic, so excluded in the first run
## problem sites: 0 (non-NA) cases: non-missing data do not overlap across all sites
##                 dim(X) must hvae a postive length: two sites overlapping problem

stationDF2 <- stationDF.upd[-c(14,21,27,28,29,42,51,56,58,64,66,71,72,76),]

Gap_Fill_5_stations(stationDF2, 
                    sourceDir = "data/ghcnd_gap_filled_tmax", 
                    destDir = "data/ghcnd_gap_filled_tmax")

# Fill all remaining stations using data within the station
### Step 5:
### Gap filling 2. - use same period in other years to fill big chunk of missing data
###                - and the remaining unfilled sites
Gap_Fill_within_station_5_stations(station.list.upd, 
                        sourceDir = "data/ghcnd_gap_filled_tmax",
                        destDir = "data/ghcnd_gap_filled_tmax")

### Step 6:
### Check year range quality - only include data with > 10 yrs of data
### commented out because Missing_check is doing this
YrRange10(sourceDir = "data/ghcnd_gap_filled_tmax")


### Step 7:
### Update stationDF2 to remove all removed stations from this list 
### and add the growing season information
final_station_DF <-Final_station_list_5_stations(sourceDir = "data/ghcnd_gap_filled_tmax", stationDF.upd,
                                        outname="tmax")

##############################################################################################################
#### Compute indices

### Step 1: 
### Calculate threshold based index fd at seasonal timestep
ThrIndS_tmaxextr_ann(sourceDir = "data/ghcnd_gap_filled_tmax", destDir = "data/indices/ThrIndS_tmax")

### Step 2: 
### Calculate consecutive days indices
consecutive_day_indices_tmax(final_station_DF, 
                        sourceDir = "data/ghcnd_gap_filled_tmax", destDir = "data/indices/WSDI")

### Calculate consecutive days indices for hunter/gatherer societies, i.e. sites without any plant grow information
consecutive_day_indices_hunter_gatherer_tmax(final_station_DF, 
                                        sourceDir = "data/ghcnd_gap_filled_tmax", destDir = "data/indices/WSDI_hunter_gatherer")

### Calculate annual consecutive day indices, regardless of the societies
CSDI_annual(sourceDir = "data/ghcnd_gap_filled_tmax", destDir = "data/indices/annual_consecutive_tmax")

##############################################################################################################
#### Calculate whole year range predictability

##Calculate fd predictability
su_pred(sourceDir = "data/indices/ThrIndS_tmax", destDir = "data/predictability")

##Calculate tmax predictability
tmax_pred(sourceDir = "data/indices/ThrIndS_tmax", destDir = "data/predictability")

## Calculate CSDI consecutive predictability
consec_warm_pred(sourceDir = "data/indices/WSDI", destDir = "data/predictability")

## Calculate CSDI consecutive predictability for hunter and gatherer societies
consec_warm_pred_hunter_gatherer(sourceDir = "data/indices/WSDI_hunter_gatherer", destDir = "data/predictability")

##############################################################################################################
### End
rm(list=ls())




