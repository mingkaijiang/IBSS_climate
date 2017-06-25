################################### Script for calculating predictability of extremes   #######################
################################### Created by: Mingkai Jiang                           #######################
################################### First created on: 2015-03-10                        #######################
################################### Modified on: 2017-06-08                             #######################
##############################################################################################################
##############################################################################################################


##get relevant library functions
#library(reshape)
#library(reshape2)
#library(lubridate) #needed for the leap_year function
#library(eeptools)
##############################################################################################################
#### Prepare all functions and coordinate files
### clear workspace
rm(list=ls())

### Calls script containing all necessary functions
source("R/functionCode_V4.R")

### Get SCCS coordinates and Station ID Files
corDF <- read.csv("data/weight_dis_ht.csv")

### Get GHCN station list
gDF <- read.csv("data/ghcnd-stations.csv")

### Obtain SCCS based GHCN stations that are closest to the SCCS point
stationDF <- select_9_ghcn_stations(corDF, gDF)

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
### Check year range quality - only include data with > 10 yrs of data
#YrRange10(sourceDir = "data/ghcnd_selected")

### Step 3:
### Restructure the files to continuous days, added leap years
### Replacing old with new files
ReStructureFile(sourceDir = "data/ghcnd_selected", destDir = "data/restructured")

### Step 4:
### Check year range quality - only include data with > 10 yrs of data
###                          - and data with < 20% missing values
station.list.upd <- Missing_check(station.list, 
                                  sourceDir = "data/restructured", 
                                  destDir = "data/ghcnd_gap_filled")

## update stationDF
stationDF.upd <- Update_station_list(station.list.upd, stationDF)

### Step 5: 
### Gap filling 1. - use statistical correlation among 9 stations to gap fill all 9 stations
###                - excluding big chunk of missing data which will be filled later use a different function

## the following sites are problematic, so excluded in the first run
## problem sites: 0 (non-NA) cases: non-missing data do not overlap across all sites
##                 dim(X) must hvae a postive length: two sites overlapping problem
##                lmCoef[j, i]: subscript out of bounds: 
##                error in modDF$date: 14, 15, 53, 61, 71, 
stationDF2 <- stationDF.upd[-c(18, 20, 22, 44, 65, 69,            # 0 (non-NA) cases
                               24, 40, 46, 60,                    # dim(X) must have a positive length
                               41, 54),]                          # lmCoef[j, i]: subscript out of bounds
Gap_Fill(stationDF2, 
         sourceDir = "data/ghcnd_gap_filled", 
         destDir = "data/ghcnd_gap_filled")

# Fill all remaining stations using data within the station

### Step 6:
### Gap filling 2. - use same period in other years to fill big chunk of missing data
###                - and the remaining unfilled sites
Gap_Fill_within_station(station.list.upd, 
                        sourceDir = "data/ghcnd_gap_filled",
                        destDir = "data/ghcnd_gap_filled")

##############################################################################################################
#### Compute indices

### Step 1:
### Compute coefficient of variation for all GHCN stations
CoefVar(sourceDir = "data/ghcnd_gap_filled_2", destDir = "data/indices")

### Step 2: 
### Calculate seasonal 1D prcp and save into corresponding directory
RX1S(sourceDir = "data/ghcnd_gap_filled_2", destDir = "data/indices/rx1s")

### Step 3:
##Calculate seasonal 5D prcp and save into corresponding directory
RX5S(sourceDir = "data/ghcnd_gap_filled_2", destDir = "data/indices/rx5s")

### Step 4: 
### Calculate threshold based indices, R10, R20, R95P, R99P,PRCPTOT at seasonal timestep
ThrIndS(sourceDir = "data/ghcnd_gap_filled_2", destDir = "data/indices/ThrIndS")

### Step 5:
### Calculate prcp/# of wet days over each season and save into corresponding directory
SDIIS(sourceDir = "data/ghcnd_gap_filled_2", destDir = "data/indices/SDIIS")


##############################################################################################################
#### Calculate whole year range predictability

##Calculate R10 predictability
R10S_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate R20 predictability
R20S_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate PRCPTOT predictability
PRCPTOTS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate R95P predictability
##Need to exclude two files:
##SU000062650.csv in the ThrIndS folder
R95PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")



### To do next

##Calculate R99P predictability
##Need to exclude two files:  
##ASN00015628.csv & WA004150450.csv in the ThrIndS folder
R99PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


R05PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")



R01PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


##Calculate RX1S predictability
RX1S_pred(sourceDir = "data/indices/rx1s", destDir = "data/predictability")


##Calculate RX5S predictability
RX5S_pred(sourceDir = "data/indices/rx5s", destDir = "data/predictability")


##Calculate SDII predictability
SDIIS_pred(sourceDir = "data/indices/SDIIS", destDir = "data/predictability")





##############################################################################################################
##Filter data with year range > 60 years for long term trend analysis
YrRange60(sourceDir = "E:/IBSS/Output/selected10", destDir = "E:/IBSS/Output/selected60")

