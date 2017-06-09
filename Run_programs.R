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

### SCCS station list
sDF <- unique(corDF[corDF$SCCSID,])

## Get GHCN station list
gDF <- read.csv("data/ghcnd-stations.csv")

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
ReStructureFile(sourceDir = "data/ghcnd_selected", destDir = "data/ghcnd_selected")

### Step 3:
### Check year range quality - only include data with > 10 yrs of data
###                          - and data with < 20% missing values
YrRange10(sourceDir = "data/ghcnd_selected")

### Step 4: 
### Gap filling
Gap_Fill(sourceDir = "data/ghcnd_selected", destDir = "data/ghcnd_gap_filled")

##############################################################################################################
#### Compute indices

### Step 1:
### Compute coefficient of variation for all GHCN stations
CoefVar(sourceDir = "data/ghcnd_selected", destDir = "data/indices")

### Step 2: 
### Calculate seasonal 1D prcp and save into corresponding directory
RX1S(sourceDir = "data/ghcnd_selected", destDir = "data/indices/rx1s")

### Step 3:
##Calculate seasonal 5D prcp and save into corresponding directory
RX5S(sourceDir = "data/ghcnd_selected", destDir = "data/indices/rx5s")

### Step 4: 
### Calculate threshold based indices, R10, R20, R95P, R99P,PRCPTOT at seasonal timestep
ThrIndS(sourceDir = "data/ghcnd_selected", destDir = "data/indices/ThrIndS")

### Step 5:
### Calculate prcp/# of wet days over each season and save into corresponding directory
SDIIS(sourceDir = "data/ghcnd_selected", destDir = "data/indices/SDIIS")


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

