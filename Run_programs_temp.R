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

### Obtain SCCS based GHCN stations that are closest to the SCCS point
stationDF <- select_9_ghcn_stations(corDF, gDF)

### Obtain GHCN station list to process
station.list <- c(stationDF$ghcn1, stationDF$ghcn2, stationDF$ghcn3,
                  stationDF$ghcn4, stationDF$ghcn5, stationDF$ghcn6,
                  stationDF$ghcn7, stationDF$ghcn8, stationDF$ghcn9)