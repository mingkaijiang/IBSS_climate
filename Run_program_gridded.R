################################### Script for calculating predictability of extremes   #######################
################################### based on gridded daily precipitation dataset        #######################
##  data source: https://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/cpc_global_precip/catalog.html       ##
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

### To convert SCCS site coordinates onto CPC gridded 0.5 resolution grid
corDF_upd <- Convert_coordinates(corDF)

### Create df to store CPC climate data
nc_to_csv(corDF_upd)

### Gap fill some random missing values
Gap_Fill_within_station(station.list.upd, 
                        sourceDir = "data/ghcnd_gap_filled",
                        destDir = "data/ghcnd_gap_filled")