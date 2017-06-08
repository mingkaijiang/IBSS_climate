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
### Calls script containing all necessary functions
source("R/functionCode_V4.R")

### Get SCCS coordinates and Station ID Files
corDF <- read.csv("data/weight_dis_ht.csv")

### GHCN station ID list
station.list <- unique(corDF$SITE.name)

##############################################################################################################
#### select on SCCS sites based on their information sheet
### Step 1: 
### Convert all files from .dly to .csv format, removed data quality flag
ConvertFiles(sourceDir = "data/ghcnd_all/ghcnd_all/",
             stations = station.list,
             destDir = "data/ghcnd_selected")

### Step 2:
### Check year range quality - only include data with > 10 yrs of data
### Return GHCN sites with < 10 yr of data
YrRange10(sourceDir = "data/ghcnd_selected")

### Step 3:
### Restructure the files to continuous days, added leap years
### Replacing old with new files
ReStructureFile(sourceDir = "data/ghcnd_selected", destDir = "data/ghcnd_selected")


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
##Calculate whole year range predictability
##Bin prcp as biome-specific and calculate their specific P,M,C values individually

##Calculate R10 predictability
R10S_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/R10S_pred")

##Sweep through all files and put file name onto each table row as station ID
##Ready for correlating with long, lat, elev information in a separate data table
##!!Important: Need to change to corresponding extreme predictability directory!!!

setwd("E:/IBSS/Output/R10S_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    ## return the dataframe
})
## combine into a single dataframe
myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/r10s_summary.csv",col.names=T,row.names=F,sep=",")


##Calculate R20 predictability
R20S_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/R20S_pred")

setwd("E:/IBSS/Output/R20S_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/r20s_summary.csv",col.names=T,row.names=F,sep=",")

##Calculate PRCPTOT predictability
PRCPTOTS_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/PRCPTOTS_pred")

setwd("E:/IBSS/Output/PRCPTOTS_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/prcptots_summary.csv",col.names=T,row.names=F,sep=",")

##Calculate R95P predictability
##Need to exclude two files:
##SU000062650.csv in the ThrIndS folder
R95PS_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/R95PS_pred")

setwd("E:/IBSS/Output/R95PS_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/r95ps_summary.csv",col.names=T,row.names=F,sep=",")


##Calculate R99P predictability
##Need to exclude two files:
##ASN00015628.csv & WA004150450.csv in the ThrIndS folder
R99PS_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/R99PS_pred")

setwd("E:/IBSS/Output/R99PS_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/r99ps_summary.csv",col.names=T,row.names=F,sep=",")

R05PS_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/R05PS_pred")

setwd("E:/IBSS/Output/R05PS_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/r05ps_summary.csv",col.names=T,row.names=F,sep=",")

R01PS_pred(sourceDir = "E:/IBSS/Output/ThrIndS/selected", destDir = "E:/IBSS/Output/R01PS_pred")

setwd("E:/IBSS/Output/R01PS_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/r01ps_summary.csv",col.names=T,row.names=F,sep=",")


##Calculate RX1S predictability
RX1S_pred(sourceDir = "E:/IBSS/Output/RX1S/selected", destDir = "E:/IBSS/Output/RX1S_pred")

setwd("E:/IBSS/Output/RX1S_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/rx1s_summary.csv",col.names=T,row.names=F,sep=",")


##Calculate RX5S predictability
RX5S_pred(sourceDir = "E:/IBSS/Output/RX5S/selected", destDir = "E:/IBSS/Output/RX5S_pred")

setwd("E:/IBSS/Output/RX5S_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat   
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/rx5s_summary.csv",col.names=T,row.names=F,sep=",")

##Calculate SDII predictability
SDIIS_pred(sourceDir = "E:/IBSS/Output/SDIIS/selected", destDir = "E:/IBSS/Output/SDIIS_pred")

setwd("E:/IBSS/Output/SDIIS_pred")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

write.table(myDF,"E:/IBSS/Output/Spatial_pattern/sdiis_summary.csv",col.names=T,row.names=F,sep=",")


##############################################################################################################
##Check working directory, clean up memory, re-read script
#getwd()
#rm(list=ls(all=TRUE))
#source("E:/IBSS/Code/myCode/functionCode.r")

##############################################################################################################
##Filter data with year range > 60 years for long term trend analysis
YrRange60(sourceDir = "E:/IBSS/Output/selected10", destDir = "E:/IBSS/Output/selected60")

##############################################################################################################
##Seasonal extreme indices


##Calculate seasonal 1D prcp and save into corresponding directory
RX1S(sourceDir = "E:/IBSS/Output/selected60", destDir = "E:/IBSS/Output/RX1S_60")

setwd("E:/IBSS/Output/RX1S_60")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF$idcsv <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 2)
myDF$id <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 1)

myDF <- subset(myDF, select=-idcsv)

corDF <- read.csv("E:/IBSS/DEM/selected_biome.csv",header=T)

newDF <- merge(corDF, myDF, by.x="Site_ID", by.y="id")

newDF <- newDF[order(newDF$year),]

splnames <- split(newDF,newDF$Site_ID) 

lapply(names(splnames), function(x){write.table(splnames[[x]], file = paste(x,".csv", sep = ""), 
                                                sep=",", row.names=F)})

NullRemove(sourceDir = "E:/IBSS/Output/RX1S_60", destDir = "E:/IBSS/Output/RX1S_60/selected")

setwd("E:/IBSS/Output/RX1S_60/selected")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

biome_rng <- matrix(0,ncol=7,nrow=14)
dimnames(biome_rng)<-list(NULL,c("WWF_Num","WWF_Name","prcpmax","prcpmin","prcprng",
                                 "prcpmean","prcpstdev"))

biome_rng <- as.data.frame(biome_rng)

biome_rng$WWF_Num <- c(1:14)
biome_rng$WWF_Name <- c("Tropical and subtropical moist broadleaf forest","Tropical and subtropical dry broadleaf forest",
                        "Tropical and subtropical coniferous forest","Temperate broadleaf and mixed forest",
                        "Temperate conifer forest","Boreal forest and taiga","Tropical and subtropical grassland",
                        "Temperate grassland","flooded grassland and savannas","Montane grassland and shrubland",
                        "Tundra","Meditterranean forest","Deserts and xeric shrubland","Mangroves")
for (i in 1:14)
{
  biome_rng$prcpmax[i] <- max(myDF[myDF$WWF_MHTNUM == i, "annual"])
  biome_rng$prcpmin[i] <- min(myDF[myDF$WWF_MHTNUM == i, "annual"])
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- mean(myDF[myDF$WWF_MHTNUM == i, "annual"])
  biome_rng$prcpstdev[i] <- sd(myDF[myDF$WWF_MHTNUM == i, "annual"])
}

path <- "E:/IBSS/Output/biome_prcp_rng_60"
dir.create(path, showWarnings = FALSE)

write.table(biome_rng, file.path(path,"biome_RX1S.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

##Calculate seasonal 5D prcp and save into corresponding directory
RX5S(sourceDir = "E:/IBSS/Output/selected60", destDir = "E:/IBSS/Output/RX5S_60")

setwd("E:/IBSS/Output/RX5S_60")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF$idcsv <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 2)
myDF$id <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 1)

myDF <- subset(myDF, select=-idcsv)

corDF <- read.csv("E:/IBSS/DEM/selected_biome.csv",header=T)

newDF <- merge(corDF, myDF, by.x="Site_ID", by.y="id")

newDF <- newDF[order(newDF$year),]

splnames <- split(newDF,newDF$Site_ID) 

lapply(names(splnames), function(x){write.table(splnames[[x]], file = paste(x,".csv", sep = ""), 
                                                sep=",", row.names=F)})

NullRemove(sourceDir = "E:/IBSS/Output/RX5S_60", destDir = "E:/IBSS/Output/RX5S_60/selected")

setwd("E:/IBSS/Output/RX5S_60/selected")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

biome_rng <- matrix(0,ncol=7,nrow=14)
dimnames(biome_rng)<-list(NULL,c("WWF_Num","WWF_Name","prcpmax","prcpmin","prcprng",
                                 "prcpmean","prcpstdev"))

biome_rng <- as.data.frame(biome_rng)

biome_rng$WWF_Num <- c(1:14)
biome_rng$WWF_Name <- c("Tropical and subtropical moist broadleaf forest","Tropical and subtropical dry broadleaf forest",
                        "Tropical and subtropical coniferous forest","Temperate broadleaf and mixed forest",
                        "Temperate conifer forest","Boreal forest and taiga","Tropical and subtropical grassland",
                        "Temperate grassland","flooded grassland and savannas","Montane grassland and shrubland",
                        "Tundra","Meditterranean forest","Deserts and xeric shrubland","Mangroves")
for (i in 1:14)
{
  biome_rng$prcpmax[i] <- max(myDF[myDF$WWF_MHTNUM == i, "annual"])
  biome_rng$prcpmin[i] <- min(myDF[myDF$WWF_MHTNUM == i, "annual"])
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- mean(myDF[myDF$WWF_MHTNUM == i, "annual"])
  biome_rng$prcpstdev[i] <- sd(myDF[myDF$WWF_MHTNUM == i, "annual"])
}

path <- "E:/IBSS/Output/biome_prcp_rng_60"
dir.create(path, showWarnings = FALSE)

write.table(biome_rng, file.path(path,"biome_RX5S.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)


##Calculate threshold based indices, R10, R20, R95P, R99P,PRCPTOT at seasonal timestep
ThrIndS(sourceDir = "E:/IBSS/Output/selected60", destDir = "E:/IBSS/Output/ThrIndS_60")

setwd("E:/IBSS/Output/ThrIndS_60")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF$idcsv <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 2)
myDF$id <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 1)

myDF <- subset(myDF, select=-idcsv)

corDF <- read.csv("E:/IBSS/DEM/selected_biome.csv",header=T)

newDF <- merge(corDF, myDF, by.x="Site_ID", by.y="id")

newDF <- newDF[order(newDF$year),]

splnames <- split(newDF,newDF$Site_ID) 

lapply(names(splnames), function(x){write.table(splnames[[x]], file = paste(x,".csv", sep = ""), 
                                                sep=",", row.names=F)})

NullRemove(sourceDir = "E:/IBSS/Output/ThrIndS_60", destDir = "E:/IBSS/Output/ThrIndS_60/selected")

setwd("E:/IBSS/Output/ThrIndS_60/selected")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

biome_rng <- matrix(0,ncol=7,nrow=14)
dimnames(biome_rng)<-list(NULL,c("WWF_Num","WWF_Name","prcpmax","prcpmin","prcprng",
                                 "prcpmean","prcpstdev"))

biome_rng <- as.data.frame(biome_rng)

biome_rng$WWF_Num <- c(1:14)
biome_rng$WWF_Name <- c("Tropical and subtropical moist broadleaf forest","Tropical and subtropical dry broadleaf forest",
                        "Tropical and subtropical coniferous forest","Temperate broadleaf and mixed forest",
                        "Temperate conifer forest","Boreal forest and taiga","Tropical and subtropical grassland",
                        "Temperate grassland","flooded grassland and savannas","Montane grassland and shrubland",
                        "Tundra","Meditterranean forest","Deserts and xeric shrubland","Mangroves")

for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "R10_spr"], myDF[myDF$WWF_MHTNUM == i, "R10_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "R10_aut"], myDF[myDF$WWF_MHTNUM == i, "R10_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "R10_spr"], myDF[myDF$WWF_MHTNUM == i, "R10_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "R10_aut"], myDF[myDF$WWF_MHTNUM == i, "R10_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "R10_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "R10_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "R10_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "R10_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "R10_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "R10_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "R10_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "R10_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}

path <- "E:/IBSS/Output/biome_prcp_rng_60"
dir.create(path, showWarnings = FALSE)

write.table(biome_rng, file.path(path,"biome_R10S.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "R20_spr"], myDF[myDF$WWF_MHTNUM == i, "R20_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "R20_aut"], myDF[myDF$WWF_MHTNUM == i, "R20_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "R20_spr"], myDF[myDF$WWF_MHTNUM == i, "R20_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "R20_aut"], myDF[myDF$WWF_MHTNUM == i, "R20_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "R20_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "R20_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "R20_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "R20_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "R20_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "R20_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "R20_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "R20_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}

write.table(biome_rng, file.path(path,"biome_R20S.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "r95p_spr"], myDF[myDF$WWF_MHTNUM == i, "r95p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r95p_aut"], myDF[myDF$WWF_MHTNUM == i, "r95p_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "r95p_spr"], myDF[myDF$WWF_MHTNUM == i, "r95p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r95p_aut"], myDF[myDF$WWF_MHTNUM == i, "r95p_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "r95p_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "r95p_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "r95p_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "r95p_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "r95p_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "r95p_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "r95p_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "r95p_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}


write.table(biome_rng, file.path(path,"biome_R95PS.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "r99p_spr"], myDF[myDF$WWF_MHTNUM == i, "r99p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r99p_aut"], myDF[myDF$WWF_MHTNUM == i, "r99p_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "r99p_spr"], myDF[myDF$WWF_MHTNUM == i, "r99p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r99p_aut"], myDF[myDF$WWF_MHTNUM == i, "r99p_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "r99p_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "r99p_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "r99p_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "r99p_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "r99p_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "r99p_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "r99p_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "r99p_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}

write.table(biome_rng, file.path(path,"biome_R99PS.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "r05p_spr"], myDF[myDF$WWF_MHTNUM == i, "r05p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r05p_aut"], myDF[myDF$WWF_MHTNUM == i, "r05p_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "r05p_spr"], myDF[myDF$WWF_MHTNUM == i, "r05p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r05p_aut"], myDF[myDF$WWF_MHTNUM == i, "r05p_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "r05p_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "r05p_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "r05p_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "r05p_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "r05p_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "r05p_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "r05p_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "r05p_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}


write.table(biome_rng, file.path(path,"biome_R05PS.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "r01p_spr"], myDF[myDF$WWF_MHTNUM == i, "r01p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r01p_aut"], myDF[myDF$WWF_MHTNUM == i, "r01p_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "r01p_spr"], myDF[myDF$WWF_MHTNUM == i, "r01p_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "r01p_aut"], myDF[myDF$WWF_MHTNUM == i, "r01p_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "r01p_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "r01p_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "r01p_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "r01p_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "r01p_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "r01p_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "r01p_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "r01p_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}


write.table(biome_rng, file.path(path,"biome_R01PS.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)


for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "prcptot_spr"], myDF[myDF$WWF_MHTNUM == i, "prcptot_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "prcptot_aut"], myDF[myDF$WWF_MHTNUM == i, "prcptot_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "prcptot_spr"], myDF[myDF$WWF_MHTNUM == i, "prcptot_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "prcptot_aut"], myDF[myDF$WWF_MHTNUM == i, "prcptot_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "prcptot_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "prcptot_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "prcptot_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "prcptot_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "prcptot_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "prcptot_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "prcptot_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "prcptot_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}
write.table(biome_rng, file.path(path,"biome_PRCPTOTS.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

##Calculate prcp/# of wet days over each season and save into corresponding directory
SDIIS(sourceDir = "E:/IBSS/Output/selected60", destDir = "E:/IBSS/Output/SDIIS_60")

setwd("E:/IBSS/Output/SDIIS_60")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF$idcsv <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 2)
myDF$id <- lapply(strsplit(as.character(myDF$id), "\\."), "[", 1)

myDF <- subset(myDF, select=-idcsv)

corDF <- read.csv("E:/IBSS/DEM/selected_biome.csv",header=T)

newDF <- merge(corDF, myDF, by.x="Site_ID", by.y="id")

newDF <- newDF[order(newDF$year),]

splnames <- split(newDF,newDF$Site_ID) 

lapply(names(splnames), function(x){write.table(splnames[[x]], file = paste(x,".csv", sep = ""), 
                                                sep=",", row.names=F)})

NullRemove(sourceDir = "E:/IBSS/Output/SDIIS_60", destDir = "E:/IBSS/Output/SDIIS_60/selected")

setwd("E:/IBSS/Output/SDIIS_60/selected")

filenames <- Sys.glob("*.csv")  
allData <- lapply(filenames, function(.file)
{
  dat<-read.csv(.file, header=T)
  dat$id<-as.character(.file)
  dat    
})

myDF <- do.call(rbind, allData)
myDF <- as.data.frame(myDF, row.names = NULL, stringsAsFactors = FALSE)

myDF <- subset(myDF, select=-id)

myDF[myDF$sdii_spr<=(-99.),"sdii_spr"]<-NA
myDF[myDF$sdii_sum<=(-99.),"sdii_sum"]<-NA
myDF[myDF$sdii_aut<=(-99.),"sdii_aut"]<-NA
myDF[myDF$sdii_win<=(-99.),"sdii_win"]<-NA

biome_rng <- matrix(0,ncol=7,nrow=14)
dimnames(biome_rng)<-list(NULL,c("WWF_Num","WWF_Name","prcpmax","prcpmin","prcprng",
                                 "prcpmean","prcpstdev"))

biome_rng <- as.data.frame(biome_rng)

biome_rng$WWF_Num <- c(1:14)
biome_rng$WWF_Name <- c("Tropical and subtropical moist broadleaf forest","Tropical and subtropical dry broadleaf forest",
                        "Tropical and subtropical coniferous forest","Temperate broadleaf and mixed forest",
                        "Temperate conifer forest","Boreal forest and taiga","Tropical and subtropical grassland",
                        "Temperate grassland","flooded grassland and savannas","Montane grassland and shrubland",
                        "Tundra","Meditterranean forest","Deserts and xeric shrubland","Mangroves")
for (i in 1:14)
{
  biome_max <- max(myDF[myDF$WWF_MHTNUM == i, "sdii_spr"], myDF[myDF$WWF_MHTNUM == i, "sdii_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "sdii_aut"], myDF[myDF$WWF_MHTNUM == i, "sdii_win"], na.rm=T)
  
  biome_min <- min(myDF[myDF$WWF_MHTNUM == i, "sdii_spr"], myDF[myDF$WWF_MHTNUM == i, "sdii_sum"],
                   myDF[myDF$WWF_MHTNUM == i, "sdii_aut"], myDF[myDF$WWF_MHTNUM == i, "sdii_win"], na.rm=T)
  
  biome_spr <- mean(myDF[myDF$WWF_MHTNUM == i, "sdii_spr"],na.rm=T)
  biome_sum <- mean(myDF[myDF$WWF_MHTNUM == i, "sdii_sum"],na.rm=T)
  biome_aut <- mean(myDF[myDF$WWF_MHTNUM == i, "sdii_aut"],na.rm=T)
  biome_win <- mean(myDF[myDF$WWF_MHTNUM == i, "sdii_win"],na.rm=T)
  
  biome_mean <- mean(biome_spr,biome_sum,biome_aut,biome_win, na.rm=T)
  
  sd_spr <- sd(myDF[myDF$WWF_MHTNUM == i, "sdii_spr"],na.rm=T)
  sd_sum <- sd(myDF[myDF$WWF_MHTNUM == i, "sdii_sum"],na.rm=T)
  sd_aut <- sd(myDF[myDF$WWF_MHTNUM == i, "sdii_aut"],na.rm=T)
  sd_win <- sd(myDF[myDF$WWF_MHTNUM == i, "sdii_win"],na.rm=T)
  sddata <- rbind(sd_spr,sd_sum,sd_aut,sd_win)
  
  biome_sd <- sd(sddata, na.rm=T)
  
  biome_rng$prcpmax[i] <- biome_max
  biome_rng$prcpmin[i] <- biome_min
  biome_rng$prcprng[i] <- biome_rng$prcpmax[i] - biome_rng$prcpmin[i]
  biome_rng$prcpmean[i] <- biome_mean
  biome_rng$prcpstdev[i] <- biome_sd
}

path <- "E:/IBSS/Output/biome_prcp_rng_60"
dir.create(path, showWarnings = FALSE)

write.table(biome_rng, file.path(path,"biome_SDIIS.csv", fsep = .Platform$file.sep),
            sep=",", row.names=F,col.names=T)

##############################################################################################################
##Calculate predictability for 30-year running mean of extremes

##Calculate 30-year moving average of RX1S predictability based on > 60 year dataset
RX1S_pred_move(sourceDir = "E:/IBSS/Output/RX1S_60/selected", destDir = "E:/IBSS/Output/RX1S_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/RX1S_60_pred", destDir = "E:/IBSS/Output/RX1S_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/RX1S_60_pred/focal", destDir = "E:/IBSS/Output/RX1S_60_plot")

##Calculate 30-year moving average of RX5S predictability based on > 60 year dataset
RX5S_pred_move(sourceDir = "E:/IBSS/Output/RX5S_60/selected", destDir = "E:/IBSS/Output/RX5S_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/RX5S_60_pred", destDir = "E:/IBSS/Output/RX5S_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/RX5S_60_pred/focal", destDir = "E:/IBSS/Output/RX5S_60_plot")

##Calculate 30-year moving average of R10S predictability based on > 60 year dataset
R10S_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/R10S_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/R10S_60_pred", destDir = "E:/IBSS/Output/R10S_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/R10S_60_pred/focal", destDir = "E:/IBSS/Output/R10S_60_plot")

##Calculate 30-year moving average of R20S predictability based on > 60 year dataset
R20S_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/R20S_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/R20S_60_pred", destDir = "E:/IBSS/Output/R20S_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/R20S_60_pred/focal", destDir = "E:/IBSS/Output/R20S_60_plot")

##Calculate 30-year moving average of R95PS predictability based on > 60 year dataset
R95PS_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/R95PS_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/R95PS_60_pred", destDir = "E:/IBSS/Output/R95PS_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/R95PS_60_pred/focal", destDir = "E:/IBSS/Output/R95PS_60_plot")

##Calculate 30-year moving average of R99PS predictability based on > 60 year dataset
R99PS_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/R99PS_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/R99PS_60_pred", destDir = "E:/IBSS/Output/R99PS_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/R99PS_60_pred/focal", destDir = "E:/IBSS/Output/R99PS_60_plot")

##Calculate 30-year moving average of R05PS predictability based on > 60 year dataset
R05PS_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/R05PS_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/R05PS_60_pred", destDir = "E:/IBSS/Output/R05PS_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/R05PS_60_pred/focal", destDir = "E:/IBSS/Output/R05PS_60_plot")

##Calculate 30-year moving average of R01PS predictability based on > 60 year dataset
R01PS_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/R01PS_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/R01PS_60_pred", destDir = "E:/IBSS/Output/R01PS_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/R01PS_60_pred/focal", destDir = "E:/IBSS/Output/R01PS_60_plot")

##Calculate 30-year moving average of SDIIS predictability based on > 60 year dataset
SDIIS_pred_move(sourceDir = "E:/IBSS/Output/SDIIS_60/selected", destDir = "E:/IBSS/Output/SDIIS_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/SDIIS_60_pred", destDir = "E:/IBSS/Output/SDIIS_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/SDIIS_60_pred/focal", destDir = "E:/IBSS/Output/SDIIS_60_plot")

##Calculate 30-year moving average of PRCPTOTS predictability based on > 60 year dataset
PRCPTOTS_pred_move(sourceDir = "E:/IBSS/Output/ThrIndS_60/selected", destDir = "E:/IBSS/Output/PRCPTOTS_60_pred")

FocalYr_cross(sourceDir = "E:/IBSS/Output/PRCPTOTS_60_pred", destDir = "E:/IBSS/Output/PRCPTOTS_60_pred/focal")

PredPlot(sourceDir = "E:/IBSS/Output/PRCPTOTS_60_pred/focal", destDir = "E:/IBSS/Output/PRCPTOTS_60_plot")

##############################################################################################################
