## Actual computation of consecutive day indices
## based on no growing season for each SCCS society

compute_consecutive_indices_no_growing_season <- function(wea.station, sccs.id, source, dest) {
    # s.date: starting doy for plant 1
    # e.date: end doy for plant 1
    # wea.station: weather station id list
    # sccs.id: sccs id
    # dest: dest directory
    
    require(lubridate)
    
    in.list <- paste0(wea.station, ".csv")
    out.list <- paste0(wea.station, "_sccs", sccs.id, "_no_plant.csv")
    
    for (k in 1:length(wea.station)) 
    {
        # print the file to screen
        print(wea.station[k])
        
        # prepare input and output file names
        inName <- file.path(source, in.list[k], fsep = .Platform$file.sep)
        outName <- file.path(dest, out.list[k], fsep = .Platform$file.sep)
        
        # read in file and prepare the df
        dd <- read.csv(inName)
        colnames(dd)<-c("id","year","month","day","prcp")
        dd$doy <- yday(dd$id)

        # prepare output df
        outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                            NA, NA, NA, NA, NA)
        colnames(outDF) <- c("year", "dry_DJF", "dry_MAM", "dry_JJA", "dry_SON",
                             "wet_DJF", "wet_MAM", "wet_JJA", "wet_SON")
        outDF <- outDF[-1,]
        
        # count # days 
        for (j in outDF$year) {
            # extract the four seasons
            d1 <- subset(dd[dd$year == (j-1),], month == 12)
            d2 <- subset(dd[dd$year == j,], month >= 1 & month <= 2)
            djf <- rbind(d1, d2)
            mam <- subset(dd[dd$year == j,], month >= 3 & month <= 5)
            jja <- subset(dd[dd$year == j,], month >= 6 & month <= 8)
            son <- subset(dd[dd$year == j,], month >= 9 & month <= 11)
            
            # consecutive dry days in the three periods
            dry_djf <- rle(djf$prcp)
            dry_mam <- rle(mam$prcp)
            dry_jja <- rle(jja$prcp)
            dry_son <- rle(son$prcp)
            
            
            outDF[outDF$year == j, "dry_DJF"] <- max(dry_djf$lengths[dry_djf$values==0]) / 91.0
            outDF[outDF$year == j, "dry_MAM"] <- max(dry_mam$lengths[dry_mam$values==0]) / 92.0
            outDF[outDF$year == j, "dry_JJA"] <- max(dry_jja$lengths[dry_jja$values==0]) / 92.0
            outDF[outDF$year == j, "dry_SON"] <- max(dry_son$lengths[dry_son$values==0]) / 91.0
            
            # consecutive wet days in the three periods
            wet_djf <- rle(djf$prcp)
            wet_mam <- rle(mam$prcp)
            wet_jja <- rle(jja$prcp)
            wet_son <- rle(son$prcp)
            
            outDF[outDF$year == j, "wet_DJF"] <- ifelse(length(wet_djf$lengths[wet_djf$values>0]) == 0, 0, max(wet_djf$lengths[wet_djf$values>0]) / 91.0)
            outDF[outDF$year == j, "wet_MAM"] <- ifelse(length(wet_mam$lengths[wet_mam$values>0]) == 0, 0, max(wet_mam$lengths[wet_mam$values>0]) / 92.0)
            outDF[outDF$year == j, "wet_JJA"] <- ifelse(length(wet_jja$lengths[wet_jja$values>0]) == 0, 0, max(wet_jja$lengths[wet_jja$values>0]) / 92.0)
            outDF[outDF$year == j, "wet_SON"] <- ifelse(length(wet_son$lengths[wet_son$values>0]) == 0, 0, max(wet_son$lengths[wet_son$values>0]) / 91.0)
            
        }
        
        # remove the first and last year to avoid incomplete year problem
        outDF1 <- outDF[-1,]
        l <- dim(outDF1)[1]
        outDF2 <- outDF1[-l,]
        
        # write output
        write.csv(outDF2, outName, row.names=F)
    }
    print(paste0("finish k loop ", k))
    
}

