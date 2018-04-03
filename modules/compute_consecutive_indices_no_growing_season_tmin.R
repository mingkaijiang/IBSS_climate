## Actual computation of consecutive day indices
## based on no growing season for each SCCS society

compute_consecutive_indices_no_growing_season_tmin <- function(wea.station, sccs.id, source, dest) {
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
        colnames(outDF) <- c("year", "cold_DJF", "cold_MAM", "cold_JJA", "cold_SON",
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
            
            tmin10djf<-percentile(length(djf),djf$tmin,0.1)
            tmin10mam<-percentile(length(mam),mam$tmin,0.1)
            tmin10jja<-percentile(length(jja),jja$tmin,0.1)
            tmin10son<-percentile(length(son),son$tmin,0.1)
            
            # consecutive cold days in the four periods
            cold_djf <- rle(djf$tmin - tmin10djf)
            cold_mam <- rle(mam$tmin - tmin10mam)
            cold_jja <- rle(jja$tmin - tmin10jja)
            cold_son <- rle(son$tmin - tmin10son)
            
            outDF[outDF$year == j, "cold_djf"] <- max(cold_djf$lengths[tmin10djf$values<0]) / 91.0
            outDF[outDF$year == j, "cold_mam"] <- max(cold_mam$lengths[tmin10mam$values<0]) / 92.0
            outDF[outDF$year == j, "cold_jja"] <- max(cold_jja$lengths[tmin10jja$values<0]) / 92.0
            outDF[outDF$year == j, "cold_son"] <- max(cold_son$lengths[tmin10osn$values<0]) / 91.0
            

            
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

