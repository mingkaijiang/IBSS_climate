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
        colnames(dd)<-c("id","year","month","day","tmin")
        dd$doy <- yday(dd$id)

        # prepare output df
         outDF <- data.frame(unique(dd$year), NA, NA, NA, NA)
         colnames(outDF) <- c("year", "cold_djf", "cold_mam", "cold_jja", "cold_son")
         outDF <- outDF[-1,]
         
         tmin10djf<-quantile(djf$tmin,0.1)/10.0
         tmin10mam<-quantile(mam$tmin,0.1)/10.0
         tmin10jja<-quantile(jja$tmin,0.1)/10.0
         tmin10son<-quantile(son$tmin,0.1)/10.0

        # count # days 
        for (j in outDF$year) {
            # extract the four seasons
            d1 <- subset(dd[dd$year == (j-1),], month == 12)
            d2 <- subset(dd[dd$year == j,], month >= 1 & month <= 2)
            djf <- rbind(d1, d2)
            mam <- subset(dd[dd$year == j,], month >= 3 & month <= 5)
            jja <- subset(dd[dd$year == j,], month >= 6 & month <= 8)
            son <- subset(dd[dd$year == j,], month >= 9 & month <= 11)
            

            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) {print(tmin10mam)}
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) {print(mam$tmin/10.0 - tmin10mam)}
            
          
            #ifelse((djf$tmin/10.0 - tmin10mam) < 0.0, 0, 1)
            #ifelse((mam$tmin/10.0 - tmin10mam) < 0.0, 0, 1)
            #ifelse((jja$tmin/10.0 - tmin10mam) < 0.0, 0, 1)
            #ifelse((son$tmin/10.0 - tmin10mam) < 0.0, 0, 1)
            
            
            # consecutive cold days in the four periods
            cold_djf <- rle(ifelse((djf$tmin/10.0 - tmin10djf) < 0.0, 0, 1))
            cold_mam <- rle(ifelse((mam$tmin/10.0 - tmin10mam) < 0.0, 0, 1))
            cold_jja <- rle(ifelse((jja$tmin/10.0 - tmin10jja) < 0.0, 0, 1))
            cold_son <- rle(ifelse((son$tmin/10.0 - tmin10son) < 0.0, 0, 1))
            
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(cold_mam)}
            
            outDF[outDF$year == j, "cold_djf"] <- max(cold_djf$lengths[cold_djf$values==0]) / 91.0
            outDF[outDF$year == j, "cold_mam"] <- max(cold_mam$lengths[cold_mam$values==0]) / 92.0
            outDF[outDF$year == j, "cold_jja"] <- max(cold_jja$lengths[cold_jja$value==0]) / 92.0
            outDF[outDF$year == j, "cold_son"] <- max(cold_son$lengths[cold_son$values==0]) / 91.0
            
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(outDF[outDF$year == j, "cold_mam"])}
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(cold_mam$values)}
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(cold_mam$lengths[cold_mam$values==0])}
          

            
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

