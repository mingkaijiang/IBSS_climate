## Actual computation of consecutive day indices
## based on no growing season for each SCCS society

compute_consecutive_indices_no_growing_season_tmax <- function(wea.station, sccs.id, source, dest) {
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
        colnames(dd)<-c("id","year","month","day","tmax")
        dd$doy <- yday(dd$id)

        # prepare output df
         outDF <- data.frame(unique(dd$year), NA, NA, NA, NA)
         colnames(outDF) <- c("year", "warm_djf", "warm_mam", "warm_jja", "warm_son")
         outDF <- outDF[-1,]
         
         d1 <- subset(dd, month == 12)
         d2 <- subset(dd, month >= 1 & month <= 2)
         djf <- rbind(d1, d2)
         mam <- subset(dd, month >= 3 & month <= 5)
         jja <- subset(dd, month >= 6 & month <= 8)
         son <- subset(dd, month >= 9 & month <= 11)
         
         tmax10djf<-quantile(djf$tmax,0.9)/10.0
         tmax10mam<-quantile(mam$tmax,0.9)/10.0
         tmax10jja<-quantile(jja$tmax,0.9)/10.0
         tmax10son<-quantile(son$tmax,0.9)/10.0

        # count # days 
        for (j in outDF$year) {
            # extract the four seasons
            d1 <- subset(dd[dd$year == (j-1),], month == 12)
            d2 <- subset(dd[dd$year == j,], month >= 1 & month <= 2)
            djf <- rbind(d1, d2)
            mam <- subset(dd[dd$year == j,], month >= 3 & month <= 5)
            jja <- subset(dd[dd$year == j,], month >= 6 & month <= 8)
            son <- subset(dd[dd$year == j,], month >= 9 & month <= 11)
            

            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) {print(tmax10mam)}
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) {print(mam$tmax/10.0 - tmax10mam)}
            
          
            #ifelse((djf$tmax/10.0 - tmax10mam) < 0.0, 0, 1)
            #ifelse((mam$tmax/10.0 - tmax10mam) < 0.0, 0, 1)
            #ifelse((jja$tmax/10.0 - tmax10mam) < 0.0, 0, 1)
            #ifelse((son$tmax/10.0 - tmax10mam) < 0.0, 0, 1)
            
            
            # consecutive warm days in the four periods
            warm_djf <- rle(ifelse((djf$tmax/10.0 - tmax10djf) > 0.0, 0, 1))
            warm_mam <- rle(ifelse((mam$tmax/10.0 - tmax10mam) > 0.0, 0, 1))
            warm_jja <- rle(ifelse((jja$tmax/10.0 - tmax10jja) > 0.0, 0, 1))
            warm_son <- rle(ifelse((son$tmax/10.0 - tmax10son) > 0.0, 0, 1))
            
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(warm_mam)}
            
            outDF[outDF$year == j, "warm_djf"] <- max(warm_djf$lengths[warm_djf$values==0]) / 91.0
            outDF[outDF$year == j, "warm_mam"] <- max(warm_mam$lengths[warm_mam$values==0]) / 92.0
            outDF[outDF$year == j, "warm_jja"] <- max(warm_jja$lengths[warm_jja$value==0]) / 92.0
            outDF[outDF$year == j, "warm_son"] <- max(warm_son$lengths[warm_son$values==0]) / 91.0
            
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(outDF[outDF$year == j, "warm_mam"])}
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(warm_mam$values)}
            if(inName == "data/ghcnd_gap_filled/NG000061017.csv" && j == 1940) { print(warm_mam$lengths[warm_mam$values==0])}
          

            
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

