## Actual computation of consecutive day indices
## based on 1 growing season for each SCCS society

compute_consecutive_indices_1_tmax <- function(s.date, e.date, wea.station, sccs.id, source, dest) {
    # s.date: starting doy for plant 1
    # e.date: end doy for plant 1
    # wea.station: weather station id list
    # sccs.id: sccs id
    # dest: dest directory
    
    require(lubridate)
    
    in.list <- paste0(wea.station, ".csv")
    out.list <- paste0(wea.station, "_sccs", sccs.id, "_plant1.csv")
    
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

        # check if the location is in southern hemisphere
        if (s.date < e.date) {
            # growing season starts before the ending date within a year
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA)
            colnames(outDF) <- c("year", "warm_before", "warm_growing", "warm_after")
            
            # growing period
            g.period <- e.date - s.date + 1
            b.period <- s.date - 0
            a.period <- 366 - e.date + 1
            
            before_g <- subset(dd, doy < s.date)
            during_g <- subset(dd, doy >= s.date & doy <= e.date)
            after_g <- subset(dd,  doy > e.date)
            
            tmax10before<-quantile(before_g$tmax,0.9)/10.0
            tmax10during<-quantile(during_g$tmax,0.9)/10.0
            tmax10after<-quantile(after_g$tmax,0.9)/10.0
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s.date)
                during_g <- subset(dd[dd$year == j,], doy >= s.date & doy <= e.date)
                after_g <- subset(dd[dd$year == j,],  doy > e.date)
                
                
                
                # consecutive warm days in the three periods
                warm_before <- rle(ifelse((before_g$tmax/10.0 - tmax10before) > 0.0, 0, 1))
                warm_during <- rle(ifelse((during_g$tmax/10.0 - tmax10during) > 0.0, 0, 1))
                warm_after <- rle(ifelse((after_g$tmax/10.0 - tmax10after) > 0.0, 0, 1))
                
                outDF[outDF$year == j, "warm_before"] <- max(warm_before$lengths[warm_before$values==0]) / b.period
                outDF[outDF$year == j, "warm_growing"] <- max(warm_during$lengths[warm_during$values==0]) / g.period
                outDF[outDF$year == j, "warm_after"] <- max(warm_after$lengths[warm_after$values==0]) / a.period
                
            }
        } else {
            # southern hemisphere, need to take one year out (1st year)
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA)
            colnames(outDF) <- c("year", "warm_before", "warm_growing", "warm_after")
            
            outDF <- outDF[-1,]
            
            # growing period
            g.period <- 366 - s.date + e.date
            b.period <- s.date - 181 
            a.period <- 181 - e.date
            
            before_g <- subset(dd, doy < s.date & doy >= 181)
            d1 <- subset(dd, doy >= s.date)
            d2 <- subset(dd, doy <= e.date)
            during_g <- rbind(d1, d2)
            after_g <- subset(dd,  doy > e.date & doy < 181)
            
            tmax10before<-quantile(before_g$tmax,0.9)/10.0
            tmax10during<-quantile(during_g$tmax,0.9)/10.0
            tmax10after<-quantile(after_g$tmax,0.9)/10.0
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s.date)
                d2 <- subset(dd[dd$year == j, ], doy <= e.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e.date & doy < 181)
                
 
                
                # consecutive warm days in the three periods
                warm_before <- rle(ifelse((before_g$tmax/10.0 - tmax10before) > 0.0, 0, 1))
                warm_during <- rle(ifelse((during_g$tmax/10.0 - tmax10during) > 0.0, 0, 1))
                warm_after <- rle(ifelse((after_g$tmax/10.0 - tmax10after) > 0.0, 0, 1))
                
                outDF[outDF$year == j, "warm_before"] <- max(warm_before$lengths[warm_before$values==0]) / b.period
                outDF[outDF$year == j, "warm_growing"] <- max(warm_during$lengths[warm_during$values==0]) / g.period
                outDF[outDF$year == j, "warm_after"] <- max(warm_after$lengths[warm_after$values==0]) / a.period
                

            }
        }
        
        outDF1 <- outDF[-1,]
        l <- dim(outDF1)[1]
        outDF2 <- outDF1[-l,]
        
        # write output
        write.csv(outDF2, outName, row.names=F)
    }
    print(paste0("finish k loop ", k))
    
}

