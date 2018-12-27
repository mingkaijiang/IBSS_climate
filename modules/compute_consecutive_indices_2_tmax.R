## Actual computation of consecutive day indices
## based on 2 crops for each SCCS society

compute_consecutive_indices_2_tmax <- function(s1.date, e1.date, s2.date, e2.date, wea.station, sccs.id, source, dest) {
    # s1.date: starting doy for plant 1
    # e1.date: end doy for plant 1
    # s2.date
    # e2.date
    # wea.station: weather station id list
    # sccs.id: sccs id
    # dest: dest directory
    
    require(lubridate)
    
    in.list <- paste0(wea.station, ".csv")
    out.list1 <- paste0(wea.station, "_sccs", sccs.id, "_plant1.csv")
    out.list2 <- paste0(wea.station, "_sccs", sccs.id, "_plant2.csv")
    
    
    for (k in 1:length(wea.station)) 
    {
        # print the file to screen
        print(wea.station[k])
        
        # prepare input and output file names
        inName <- file.path(source, in.list[k], fsep = .Platform$file.sep)
        outName1 <- file.path(dest, out.list1[k], fsep = .Platform$file.sep)
        outName2 <- file.path(dest, out.list2[k], fsep = .Platform$file.sep)
        
        # read in file and prepare the df
        dd <- read.csv(inName)
        colnames(dd)<-c("id","year","month","day","tmax")
        dd$doy <- yday(dd$id)

        # check if the location is in southern hemisphere
        if (s1.date < e1.date) {
            # growing season starts before the ending date within a year
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA)
            colnames(outDF) <- c("year", "warm_before", "warm_growing", "warm_after")
            
            # growing period
            g.period <- e1.date - s1.date + 1
            b.period <- s1.date - 0
            a.period <- 366 - e1.date + 1
            
            before_g <- subset(dd, doy < s1.date)
            during_g <- subset(dd, doy >= s1.date & doy <= e1.date)
            after_g <- subset(dd,  doy > e1.date)
            
            tmax10before<-quantile(before_g$tmax,0.9)/10.0
            tmax10during<-quantile(during_g$tmax,0.9)/10.0
            tmax10after<-quantile(after_g$tmax,0.9)/10.0
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s1.date)
                during_g <- subset(dd[dd$year == j,], doy >= s1.date & doy <= e1.date)
                after_g <- subset(dd[dd$year == j,],  doy > e1.date)
                
                
                
                # consecutive warm days in the three periods
                warm_before <- rle(ifelse((before_g$tmax/10.0 - tmax10before) > 0.0, 0, 1))
                warm_during <- rle(ifelse((during_g$tmax/10.0 - tmax10during) > 0.0, 0, 1))
                warm_after <- rle(ifelse((after_g$tmax/10.0 - tmax10after) > 0.0, 0, 1))
                
                outDF[outDF$year == j, "warm_before"] <- max(warm_before$lengths[warm_before$values==0]) /b.period
                outDF[outDF$year == j, "warm_growing"] <- max(warm_during$lengths[warm_during$values==0])/ g.period
                outDF[outDF$year == j, "warm_after"] <- max(warm_after$lengths[warm_after$values==0])/ a.period

            }
        } else {
            # southern hemisphere, need to take one year out (1st year)
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA)
            colnames(outDF) <- c("year", "warm_before", "warm_growing", "warm_after")
            
            outDF <- outDF[-1,]
            
            # growing period
            g.period <- 366 - s1.date + e1.date
            b.period <- s1.date - 181 
            a.period <- 181 - e1.date
            
            before_g <- subset(dd, doy < s1.date & doy >= 181)
            d1 <- subset(dd, doy >= s1.date)
            d2 <- subset(dd, doy <= e1.date)
            during_g <- rbind(d1, d2)
            after_g <- subset(dd,  doy > e1.date & doy < 181)
            
            tmax10before<-quantile(before_g$tmax,0.9)/10.0
            tmax10during<-quantile(during_g$tmax,0.9)/10.0
            tmax10after<-quantile(after_g$tmax,0.9)/10.0
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s1.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s1.date)
                d2 <- subset(dd[dd$year == j, ], doy <= e1.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e1.date & doy < 181)
                
                
                
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
        write.csv(outDF2, outName1, row.names=F)
        
        # check if the location is in southern hemisphere
        if (s2.date < e2.date) {
            # growing season starts before the ending date within a year
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA)
            colnames(outDF) <- c("year", "warm_before", "warm_growing", "warm_after")
            
            # growing period
            g.period <- e2.date - s2.date + 1
            b.period <- s2.date - 0
            a.period <- 366 - e2.date + 1
            
            before_g <- subset(dd, doy < s2.date)
            during_g <- subset(dd, doy >= s2.date & doy <= e2.date)
            after_g <- subset(dd,  doy > e2.date)
            
            tmax10before<-quantile(before_g$tmax,0.9)/10.0
            tmax10during<-quantile(during_g$tmax,0.9)/10.0
            tmax10after<-quantile(after_g$tmax,0.9)/10.0
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s2.date)
                during_g <- subset(dd[dd$year == j,], doy >= s2.date & doy <= e2.date)
                after_g <- subset(dd[dd$year == j,],  doy > e2.date)
                
                
                
                # consecutive warm days in the three periods
                warm_before <- rle(ifelse((before_g$tmax/10.0 - tmax10before) > 0.0, 0, 1))
                warm_during <- rle(ifelse((during_g$tmax/10.0 - tmax10during) > 0.0, 0, 1))
                warm_after <- rle(ifelse((after_g$tmax/10.0 - tmax10after) > 0.0, 0, 1))
                
                outDF[outDF$year == j, "warm_before"] <- max(warm_before$lengths[warm_before$values==0]) /b.period
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
            g.period <- 366 - s2.date + e2.date
            b.period <- s2.date - 181 
            a.period <- 181 - e2.date
            
            before_g <- subset(dd, doy < s2.date & doy >= 181)
            d1 <- subset(dd, doy >= s2.date)
            d2 <- subset(dd, doy <= e2.date)
            during_g <- rbind(d1, d2)
            after_g <- subset(dd,  doy > e2.date & doy < 181)
            
            tmax10before<-quantile(before_g$tmax,0.9)/10.0
            tmax10during<-quantile(during_g$tmax,0.9)/10.0
            tmax10after<-quantile(after_g$tmax,0.9)/10.0
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s2.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s2.date)
                d2 <- subset(dd[dd$year == j, ], doy <= e2.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e2.date & doy < 181)
                
                
                
                # consecutive warm days in the three periods
                warm_before <- rle(ifelse((before_g$tmax/10.0 - tmax10before) > 0.0, 0, 1))
                warm_during <- rle(ifelse((during_g$tmax/10.0 - tmax10during) > 0.0, 0, 1))
                warm_after <- rle(ifelse((after_g$tmax/10.0 - tmax10after) > 0.0, 0, 1))
                
                outDF[outDF$year == j, "warm_before"] <- max(warm_before$lengths[warm_before$values==0]) / b.period
                outDF[outDF$year == j, "warm_growing"] <- max(warm_during$lengths[warm_during$values==0]) / g.period
                outDF[outDF$year == j, "warm_after"] <- max(warm_after$lengths[warm_after$values==0]) /a.period
                
            }
        }
        
        outDF1 <- outDF[-1,]
        l <- dim(outDF1)[1]
        outDF2 <- outDF1[-l,]
        
        # write output
        write.csv(outDF2, outName2, row.names=F)
    }
    print(paste0("finish k loop ", k))
    
}

