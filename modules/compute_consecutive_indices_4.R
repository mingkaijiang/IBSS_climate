## Actual computation of consecutive day indices
## based on 3 crops for each SCCS society

compute_consecutive_indices_4 <- function(s1.date, e1.date, s2.date, e2.date, 
                                          s3.date, e3.date, s4.date, e4.date,
                                          wea.station, sccs.id, source, dest) {
    # s1.date: starting doy for plant 1
    # e1.date: end doy for plant 1
    # s2.date
    # e2.date
    # s3.date
    # e3.date
    # s4.date
    # e4.date
    # wea.station: weather station id list
    # sccs.id: sccs id
    # dest: dest directory
    
    require(lubridate)
    
    in.list <- paste0(wea.station, ".csv")
    out.list1 <- paste0(wea.station, "_sccs", sccs.id, "_plant1.csv")
    out.list2 <- paste0(wea.station, "_sccs", sccs.id, "_plant2.csv")
    out.list3 <- paste0(wea.station, "_sccs", sccs.id, "_plant3.csv")
    out.list4 <- paste0(wea.station, "_sccs", sccs.id, "_plant4.csv")
    
    
    for (k in 1:length(wea.station)) 
    {
        # print the file to screen
        print(wea.station[k])
        
        # prepare input and output file names
        inName <- file.path(source, in.list[k], fsep = .Platform$file.sep)
        outName1 <- file.path(dest, out.list1[k], fsep = .Platform$file.sep)
        outName2 <- file.path(dest, out.list2[k], fsep = .Platform$file.sep)
        outName3 <- file.path(dest, out.list3[k], fsep = .Platform$file.sep)
        outName4 <- file.path(dest, out.list4[k], fsep = .Platform$file.sep)
        
        # read in file and prepare the df
        dd <- read.csv(inName)
        colnames(dd)<-c("id","year","month","day","prcp")
        dd$doy <- yday(dd$id)

        # check if the location is in southern hemisphere
        if (s1.date < e1.date) {
            # growing season starts before the ending date within a year
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            # growing period
            g.period <- e1.date - s1.date + 1
            b.period <- s1.date - 0
            a.period <- 366 - e1.date + 1
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s1.date)
                during_g <- subset(dd[dd$year == j,], doy >= s1.date & doy <= e1.date)
                after_g <- subset(dd[dd$year == j,],  doy > e1.date)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        } else {
            # southern hemisphere, need to take one year out (1st year)
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            outDF <- outDF[-1,]
            
            # growing period
            g.period <- 366 - s1.date + e1.date
            b.period <- s1.date - 181 
            a.period <- 181 - e1.date
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s1.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s1.date)
                d2 <- subset(dd[dd4year == j, ], doy <= e1.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e1.date & doy < 181)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
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
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            # growing period
            g.period <- e2.date - s2.date + 1
            b.period <- s2.date - 0
            a.period <- 366 - e2.date + 1
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s2.date)
                during_g <- subset(dd[dd$year == j,], doy >= s2.date & doy <= e2.date)
                after_g <- subset(dd[dd$year == j,],  doy > e2.date)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        } else {
            # southern hemisphere, need to take one year out (1st year)
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            outDF <- outDF[-1,]
            
            # growing period
            g.period <- 366 - s2.date + e2.date
            b.period <- s2.date - 181 
            a.period <- 181 - e2.date
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s2.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s2.date)
                d2 <- subset(dd[dd4year == j, ], doy <= e2.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e2.date & doy < 181)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        }
        
        outDF1 <- outDF[-1,]
        l <- dim(outDF1)[1]
        outDF2 <- outDF1[-l,]
        
        # write output
        write.csv(outDF2, outName2, row.names=F)
        
        # check if the location is in southern hemisphere
        if (s3.date < e3.date) {
            # growing season starts before the ending date within a year
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            # growing period
            g.period <- e3.date - s3.date + 1
            b.period <- s3.date - 0
            a.period <- 366 - e3.date + 1
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s3.date)
                during_g <- subset(dd[dd$year == j,], doy >= s3.date & doy <= e3.date)
                after_g <- subset(dd[dd$year == j,],  doy > e3.date)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        } else {
            # southern hemisphere, need to take one year out (1st year)
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            outDF <- outDF[-1,]
            
            # growing period
            g.period <- 366 - s3.date + e3.date
            b.period <- s3.date - 181 
            a.period <- 181 - e3.date
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s3.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s3.date)
                d2 <- subset(dd[dd4year == j, ], doy <= e3.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e3.date & doy < 181)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        }
        
        outDF1 <- outDF[-1,]
        l <- dim(outDF1)[1]
        outDF2 <- outDF1[-l,]
        
        # write output
        write.csv(outDF2, outName3, row.names=F)
        
        # check if the location is in southern hemisphere
        if (s4.date < e4.date) {
            # growing season starts before the ending date within a year
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            # growing period
            g.period <- e4.date - s4.date + 1
            b.period <- s4.date - 0
            a.period <- 366 - e4.date + 1
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == j,], doy < s4.date)
                during_g <- subset(dd[dd$year == j,], doy >= s4.date & doy <= e4.date)
                after_g <- subset(dd[dd$year == j,],  doy > e4.date)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        } else {
            # southern hemisphere, need to take one year out (1st year)
            
            # prepare output df
            outDF <- data.frame(unique(dd$year), NA, NA, NA, 
                                NA, NA, NA)
            colnames(outDF) <- c("year", "dry_before", "dry_growing", "dry_after",
                                 "wet_before", "wet_growing", "wet_after")
            
            outDF <- outDF[-1,]
            
            # growing period
            g.period <- 366 - s4.date + e4.date
            b.period <- s4.date - 181 
            a.period <- 181 - e4.date
            
            # count # days 
            for (j in outDF$year) {
                # extract the three periods
                before_g <- subset(dd[dd$year == (j - 1), ], doy < s4.date & doy >= 181)
                d1 <- subset(dd[dd$year == (j - 1), ], doy >= s4.date)
                d2 <- subset(dd[dd$year == j, ], doy <= e4.date)
                during_g <- rbind(d1, d2)
                after_g <- subset(dd[dd$year == j,],  doy > e4.date & doy < 181)
                
                # consecutive dry days in the three periods
                dry_before <- rle(before_g$prcp)
                dry_during <- rle(during_g$prcp)
                dry_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "dry_before"] <- max(dry_before$lengths[dry_before$values==0]) / b.period
                outDF[outDF$year == j, "dry_growing"] <- max(dry_during$lengths[dry_during$values==0]) / g.period
                outDF[outDF$year == j, "dry_after"] <- max(dry_after$lengths[dry_after$values==0]) / a.period
                
                # consecutive wet days in the three periods
                wet_before <- rle(before_g$prcp)
                wet_during <- rle(during_g$prcp)
                wet_after <- rle(after_g$prcp)
                
                outDF[outDF$year == j, "wet_before"] <- max(wet_before$lengths[wet_before$values>0]) / b.period
                outDF[outDF$year == j, "wet_growing"] <- max(wet_during$lengths[wet_during$values>0]) / g.period
                outDF[outDF$year == j, "wet_after"] <- max(wet_after$lengths[wet_after$values>0]) / a.period
            }
        }
        
        outDF1 <- outDF[-1,]
        l <- dim(outDF1)[1]
        outDF2 <- outDF1[-l,]
        
        # write output
        write.csv(outDF2, outName4, row.names=F)
    }
    print(paste0("finish k loop ", k))
    
}

