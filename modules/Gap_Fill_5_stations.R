##############################################################################################################
### Gap filling using the hyfo package as an easy solution
Gap_Fill_5_stations <- function(stationDF = STATION.DATAFRAME, 
                     sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)
    
    sDF <- matrix(ncol = 5, nrow = nrow(stationDF))
    sDF <- as.data.frame(sDF, stringsAsFactors=F)
    colnames(sDF) <- c("ghcn1", "ghcn2", "ghcn3",
                       "ghcn4", "ghcn5")
    
    sDF$ghcn1 <- stationDF$ghcn1
    sDF$ghcn2 <- stationDF$ghcn2
    sDF$ghcn3 <- stationDF$ghcn3
    sDF$ghcn4 <- stationDF$ghcn4
    sDF$ghcn5 <- stationDF$ghcn5
    
    c.count <- rowSums(!is.na(sDF))
    
    for (i in 1:length(c.count)) 
    {
        threshold <- c.count[i]
        
        targList <- paste0(sDF[i,"ghcn1"],".csv")
        supList2 <- paste0(sDF[i,"ghcn2"],".csv")
        supList3 <- paste0(sDF[i,"ghcn3"],".csv")
        supList4 <- paste0(sDF[i,"ghcn4"],".csv")
        supList5 <- paste0(sDF[i,"ghcn5"],".csv")

        
        ## read in 1st file
        inName <- file.path(sourceDir, targList, fsep = .Platform$file.sep)
        outName <- file.path(destDir, targList, fsep = .Platform$file.sep)
        
        ## read 2nd file
        inName2 <- file.path(sourceDir, supList2, fsep = .Platform$file.sep)
        outName2 <- file.path(destDir, supList2, fsep = .Platform$file.sep)
        
        
        if(file.exists(inName) && file.exists(inName2)) {
            X <- read.csv(inName)
            X$date <- as.Date(paste(X$Year, X$Month, X$Day, sep="-"),
                              format = "%Y-%m-%d")
            
            modDF <- data.frame(X$date, X$value)
            colnames(modDF) <- c("date", "s1")
            
            modDF[modDF$s1 == -99.9, "s1"] <- NA
            
            X2 <- read.csv(inName2)
            X2$date <- as.Date(paste(X2$Year, X2$Month, X2$Day, sep="-"),
                               format = "%Y-%m-%d")
            
            modDF2 <- data.frame(X2$date, X2$value)
            colnames(modDF2) <- c("date", "s2")
            
            modDF2[modDF2$s2 == -99.9, "s2"] <- NA
            
            
            if (threshold >= 3) {
                
                ## read 3rd file
                inName3 <- file.path(sourceDir, supList3, fsep = .Platform$file.sep)
                outName3 <- file.path(destDir, supList3, fsep = .Platform$file.sep)
                
                if(file.exists(inName3)) {
                    X3 <- read.csv(inName3)
                    X3$date <- as.Date(paste(X3$Year, X3$Month, X3$Day, sep="-"),
                                       format = "%Y-%m-%d")
                    
                    modDF3 <- data.frame(X3$date, X3$value)
                    colnames(modDF3) <- c("date", "s3")
                    
                    modDF3[modDF3$s3 == -99.9, "s3"] <- NA
                } 
                
                if (threshold >= 4) {
                    ## read 4th file
                    inName4 <- file.path(sourceDir, supList4, fsep = .Platform$file.sep)
                    outName4 <- file.path(destDir, supList4, fsep = .Platform$file.sep)
                    
                    if(file.exists(inName4)) {
                        X4 <- read.csv(inName4)
                        X4$date <- as.Date(paste(X4$Year, X4$Month, X4$Day, sep="-"),
                                           format = "%Y-%m-%d")
                        
                        modDF4 <- data.frame(X4$date, X4$value)
                        colnames(modDF4) <- c("date", "s4")
                        
                        modDF4[modDF4$s4 == -99.9, "s4"] <- NA
                    }
                    
                    if (threshold >= 5) {
                        
                        ## read in 5th file
                        inName5 <- file.path(sourceDir, supList5, fsep = .Platform$file.sep)
                        outName5 <- file.path(destDir, supList5, fsep = .Platform$file.sep)
                        
                        if(file.exists(inName5)) {
                            X5 <- read.csv(inName5)
                            X5$date <- as.Date(paste(X5$Year, X5$Month, X5$Day, sep="-"),
                                               format = "%Y-%m-%d")
                            
                            modDF5 <- data.frame(X5$date, X5$value)
                            colnames(modDF5) <- c("date", "s5")
                            
                            modDF5[modDF5$s5 == -99.9, "s5"] <- NA
                        } 
                        
                    }  # 5
                } # 4 
            } # 3
            
            
            if (threshold == 5) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                                 modDF4$date, modDF5$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                               modDF4$date, modDF5$date)
                
                # create new datamframe 
                t.series <- seq.Date(from = startDate, to = endDate,
                                     by = "day")
                testDF <- data.frame(t.series, NA)
                colnames(testDF) <- c("date", "value")
                
                out1 <- merge(testDF, modDF, by="date", all.x=T, sort=T)
                out2 <- merge(out1, modDF2, by="date", all.x=T, sort=T)
                out3 <- merge(out2, modDF3, by="date", all.x=T, sort=T)
                out4 <- merge(out3, modDF4, by="date", all.x=T, sort=T)
                out5 <- merge(out4, modDF5, by="date", all.x=T, sort=T)
                
                testDF <- out5[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5")
                
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:6]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:6]))
                
                # if >90% are NAs, simply repeat column 1 values 
                if(csum[[2]]/csum[[1]] <= 0.1) {
                    test4$s2 <- test4$s1
                }
                
                if(csum[[3]]/csum[[1]] <= 0.1) {
                    test4$s3 <- test4$s1
                }
                
                if(csum[[4]]/csum[[1]] <= 0.1) {
                    test4$s4 <- test4$s1
                }
                
                if(csum[[5]]/csum[[1]] <= 0.1) {
                    test4$s5 <- test4$s1
                }
                
                
            } else if (threshold == 4) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                                 modDF4$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                               modDF4$date)
                
                # create new datamframe 
                t.series <- seq.Date(from = startDate, to = endDate,
                                     by = "day")
                testDF <- data.frame(t.series, NA)
                colnames(testDF) <- c("date", "value")
                
                out1 <- merge(testDF, modDF, by="date", all.x=T, sort=T)
                out2 <- merge(out1, modDF2, by="date", all.x=T, sort=T)
                out3 <- merge(out2, modDF3, by="date", all.x=T, sort=T)
                out4 <- merge(out3, modDF4, by="date", all.x=T, sort=T)
                
                testDF <- out4[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3", "s4")
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:5]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:5]))
                
                # if >90% are NAs, simply repeat column 1 values 
                if(csum[[2]]/csum[[1]] <= 0.1) {
                    test4$s2 <- test4$s1
                }
                
                if(csum[[3]]/csum[[1]] <= 0.1) {
                    test4$s3 <- test4$s1
                }
                
                if(csum[[4]]/csum[[1]] <= 0.1) {
                    test4$s4 <- test4$s1
                }
                
                
            } else if (threshold == 3) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date)
                
                # create new datamframe 
                t.series <- seq.Date(from = startDate, to = endDate,
                                     by = "day")
                testDF <- data.frame(t.series, NA)
                colnames(testDF) <- c("date", "value")
                
                out1 <- merge(testDF, modDF, by="date", all.x=T, sort=T)
                out2 <- merge(out1, modDF2, by="date", all.x=T, sort=T)
                out3 <- merge(out2, modDF3, by="date", all.x=T, sort=T)
                
                testDF <- out3[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3")
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:4]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:4]))
                
                # if >90% are NAs, simply repeat column 1 values 
                if(csum[[2]]/csum[[1]] <= 0.1) {
                    test4$s2 <- test4$s1
                }
                
                if(csum[[3]]/csum[[1]] <= 0.1) {
                    test4$s3 <- test4$s1
                }
                
                
            } else {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date)
                
                # create new datamframe 
                t.series <- seq.Date(from = startDate, to = endDate,
                                     by = "day")
                testDF <- data.frame(t.series, NA)
                colnames(testDF) <- c("date", "value")
                
                out1 <- merge(testDF, modDF, by="date", all.x=T, sort=T)
                out2 <- merge(out1, modDF2, by="date", all.x=T, sort=T)
                
                testDF <- out2[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2")
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:3]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:3]))
                
                # if >90% are NAs, simply repeat column 1 values 
                if(csum[[2]]/csum[[1]] <= 0.1) {
                    test4$s2 <- test4$s1
                }
                
            }
            
            # gap fill the rest missing values
            test5 <- fillGap(test4, corPeriod="daily")
            colnames(test5)[1] <- "date"
            
            # re-create the dataframe over the entire period with no missing values
            t.series <- seq.Date(from = min(modDF$date), to = max(modDF$date),
                                 by = "day")
            outDF <- data.frame(t.series, NA)
            colnames(outDF) <- c("date", "value")
            c <- nrow(outDF)
            
            t1 <- data.frame(test5$date, test5$s1)
            colnames(t1) <- c("date", "s1")
            outDF$value[modDF$date %in% outDF$date] <- modDF$s1[modDF$date %in% outDF$date]
            outDF <- left_join(outDF, t1, by = "date")
            outDF$value[is.na(outDF$value)] <- outDF$s1[is.na(outDF$value)]
            
            
            
            t.series <- seq.Date(from = min(modDF2$date), to = max(modDF2$date),
                                 by = "day")
            outDF2 <- data.frame(t.series, NA)
            colnames(outDF2) <- c("date", "value")
            t2 <- data.frame(test5$date, test5$s2)
            colnames(t2) <- c("date", "s2")
            outDF2$value[modDF2$date %in% outDF2$date] <- modDF2$s2[modDF2$date %in% outDF2$date]
            outDF2 <- left_join(outDF2, t2, by = "date")
            outDF2$value[is.na(outDF2$value)] <- outDF2$s2[is.na(outDF2$value)]
            

            # assign values from output filled df
            
            outDF$Year <- as.numeric(format(outDF$date, "%Y"))
            outDF$Month <- as.numeric(format(outDF$date, "%m"))
            outDF$Day <- as.numeric(format(outDF$date, "%d"))
            
            outDF2$Year <- as.numeric(format(outDF2$date, "%Y"))
            outDF2$Month <- as.numeric(format(outDF2$date, "%m"))
            outDF2$Day <- as.numeric(format(outDF2$date, "%d"))
            
            outDF[is.na(outDF$value), "value"] <- -99.9
            outDF2[is.na(outDF2$value), "value"] <- -99.9
            
            outDF <- outDF[,c("date", "Year", "Month", "Day", "value")]
            colnames(outDF) <- c("date", "Year", "Month", "Day", "value")
            write.csv(outDF,outName, row.names=F)
            
            outDF2 <- outDF2[,c("date", "Year", "Month", "Day", "value")]
            colnames(outDF2) <- c("date", "Year", "Month", "Day", "value")
            write.csv(outDF2,outName2, row.names=F)
            
            
            if (threshold > 2) {
                t.series <- seq.Date(from = min(modDF3$date), to = max(modDF3$date),
                                     by = "day")
                outDF3 <- data.frame(t.series, NA)
                colnames(outDF3) <- c("date", "value")
                c <- nrow(outDF3)
                t3 <- data.frame(test5$date, test5$s3)
                colnames(t3) <- c("date", "s3")
                outDF3$value[modDF3$date %in% outDF3$date] <- modDF3$s3[modDF3$date %in% outDF3$date]
                outDF3 <- left_join(outDF3, t3, by = "date")
                outDF3$value[is.na(outDF3$value)] <- outDF3$s3[is.na(outDF3$value)]
                
                
                outDF3$Year <- as.numeric(format(outDF3$date, "%Y"))
                outDF3$Month <- as.numeric(format(outDF3$date, "%m"))
                outDF3$Day <- as.numeric(format(outDF3$date, "%d"))
                
                outDF3[is.na(outDF3$value), "value"] <- -99.9
                
                outDF3 <- outDF3[,c("date", "Year", "Month", "Day", "value")]
                colnames(outDF3) <- c("date", "Year", "Month", "Day", "value")
                write.csv(outDF3,outName3, row.names=F)
                
                if (threshold > 3) {
                    t.series <- seq.Date(from = min(modDF4$date), to = max(modDF4$date),
                                         by = "day")
                    outDF4 <- data.frame(t.series, NA)
                    colnames(outDF4) <- c("date", "value")
                    c <- nrow(outDF4)
                    t4 <- data.frame(test5$date, test5$s4)
                    colnames(t4) <- c("date", "s4")
                    outDF4$value[modDF4$date %in% outDF4$date] <- modDF4$s4[modDF4$date %in% outDF4$date]
                    outDF4 <- left_join(outDF4, t4, by = "date")
                    outDF4$value[is.na(outDF4$value)] <- outDF4$s4[is.na(outDF4$value)]
                    
                    
                    outDF4$Year <- as.numeric(format(outDF4$date, "%Y"))
                    outDF4$Month <- as.numeric(format(outDF4$date, "%m"))
                    outDF4$Day <- as.numeric(format(outDF4$date, "%d"))
                    
                    outDF4[is.na(outDF4$value), "value"] <- -99.9
                    
                    outDF4 <- outDF4[,c("date", "Year", "Month", "Day", "value")]
                    colnames(outDF4) <- c("date", "Year", "Month", "Day", "value")
                    write.csv(outDF4,outName4, row.names=F)
                    
                    if (threshold > 4) {
                        t.series <- seq.Date(from = min(modDF5$date), to = max(modDF5$date),
                                             by = "day")
                        outDF5 <- data.frame(t.series, NA)
                        colnames(outDF5) <- c("date", "value")
                        c <- nrow(outDF5)
                        t5 <- data.frame(test5$date, test5$s5)
                        colnames(t5) <- c("date", "s5")
                        outDF5$value[modDF5$date %in% outDF5$date] <- modDF5$s5[modDF5$date %in% outDF5$date]
                        outDF5 <- left_join(outDF5, t5, by = "date")
                        outDF5$value[is.na(outDF5$value)] <- outDF5$s5[is.na(outDF5$value)]
                        
                        outDF5$Year <- as.numeric(format(outDF5$date, "%Y"))
                        outDF5$Month <- as.numeric(format(outDF5$date, "%m"))
                        outDF5$Day <- as.numeric(format(outDF5$date, "%d"))
                        
                        outDF5[is.na(outDF5$value), "value"] <- -99.9
                        
                        outDF5 <- outDF5[,c("date", "Year", "Month", "Day", "value")]
                        colnames(outDF5) <- c("date", "Year", "Month", "Day", "value")
                        write.csv(outDF5,outName5, row.names=F)
                        
                        
                        
                    } # 4
                } # 3
            } # 2
        } # checking if file exists
        
        print(paste0(i, " ------", targList))
        
    } # looping through list
    
}
