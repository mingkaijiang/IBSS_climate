##############################################################################################################
### Gap filling using the hyfo package as an easy solution
Gap_Fill <- function(stationDF = STATION.DATAFRAME, 
                     sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)
    
    sDF <- matrix(ncol = 9, nrow = nrow(stationDF))
    sDF <- as.data.frame(sDF, stringsAsFactors=F)
    colnames(sDF) <- c("ghcn1", "ghcn2", "ghcn3",
                       "ghcn4", "ghcn5", "ghcn6",
                       "ghcn7", "ghcn8", "ghcn9")
    
    sDF$ghcn1 <- stationDF$ghcn1
    sDF$ghcn2 <- stationDF$ghcn2
    sDF$ghcn3 <- stationDF$ghcn3
    sDF$ghcn4 <- stationDF$ghcn4
    sDF$ghcn5 <- stationDF$ghcn5
    sDF$ghcn6 <- stationDF$ghcn6
    sDF$ghcn7 <- stationDF$ghcn7
    sDF$ghcn8 <- stationDF$ghcn8
    sDF$ghcn9 <- stationDF$ghcn9
    
    c.count <- rowSums(!is.na(sDF))
    
    for (i in 1:length(c.count)) 
    {
        threshold <- c.count[i]
        
        targList <- paste0(sDF[i,"ghcn1"],".csv")
        supList2 <- paste0(sDF[i,"ghcn2"],".csv")
        supList3 <- paste0(sDF[i,"ghcn3"],".csv")
        supList4 <- paste0(sDF[i,"ghcn4"],".csv")
        supList5 <- paste0(sDF[i,"ghcn5"],".csv")
        supList6 <- paste0(sDF[i,"ghcn6"],".csv")
        supList7 <- paste0(sDF[i,"ghcn7"],".csv")
        supList8 <- paste0(sDF[i,"ghcn8"],".csv")
        supList9 <- paste0(sDF[i,"ghcn9"],".csv")
        
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
                        
                        if (threshold >= 6) {
                            ## read in 6th file
                            inName6 <- file.path(sourceDir, supList6, fsep = .Platform$file.sep)
                            outName6 <- file.path(destDir, supList6, fsep = .Platform$file.sep)
                            
                            if(file.exists(inName6)) {
                                X6 <- read.csv(inName6)
                                X6$date <- as.Date(paste(X6$Year, X6$Month, X6$Day, sep="-"),
                                                   format = "%Y-%m-%d")
                                
                                modDF6 <- data.frame(X6$date, X6$value)
                                colnames(modDF6) <- c("date", "s6")
                                
                                modDF6[modDF6$s6 == -99.9, "s6"] <- NA
                            } 
                            if (threshold >= 7) {
                                ## read in 7th file
                                inName7 <- file.path(sourceDir, supList7, fsep = .Platform$file.sep)
                                outName7 <- file.path(destDir, supList7, fsep = .Platform$file.sep)
                                
                                if(file.exists(inName7)) {
                                    X7 <- read.csv(inName7)
                                    X7$date <- as.Date(paste(X7$Year, X7$Month, X7$Day, sep="-"),
                                                       format = "%Y-%m-%d")
                                    
                                    modDF7 <- data.frame(X7$date, X7$value)
                                    colnames(modDF7) <- c("date", "s7")
                                    
                                    modDF7[modDF7$s7 == -99.9, "s7"] <- NA
                                }
                                
                                if (threshold >= 8) {
                                    ## read in 8th file
                                    inName8 <- file.path(sourceDir, supList8, fsep = .Platform$file.sep)
                                    outName8 <- file.path(destDir, supList8, fsep = .Platform$file.sep)
                                    
                                    if(file.exists(inName8)) {
                                        X8 <- read.csv(inName8)
                                        X8$date <- as.Date(paste(X8$Year, X8$Month, X8$Day, sep="-"),
                                                           format = "%Y-%m-%d")
                                        
                                        modDF8 <- data.frame(X8$date, X8$value)
                                        colnames(modDF8) <- c("date", "s8")
                                        
                                        modDF8[modDF8$s8 == -99.9, "s8"] <- NA
                                    } 
                                    
                                    if (threshold == 9) {
                                        ## read in 8th file
                                        inName9 <- file.path(sourceDir, supList9, fsep = .Platform$file.sep)
                                        outName9 <- file.path(destDir, supList9, fsep = .Platform$file.sep)
                                        
                                        if(file.exists(inName9)) {
                                            X9 <- read.csv(inName9)
                                            X9$date <- as.Date(paste(X9$Year, X9$Month, X9$Day, sep="-"),
                                                               format = "%Y-%m-%d")
                                            
                                            modDF9 <- data.frame(X9$date, X9$value)
                                            colnames(modDF9) <- c("date", "s9")
                                            
                                            modDF9[modDF9$s9 == -99.9, "s9"] <- NA
                                        } 
                                    } # 9
                                } # 8
                            } # 7
                        } # 6
                    }  # 5
                } # 4 
            } # 3
            
            if (threshold  == 9) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                                 modDF4$date, modDF5$date, modDF6$date, 
                                 modDF7$date, modDF8$date, modDF9$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                               modDF4$date, modDF5$date, modDF6$date, 
                               modDF7$date, modDF8$date, modDF9$date)
                
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
                out6 <- merge(out5, modDF6, by="date", all.x=T, sort=T)
                out7 <- merge(out6, modDF7, by="date", all.x=T, sort=T)
                out8 <- merge(out7, modDF8, by="date", all.x=T, sort=T)
                out9 <- merge(out8, modDF9, by="date", all.x=T, sort=T)
                
                testDF <- out9[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                      "s6", "s7", "s8", "s9")
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:10]))<threshold, ]  # as long as less than 9 missing values, you are fine
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:10]))
                
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
                
                if(csum[[6]]/csum[[1]] <= 0.1) {
                    test4$s6 <- test4$s1
                }
                
                if(csum[[7]]/csum[[1]] <= 0.1) {
                    test4$s7 <- test4$s1
                }
                
                if(csum[[8]]/csum[[1]] <= 0.1) {
                    test4$s8 <- test4$s1
                }
                
                if(csum[[9]]/csum[[1]] <= 0.1) {
                    test4$s9 <- test4$s1
                }
                
            } else if (threshold == 8) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                                 modDF4$date, modDF5$date, modDF6$date, 
                                 modDF7$date, modDF8$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                               modDF4$date, modDF5$date, modDF6$date, 
                               modDF7$date, modDF8$date)
                
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
                out6 <- merge(out5, modDF6, by="date", all.x=T, sort=T)
                out7 <- merge(out6, modDF7, by="date", all.x=T, sort=T)
                out8 <- merge(out7, modDF8, by="date", all.x=T, sort=T)
                
                testDF <- out8[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                      "s6", "s7", "s8")
                
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:9]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:9]))
                
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
                
                if(csum[[6]]/csum[[1]] <= 0.1) {
                    test4$s6 <- test4$s1
                }
                
                if(csum[[7]]/csum[[1]] <= 0.1) {
                    test4$s7 <- test4$s1
                }
                
                if(csum[[8]]/csum[[1]] <= 0.1) {
                    test4$s8 <- test4$s1
                }
                
            } else if (threshold == 7) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                                 modDF4$date, modDF5$date, modDF6$date, 
                                 modDF7$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                               modDF4$date, modDF5$date, modDF6$date, 
                               modDF7$date)
                
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
                out6 <- merge(out5, modDF6, by="date", all.x=T, sort=T)
                out7 <- merge(out6, modDF7, by="date", all.x=T, sort=T)
                
                testDF <- out7[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                      "s6", "s7")
                
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:8]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:8]))
                
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
                
                if(csum[[6]]/csum[[1]] <= 0.1) {
                    test4$s6 <- test4$s1
                }
                
                if(csum[[7]]/csum[[1]] <= 0.1) {
                    test4$s7 <- test4$s1
                }
                
                
            } else if (threshold == 6) {
                # Find minimum start date
                startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                                 modDF4$date, modDF5$date, modDF6$date)
                
                # find maximum end date
                endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                               modDF4$date, modDF5$date, modDF6$date)
                
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
                out6 <- merge(out5, modDF6, by="date", all.x=T, sort=T)
                
                testDF <- out6[,-2]
                
                colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                      "s6")
                
                # delete row entries where the entire row are full with NAs
                test2 <- testDF[rowSums(is.na(testDF[,2:7]))<threshold, ]  
                
                test3 <- subset(test2, date <= max(modDF$date))
                test4 <- subset(test3, date >= min(modDF$date))
                
                # check column sums to ensure there's still data left for each ghcn station
                csum <- colSums(!is.na(test4[,2:7]))
                
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
                
                if(csum[[6]]/csum[[1]] <= 0.1) {
                    test4$s6 <- test4$s1
                }
                
                
                
            } else if (threshold == 5) {
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
                        
                        if (threshold > 5) {
                            t.series <- seq.Date(from = min(modDF6$date), to = max(modDF6$date),
                                                 by = "day")
                            outDF6 <- data.frame(t.series, NA)
                            colnames(outDF6) <- c("date", "value")
                            c <- nrow(outDF6)
                            t6 <- data.frame(test5$date, test5$s6)
                            colnames(t6) <- c("date", "s6")
                            outDF6$value[modDF6$date %in% outDF6$date] <- modDF6$s6[modDF6$date %in% outDF6$date]
                            outDF6 <- left_join(outDF6, t6, by = "date")
                            outDF6$value[is.na(outDF6$value)] <- outDF6$s6[is.na(outDF6$value)]
                            
                            outDF6$Year <- as.numeric(format(outDF6$date, "%Y"))
                            outDF6$Month <- as.numeric(format(outDF6$date, "%m"))
                            outDF6$Day <- as.numeric(format(outDF6$date, "%d"))
                            
                            outDF6[is.na(outDF6$value), "value"] <- -99.9
                            
                            outDF6 <- outDF6[,c("date", "Year", "Month", "Day", "value")]
                            colnames(outDF6) <- c("date", "Year", "Month", "Day", "value")
                            write.csv(outDF6,outName6, row.names=F)
                            
                            if (threshold > 6) {
                                t.series <- seq.Date(from = min(modDF7$date), to = max(modDF7$date),
                                                     by = "day")
                                outDF7 <- data.frame(t.series, NA)
                                colnames(outDF7) <- c("date", "value")
                                c <- nrow(outDF7)
                                t7 <- data.frame(test5$date, test5$s7)
                                colnames(t7) <- c("date", "s7")
                                outDF7$value[modDF7$date %in% outDF7$date] <- modDF7$s7[modDF7$date %in% outDF7$date]
                                outDF7 <- left_join(outDF7, t7, by = "date")
                                outDF7$value[is.na(outDF7$value)] <- outDF7$s7[is.na(outDF7$value)]
                                
                                outDF7$Year <- as.numeric(format(outDF7$date, "%Y"))
                                outDF7$Month <- as.numeric(format(outDF7$date, "%m"))
                                outDF7$Day <- as.numeric(format(outDF7$date, "%d"))
                                
                                outDF7[is.na(outDF7$value), "value"] <- -99.9
                                
                                outDF7 <- outDF7[,c("date", "Year", "Month", "Day", "value")]
                                colnames(outDF7) <- c("date", "Year", "Month", "Day", "value")
                                write.csv(outDF7,outName7, row.names=F)
                                
                                if (threshold > 7) {
                                    t.series <- seq.Date(from = min(modDF8$date), to = max(modDF8$date),
                                                         by = "day")
                                    outDF8 <- data.frame(t.series, NA)
                                    colnames(outDF8) <- c("date", "value")
                                    c <- nrow(outDF8)
                                    t8 <- data.frame(test5$date, test5$s8)
                                    colnames(t8) <- c("date", "s8")
                                    outDF8$value[modDF8$date %in% outDF8$date] <- modDF8$s8[modDF8$date %in% outDF8$date]
                                    outDF8 <- left_join(outDF8, t8, by = "date")
                                    outDF8$value[is.na(outDF8$value)] <- outDF8$s8[is.na(outDF8$value)]
                                    
                                    outDF8$Year <- as.numeric(format(outDF8$date, "%Y"))
                                    outDF8$Month <- as.numeric(format(outDF8$date, "%m"))
                                    outDF8$Day <- as.numeric(format(outDF8$date, "%d"))
                                    
                                    outDF8[is.na(outDF8$value), "value"] <- -99.9
                                    
                                    outDF8 <- outDF8[,c("date", "Year", "Month", "Day", "value")]
                                    colnames(outDF8) <- c("date", "Year", "Month", "Day", "value")
                                    write.csv(outDF8,outName8, row.names=F)
                                    
                                    if (threshold > 8) {
                                        t.series <- seq.Date(from = min(modDF9$date), to = max(modDF9$date),
                                                             by = "day")
                                        outDF9 <- data.frame(t.series, NA)
                                        colnames(outDF9) <- c("date", "value")
                                        c <- nrow(outDF9)
                                        t9 <- data.frame(test5$date, test5$s9)
                                        colnames(t9) <- c("date", "s9")
                                        outDF9$value[modDF9$date %in% outDF9$date] <- modDF9$s9[modDF9$date %in% outDF9$date]
                                        outDF9 <- left_join(outDF9, t9, by = "date")
                                        outDF9$value[is.na(outDF9$value)] <- outDF9$s9[is.na(outDF9$value)]
                                        
                                        outDF9$Year <- as.numeric(format(outDF9$date, "%Y"))
                                        outDF9$Month <- as.numeric(format(outDF9$date, "%m"))
                                        outDF9$Day <- as.numeric(format(outDF9$date, "%d"))
                                        
                                        outDF9[is.na(outDF9$value), "value"] <- -99.9
                                        
                                        outDF9 <- outDF9[,c("date", "Year", "Month", "Day", "value")]
                                        colnames(outDF9) <- c("date", "Year", "Month", "Day", "value")
                                        write.csv(outDF9,outName9, row.names=F)
                                    } # 8
                                } # 7
                            } # 6
                        } # 5
                    } # 4
                } # 3
            } # 2
        } # checking if file exists
        
        print(paste0(i, " ------", targList))
        
    } # looping through list
    
}
