##############################################################################################################
### Gap filling using the hyfo package as an easy solution
Gap_Fill_3 <- function(stationDF = STATION.DATAFRAME, threshold, 
                       sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)
    
    targList <- paste0(stationDF$ghcn3,".csv")
    supList2 <- paste0(stationDF$ghcn4,".csv")
    supList3 <- paste0(stationDF$ghcn5,".csv")
    supList4 <- paste0(stationDF$ghcn6,".csv")
    supList5 <- paste0(stationDF$ghcn7,".csv")
    supList6 <- paste0(stationDF$ghcn8,".csv")
    supList7 <- paste0(stationDF$ghcn9,".csv")
    
    for (i in 1:length(targList)) 
    {
        ## read in 1st file
        inName <- file.path(sourceDir, targList[i], fsep = .Platform$file.sep)
        outName <- file.path(destDir, targList[i], fsep = .Platform$file.sep)
        
        if(file.exists(inName)) {
            X <- read.csv(inName)
            X$date <- as.Date(paste(X$Year, X$Month, X$Day, sep="-"),
                              format = "%Y-%m-%d")
            
            modDF <- data.frame(X$date, X$value)
            colnames(modDF) <- c("date", "value")
            
            modDF[modDF$value == -99.9, "value"] <- NA
        } else {
            modDF <- NA
        }
        
        
        ## read 2nd file
        inName2 <- file.path(sourceDir, supList2[i], fsep = .Platform$file.sep)
        outName2 <- file.path(destDir, supList2[i], fsep = .Platform$file.sep)
        
        if(file.exists(inName2)) {
            X2 <- read.csv(inName2)
            X2$date <- as.Date(paste(X2$Year, X2$Month, X2$Day, sep="-"),
                               format = "%Y-%m-%d")
            
            modDF2 <- data.frame(X2$date, X2$value)
            colnames(modDF2) <- c("date", "value")
            
            modDF2[modDF2$value == -99.9, "value"] <- NA
        } else { 
            modDF2 <- modDF
        }
        
        if (threshold >= 2) {
            
            ## read 3rd file
            inName3 <- file.path(sourceDir, supList3[i], fsep = .Platform$file.sep)
            outName3 <- file.path(destDir, supList3[i], fsep = .Platform$file.sep)
            
            if(file.exists(inName3)) {
                X3 <- read.csv(inName3)
                X3$date <- as.Date(paste(X3$Year, X3$Month, X3$Day, sep="-"),
                                   format = "%Y-%m-%d")
                
                modDF3 <- data.frame(X3$date, X3$value)
                colnames(modDF3) <- c("date", "value")
                
                modDF3[modDF3$value == -99.9, "value"] <- NA
            } else { 
                modDF3 <- modDF 
            }
        } else {
            modDF3 <- modDF
        } 
        
        if (threshold >= 3) {
            ## read 4th file
            inName4 <- file.path(sourceDir, supList4[i], fsep = .Platform$file.sep)
            outName4 <- file.path(destDir, supList4[i], fsep = .Platform$file.sep)
            
            if(file.exists(inName4)) {
                X4 <- read.csv(inName4)
                X4$date <- as.Date(paste(X4$Year, X4$Month, X4$Day, sep="-"),
                                   format = "%Y-%m-%d")
                
                modDF4 <- data.frame(X4$date, X4$value)
                colnames(modDF4) <- c("date", "value")
                
                modDF4[modDF4$value == -99.9, "value"] <- NA
            } else { modDF4 <- modDF }
        } else {
            modDF4 <- modDF
        }
        
        if (threshold >= 4) {
            
            ## read in 5th file
            inName5 <- file.path(sourceDir, supList5[i], fsep = .Platform$file.sep)
            outName5 <- file.path(destDir, supList5[i], fsep = .Platform$file.sep)
            
            if(file.exists(inName5)) {
                X5 <- read.csv(inName5)
                X5$date <- as.Date(paste(X5$Year, X5$Month, X5$Day, sep="-"),
                                   format = "%Y-%m-%d")
                
                modDF5 <- data.frame(X5$date, X5$value)
                colnames(modDF5) <- c("date", "value")
                
                modDF5[modDF5$value == -99.9, "value"] <- NA
            } else { modDF5 <- modDF }
        } else {
            modDF5 <- modDF
        }
        
        if (threshold >= 5) {
            ## read in 6th file
            inName6 <- file.path(sourceDir, supList6[i], fsep = .Platform$file.sep)
            outName6 <- file.path(destDir, supList6[i], fsep = .Platform$file.sep)
            
            if(file.exists(inName6)) {
                X6 <- read.csv(inName6)
                X6$date <- as.Date(paste(X6$Year, X6$Month, X6$Day, sep="-"),
                                   format = "%Y-%m-%d")
                
                modDF6 <- data.frame(X6$date, X6$value)
                colnames(modDF6) <- c("date", "value")
                
                modDF6[modDF6$value == -99.9, "value"] <- NA
            } else { modDF6 <- modDF }
        } else {
            modDF6 <- modDF
        }
        
        if (threshold >= 6) {
            ## read in 7th file
            inName7 <- file.path(sourceDir, supList7[i], fsep = .Platform$file.sep)
            outName7 <- file.path(destDir, supList7[i], fsep = .Platform$file.sep)
            
            if(file.exists(inName7)) {
                X7 <- read.csv(inName7)
                X7$date <- as.Date(paste(X7$Year, X7$Month, X7$Day, sep="-"),
                                   format = "%Y-%m-%d")
                
                modDF7 <- data.frame(X7$date, X7$value)
                colnames(modDF7) <- c("date", "value")
                
                modDF7[modDF7$value == -99.9, "value"] <- NA
            } else { modDF7 <- modDF }
        } else {
            modDF7 <- modDF
        }
        
        
        
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
        testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA)
        colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                              "s6", "s7")
        
        # assign station values onto the dataframe
        for (j in modDF$date) {
            testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
        }
        
        for (j in modDF2$date) {
            testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
        }
        
        for (j in modDF3$date) {
            testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
        }
        
        for (j in modDF4$date) {
            testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
        }
        
        for (j in modDF5$date) {
            testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
        }
        
        for (j in modDF6$date) {
            testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
        }
        
        for (j in modDF7$date) {
            testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
        }
        
        
        if (threshold >= 6) {
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
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                  "s6", "s7")
            
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            for (j in modDF6$date) {
                testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
            }
            
            for (j in modDF7$date) {
                testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:8]))<=threshold, ]  
            
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
            
            
        } else if (threshold >= 5) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date, modDF6$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date, modDF6$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                  "s6")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            for (j in modDF6$date) {
                testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:7]))<=threshold, ]  
            
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
            
            
            
        } else if (threshold >= 4) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:6]))<=threshold, ]  
            
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
            
            
        } else if (threshold >= 3) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:5]))<=threshold, ]  
            
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
            
            
        } else if (threshold >= 2) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:4]))<=threshold, ]  
            
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
            testDF <- data.frame(t.series, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:3]))<=threshold, ]  
            
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
        
        # re-create the dataframe over the entire period with no missing values
        t.series <- seq.Date(from = min(modDF$date), to = max(modDF$date),
                             by = "day")
        outDF <- data.frame(t.series, NA)
        colnames(outDF) <- c("date", "value")
        
        t.series <- seq.Date(from = min(modDF2$date), to = max(modDF2$date),
                             by = "day")
        outDF2 <- data.frame(t.series, NA)
        colnames(outDF2) <- c("date", "value")
        
        # assign values from output filled df
        for (j in test5$Date) {
            outDF[outDF$date == j, "value"] <- test5[test5$Date == j, "s1"]
            outDF2[outDF2$date == j, "value"] <- test5[test5$Date == j, "s2"]
        }
        
        outDF$Year <- as.numeric(format(outDF$date, "%Y"))
        outDF$Month <- as.numeric(format(outDF$date, "%m"))
        outDF$Day <- as.numeric(format(outDF$date, "%d"))
        
        outDF2$Year <- as.numeric(format(outDF2$date, "%Y"))
        outDF2$Month <- as.numeric(format(outDF2$date, "%m"))
        outDF2$Day <- as.numeric(format(outDF2$date, "%d"))
        
        outDF$value[is.na(outDF$value)] <- -99.9
        outDF2$value[is.na(outDF2$value)] <- -99.9
        
        outDF <- outDF[,c("date", "Year", "Month", "Day", "value")]
        colnames(outDF) <- c("date", "Year", "Month", "Day", "value")
        write.csv(outDF,outName, row.names=F)
        
        outDF2 <- outDF2[,c("date", "Year", "Month", "Day", "value")]
        colnames(outDF2) <- c("date", "Year", "Month", "Day", "value")
        write.csv(outDF2,outName2, row.names=F)
        
        
        if (threshold >= 2) {
            t.series <- seq.Date(from = min(modDF3$date), to = max(modDF3$date),
                                 by = "day")
            outDF3 <- data.frame(t.series, NA)
            colnames(outDF3) <- c("date", "value")
            
            # assign values from output filled df
            for (j in test5$Date) {
                outDF3[outDF3$date == j, "value"] <- test5[test5$Date == j, "s3"]
            }
            
            outDF3$Year <- as.numeric(format(outDF3$date, "%Y"))
            outDF3$Month <- as.numeric(format(outDF3$date, "%m"))
            outDF3$Day <- as.numeric(format(outDF3$date, "%d"))
            
            outDF3$value[is.na(outDF3$value)] <- -99.9
            
            outDF3 <- outDF3[,c("date", "Year", "Month", "Day", "value")]
            colnames(outDF3) <- c("date", "Year", "Month", "Day", "value")
            write.csv(outDF3,outName3, row.names=F)
            
            if (threshold >= 3) {
                
                t.series <- seq.Date(from = min(modDF4$date), to = max(modDF4$date),
                                     by = "day")
                outDF4 <- data.frame(t.series, NA)
                colnames(outDF4) <- c("date", "value")
                
                # assign values from output filled df
                for (j in test5$Date) {
                    outDF4[outDF4$date == j, "value"] <- test5[test5$Date == j, "s4"]
                }
                
                outDF4$Year <- as.numeric(format(outDF4$date, "%Y"))
                outDF4$Month <- as.numeric(format(outDF4$date, "%m"))
                outDF4$Day <- as.numeric(format(outDF4$date, "%d"))
                
                outDF4$value[is.na(outDF4$value)] <- -99.9
                
                outDF4 <- outDF4[,c("date", "Year", "Month", "Day", "value")]
                colnames(outDF4) <- c("date", "Year", "Month", "Day", "value")
                write.csv(outDF4,outName4, row.names=F)
                
                if (threshold >= 4) {
                    t.series <- seq.Date(from = min(modDF5$date), to = max(modDF5$date),
                                         by = "day")
                    outDF5 <- data.frame(t.series, NA)
                    colnames(outDF5) <- c("date", "value")
                    
                    # assign values from output filled df
                    for (j in test5$Date) {
                        outDF5[outDF5$date == j, "value"] <- test5[test5$Date == j, "s5"]
                    }
                    
                    outDF5$Year <- as.numeric(format(outDF5$date, "%Y"))
                    outDF5$Month <- as.numeric(format(outDF5$date, "%m"))
                    outDF5$Day <- as.numeric(format(outDF5$date, "%d"))
                    
                    outDF5$value[is.na(outDF5$value)] <- -99.9
                    
                    outDF5 <- outDF5[,c("date", "Year", "Month", "Day", "value")]
                    colnames(outDF5) <- c("date", "Year", "Month", "Day", "value")
                    write.csv(outDF5,outName5, row.names=F)
                    
                    if (threshold >= 5) {
                        t.series <- seq.Date(from = min(modDF6$date), to = max(modDF6$date),
                                             by = "day")
                        outDF6 <- data.frame(t.series, NA)
                        colnames(outDF6) <- c("date", "value")
                        
                        # assign values from output filled df
                        for (j in test5$Date) {
                            outDF6[outDF6$date == j, "value"] <- test5[test5$Date == j, "s6"]
                        }
                        
                        outDF6$Year <- as.numeric(format(outDF6$date, "%Y"))
                        outDF6$Month <- as.numeric(format(outDF6$date, "%m"))
                        outDF6$Day <- as.numeric(format(outDF6$date, "%d"))
                        
                        outDF6$value[is.na(outDF6$value)] <- -99.9
                        
                        outDF6 <- outDF6[,c("date", "Year", "Month", "Day", "value")]
                        colnames(outDF6) <- c("date", "Year", "Month", "Day", "value")
                        write.csv(outDF6,outName6, row.names=F)
                        
                        if (threshold >= 6) {
                            t.series <- seq.Date(from = min(modDF7$date), to = max(modDF7$date),
                                                 by = "day")
                            outDF7 <- data.frame(t.series, NA)
                            colnames(outDF7) <- c("date", "value")
                            
                            # assign values from output filled df
                            for (j in test5$Date) {
                                outDF7[outDF7$date == j, "value"] <- test5[test5$Date == j, "s7"]
                            }
                            
                            outDF7$Year <- as.numeric(format(outDF7$date, "%Y"))
                            outDF7$Month <- as.numeric(format(outDF7$date, "%m"))
                            outDF7$Day <- as.numeric(format(outDF7$date, "%d"))
                            
                            outDF7$value[is.na(outDF7$value)] <- -99.9
                            
                            outDF7 <- outDF7[,c("date", "Year", "Month", "Day", "value")]
                            colnames(outDF7) <- c("date", "Year", "Month", "Day", "value")
                            write.csv(outDF7,outName7, row.names=F)
                            
                            
                        } # 6
                    } # 5
                } # 4
            } # 3
        } # 2
        
        print(targList[i])
        
    }
    
}
