##############################################################################################################
### Gap filling using the hyfo package as an easy solution
Gap_Fill_within_station <- function(station.list.input, 
                                    sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)
    
    station.list <- unique(station.list.input$station)
    station.list <- station.list[!is.na(station.list)]
    
    # remove one station
    r <- station.list[489]
    rm.station <- paste0(sourceDir, "/", r, ".csv")
    if (file.exists(rm.station)) file.remove(rm.station)
    
    # rmove the station from the station list
    station.list <- station.list[-489]
    
    targList <- paste0(station.list,".csv")
    
    for (i in 1:length(targList)) 
    {
        inName <- file.path(sourceDir, targList[i], fsep = .Platform$file.sep)
        outName <- file.path(destDir, targList[i], fsep = .Platform$file.sep)
        
        if (file.exists(inName)) {
            # read in 1st file
            X <- read.csv(inName)
            X$date <- as.Date(paste(X$Year, X$Month, X$Day, sep="-"),
                              format = "%Y-%m-%d")
            
            X[X$value == -99.9, "value"] <- NA
            
            m.list <- unique(X$Month)
            
            for (j in m.list) {
                
                # select the month
                mt <- subset(X, Month == j)
                check.na <- sum(is.na(mt$value))
                
                if (check.na/nrow(mt) == 0) {
                    print(paste0("Data ", i, " Month ", j, " has no missing values"))
                    
                } else if (check.na/nrow(mt) == 1) {
                    # fill current month with previous month values
                    if (j >= 2) {
                        prev.mt <- subset(X, Month == (j-1))
                        
                        v.list <- prev.mt[!is.na(prev.mt$value), "value"]
                        
                        # computing frequency
                        rg <- range(v.list)
                        
                        # insert a conditional check to make sure breaks have values
                        if(rg[2]-rg[1] <= 1) {
                            by.value <- 0.01
                        } else if (rg[2] - rg[1] <= 100) {
                            by.value <- 0.1
                        } else if (rg[2] - rg[1] <= 10000) {
                            by.value <- 10
                        }
                        
                        s.value <- rg[1] - 0.0001
                        e.value <- rg[2] + by.value - 0.0001
                        
                        breaks <- seq(s.value, e.value, by = by.value)
                        fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
                        precip.cut <- cut(v.list, breaks)
                        precip.freq <- table(precip.cut)
                        
                        # computing probability
                        myprob <- data.frame(precip.freq, NA)
                        colnames(myprob) <- c("range", "freq", "prob")
                        csum <- sum(myprob$freq)
                        myprob$prob <- myprob$freq/csum
                        
                        # computing selection number list
                        mynumbers <- fill.list[1:length(fill.list)-1]
                        
                        # count number of missing values
                        mis <- sum(is.na(mt$value))
                        
                        # sampling from the number list following distribution probability
                        d <- sample(mynumbers, size=mis, replace=T, prob=myprob$prob)
                        
                        X[X$Month == j & is.na(X$value), "value"] <- d
                        
                    } else {  # for month 1, use Dec values
                        prev.mt <- subset(X, Month == 12)
                        
                        v.list <- prev.mt[!is.na(prev.mt$value), "value"]
                        
                        # computing frequency
                        rg <- range(v.list)
                        
                        # insert a conditional check to make sure breaks have values
                        if(rg[2]-rg[1] <= 1) {
                            by.value <- 0.01
                        } else if (rg[2] - rg[1] <= 100) {
                            by.value <- 0.1
                        } else if (rg[2] - rg[1] <= 10000) {
                            by.value <- 10
                        }
                        
                        s.value <- rg[1] - 0.0001
                        e.value <- rg[2] + by.value - 0.0001
                        
                        breaks <- seq(s.value, e.value, by = by.value)
                        fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
                        precip.cut <- cut(v.list, breaks)
                        precip.freq <- table(precip.cut)
                        
                        # computing probability
                        myprob <- data.frame(precip.freq, NA)
                        colnames(myprob) <- c("range", "freq", "prob")
                        csum <- sum(myprob$freq)
                        myprob$prob <- myprob$freq/csum
                        
                        # computing selection number list
                        mynumbers <- fill.list[1:length(fill.list)-1]
                        
                        # count number of missing values
                        mis <- sum(is.na(mt$value))
                        
                        # sampling from the number list following distribution probability
                        d <- sample(mynumbers, size=mis, replace=T, prob=myprob$prob)
                        
                        X[X$Month == j & is.na(X$value), "value"] <- d
                    }
                    
                } else {
                    v.list <- mt[!is.na(mt$value), "value"]
                    
                    
                    # computing frequency
                    rg <- range(v.list)
                    
                    
                    # insert a conditional check to make sure breaks have values
                    if(rg[2]-rg[1] <= 1) {
                        by.value <- 0.01
                    } else if (rg[2] - rg[1] <= 100) {
                        by.value <- 0.1
                    } else if (rg[2] - rg[1] <= 10000) {
                        by.value <- 10
                    }
                    
                    s.value <- rg[1] - 0.0001
                    e.value <- rg[2] + by.value - 0.0001
                    
                    breaks <- seq(s.value, e.value, by = by.value)
                    fill.list <- seq(rg[1], rg[2]+by.value, by = by.value)
                    precip.cut <- cut(v.list, breaks)
                    precip.freq <- table(precip.cut)
                    
                    # computing probability
                    myprob <- data.frame(precip.freq, NA)
                    colnames(myprob) <- c("range", "freq", "prob")
                    csum <- sum(myprob$freq)
                    myprob$prob <- myprob$freq/csum
                    
                    # computing selection number list
                    mynumbers <- fill.list[1:length(fill.list)-1]
                    
                    # count number of missing values
                    mis <- sum(is.na(mt$value))
                    
                    # sampling from the number list following distribution probability
                    d <- sample(mynumbers, size=mis, replace=T, prob=myprob$prob)
                    
                    X[X$Month == j & is.na(X$value), "value"] <- d
                }
                
            }
            
            write.csv(X,outName, row.names=F)
            
            print(i)
        } else {
            print(paste0("file ", i, " doesn't exist!"))
        }
        
        
    }  # i loop
    
}