##############################################################################################################
## Filter data for Year range > 10 for long-term trend analysis 
## Also check for missing data issue, missing data should not be > 80%
Missing_check<-function(station.list.input, sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- paste0(station.list.input,".csv")
    dir.create(destDir, showWarnings = FALSE)
    
    id.list <- c(1:length(station.list.input))
    station.list.output <- data.frame(id.list, NA)
    colnames(station.list.output) <- c("id", "station")
    
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        if(file.exists(inName)) {
            outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
            
            dd <- read.csv(inName,header=F,skip = 1, sep=",")
            colnames(dd) <- c("id", "Year", "Month", "Day", "value")
            dd$date <- as.Date(paste(dd$Year, dd$Month, dd$Day, sep="-"),
                               format = "%Y-%m-%d")
            
            t.series <- seq.Date(from = min(dd$date), to = max(dd$date),
                                 by = "day")
            
            dd[dd$value == -99.9, "value"] <- NA
            
            # create outout df
            out <- data.frame(t.series, NA, NA, NA, NA)
            
            colnames(out) <- c("date", "Year", "Month", "Day", "value")
            out$value <- dd$value[match(out$date, dd$date)]
            
            out$Year <- as.numeric(format(out$date, "%Y"))
            out$Month <- as.numeric(format(out$date, "%m"))
            out$Day <- as.numeric(format(out$date, "%d"))
            out[is.na(out$value), "value"] <- -99.9
            
            # check for missing data issue
            target <- length(t.series)
            
            d2 <- dd[complete.cases(dd),]
            reality <- nrow(d2)
            miss_percent <- (target - reality) / target
            
            if (miss_percent <= 0.2)
            {  
                print(paste0(thisFile, "------", station.list.input[thisFile]))
                write.csv(out, outName, row.names=F)
                station.list.output[thisFile, "station"] <- station.list.input[thisFile]
            }
        }
    }
    
    return(station.list.output)
}
