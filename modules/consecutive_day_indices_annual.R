## Actual computation of consecutive day indices
## based on 1 growing season for each SCCS society

consecutive_day_indices_annual <- function(sourceDir, destDir) {
    # s.date: starting doy for plant 1
    # e.date: end doy for plant 1
    # wea.station: weather station id list
    # sccs.id: sccs id
    # dest: dest directory
    
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    dir.create(destDir, showWarnings = FALSE)
    
    for (thisFile in 1:length(DatFiles)) {
        print(thisFile)
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.csv(inName)
        
        outDF <- data.frame(unique(dd$Year), NA, NA)
        colnames(outDF) <- c("Year", "consecutive_dry", "consecutive_wet")
        
        for (j in dd$Year) {
            myDF <- dd[dd$Year == j, ]
            index <- myDF$value > 0
            myDF$value[index] <- 1
            ann <- rle(myDF$value)
            p <- length(myDF$value)
            outDF[outDF$Year == j, "consecutive_wet"] <- max(ann$lengths[ann$values>0]) / p
            outDF[outDF$Year == j, "consecutive_dry"] <- max(ann$lengths[ann$values==0]) / p
        }
        
        outDF$consecutive_dry[is.infinite(outDF$consecutive_dry)] <- 0
        outDF$consecutive_wet[is.infinite(outDF$consecutive_wet)] <- 0
        
        # write output
        write.csv(outDF, outName, row.names=F)
        
    }
}

