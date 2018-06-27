## Actual computation of consecutive day indices
## based on 1 growing season for each SCCS society

CSDI_annual <- function(sourceDir, destDir) {
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
        
        outDF <- data.frame(unique(dd$Year), NA)
        colnames(outDF) <- c("Year", "consecutive_days")
        
        for (j in dd$Year) {
            myDF <- dd[dd$Year == j, ]
            p <- length(myDF$value)
            t <- quantile(myDF$value,0.1)
            myDF2 <- myDF$value > t
            ann <- rle(myDF2)
            outDF[outDF$Year == j, "consecutive_days"] <- max(ann$lengths[which(ann$values == "TRUE")]) / p
        }
        
        outDF$consecutive_days[is.infinite(outDF$consecutive_days)] <- 0

        # write output
        write.csv(outDF, outName, row.names=F)
        
    }
}

