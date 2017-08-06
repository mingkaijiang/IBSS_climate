#### To convert nc data into csv file
nc_to_csv <- function(inDF) {
    
    lon.list <- seq(0.25, 359.75, by = 0.5)
    lat.list <- seq(-89.75, 89.75, by = 0.5)
    
    outDir <- "data/cpc_selected"
    dir.create(outDir, showWarnings = FALSE)
    
    for (j in 1:nrow(inDF)) {
        
        # get location of the lon and lat for SCCS site
        lat <- inDF[j, "lat_cpc"]
        lon <- inDF[j, "lon_cpc"]
        
        loc_lat <- match(lat, lat.list)
        loc_lon <- match(lon, lon.list)
        
        # prepare output df
        outDF <- matrix(ncol=7, nrow=365*30)
        outDF <- as.data.frame(outDF)
        colnames(outDF) <- c("SCCSID", "SITEID", "lon", "lat", "year", "doy", "prcp")
        outDF$SCCSID <- inDF[j, "SCCSID"]
        outDF$SITEID <- inDF[j, "SITEID"]
        outDF$lon <- lon
        outDF$lat <- lat
        outDF$year <- rep(1981:2010, each = 365)
        outDF$doy <- rep(1:365, times = 30)
        
        for (i in 1981:2010) {
            inFile <- paste0("data/cpc/precip.", i, ".nc")
            
            # read in nc file
            ncDF <- nc_open(inFile) 
            
            tmp.array <- ncvar_get(ncDF)
            
            d.array <- tmp.array[loc_lon, loc_lat, ]
            
            outDF[outDF$year == i, "prcp"] <- d.array[1:365]
            
            nc_close(ncDF)
        }   # end of year loop
        
        outName <- outDF[1, "SITEID"]
        outFile <- paste0(outDir, "/SITEID", outName, ".csv")

        write.table(outDF, outFile, 
                    row.names = F,col.names=T, sep = ",")
        
    }       # end of site loop
    
}


