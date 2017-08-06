####################################################################################
## To convert SCCS site coordinates onto CPC gridded 0.5 resolution grid
Convert_coordinates <- function(inDF) {
    
    # prepare inDF
    inDF$Weight <- NULL
    inDF$ClimateStart <- NULL
    inDF$ClimateEnd <- NULL
    inDF$slat <- NULL
    inDF$slon <- NULL
    inDF$SITE.name <- NULL
    
    # prepare lon and lat information
    inDF$intlon <- round(inDF[,"flon"], 0)
    inDF$declon <- inDF[,"flon"] - inDF$intlon
    inDF$u <- inDF$intlon + 0.5
    inDF$d <- inDF$intlon - 0.5
    inDF[,"lon_cpc_interm"] <- ifelse(inDF[,"flon"] >= inDF$intlon, inDF$intlon+0.25, 
                               inDF$intlon-0.25)
    
    inDF$intlat <- round(inDF[, "flat"], 0)
    inDF$declat <- inDF[,"flat"] - inDF$intlat
    inDF$u <- inDF$intlat + 0.5
    inDF$d <- inDF$intlat - 0.5
    inDF[,"lat_cpc"] <- ifelse(inDF[,"flat"] >= inDF$intlat, inDF$intlat+0.25, 
                               inDF$intlat-0.25)
    
    # convert lon
    inDF[,"lon_cpc"] <- ifelse(inDF[,"lon_cpc_interm"] >= 0, inDF[,"lon_cpc_interm"],
                               inDF$lon_cpc_interm + 360)
    
        
    inDF$u <-NULL
    inDF$d <-NULL
    inDF$intlon <- NULL
    inDF$intlat <- NULL
    inDF$declon <- NULL
    inDF$declat <- NULL
    inDF$lon_cpc_interm <- NULL
    
    return(inDF)
}