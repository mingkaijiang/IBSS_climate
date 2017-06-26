##############################################################################################################
select_9_ghcn_stations <- function(corDF, gDF) {
    
    ### libraries
    library(sp)
    library(rgeos)
    library(Imap)
    
    
    ### SCCS station list
    corDF$SCCSID <- as.character(corDF$SCCSID)
    sDF <- corDF[!duplicated(corDF$SCCSID), ] 
    
    newDF <- data.frame(sDF$SCCSID, sDF$slat, sDF$slon,
                        0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0)
    
    colnames(newDF) <- c("sccs_id", "slat", "slon",
                         "ghcn1", "lat1", "lon1", "elev1",
                         "ghcn2", "lat2", "lon2", "elev2",
                         "ghcn3", "lat3", "lon3", "elev3",
                         "ghcn4", "lat4", "lon4", "elev4",
                         "ghcn5", "lat5", "lon5", "elev5",
                         "ghcn6", "lat6", "lon6", "elev6",
                         "ghcn7", "lat7", "lon7", "elev7",
                         "ghcn8", "lat8", "lon8", "elev8",
                         "ghcn9", "lat9", "lon9", "elev9")
    
    
    gDF$GHCN_ID <- as.character(gDF$GHCN_ID)
    
    
    ### Find nearest 9 stations 
    for (i in 1:nrow(newDF)) {
        gDF$dist <- gdist(lat.1=gDF$Lat,
                          lon.1=gDF$Lon,
                          lat.2=newDF$slat[i],
                          lon.2=newDF$slon[i])
        
        newg <- gDF[order(gDF$dist),]
        
        newDF[i,"ghcn1"] <- newg[1,"GHCN_ID"]
        newDF[i,"lat1"] <- newg[1,"Lat"]
        newDF[i,"lon1"] <- newg[1,"Lon"]
        newDF[i,"elev1"] <- newg[1,"Elev"]
        
        newDF[i,"ghcn2"] <- newg[2,"GHCN_ID"]
        newDF[i,"lat2"] <- newg[2,"Lat"]
        newDF[i,"lon2"] <- newg[2,"Lon"]
        newDF[i,"elev2"] <- newg[2,"Elev"]
        
        newDF[i,"ghcn3"] <- newg[3,"GHCN_ID"]
        newDF[i,"lat3"] <- newg[3,"Lat"]
        newDF[i,"lon3"] <- newg[3,"Lon"]
        newDF[i,"elev3"] <- newg[3,"Elev"]
        
        newDF[i,"ghcn4"] <- newg[4,"GHCN_ID"]
        newDF[i,"lat4"] <- newg[4,"Lat"]
        newDF[i,"lon4"] <- newg[4,"Lon"]
        newDF[i,"elev4"] <- newg[4,"Elev"]
        
        newDF[i,"ghcn5"] <- newg[5,"GHCN_ID"]
        newDF[i,"lat5"] <- newg[5,"Lat"]
        newDF[i,"lon5"] <- newg[5,"Lon"]
        newDF[i,"elev5"] <- newg[5,"Elev"]
        
        newDF[i,"ghcn6"] <- newg[6,"GHCN_ID"]
        newDF[i,"lat6"] <- newg[6,"Lat"]
        newDF[i,"lon6"] <- newg[6,"Lon"]
        newDF[i,"elev6"] <- newg[6,"Elev"]
        
        newDF[i,"ghcn7"] <- newg[7,"GHCN_ID"]
        newDF[i,"lat7"] <- newg[7,"Lat"]
        newDF[i,"lon7"] <- newg[7,"Lon"]
        newDF[i,"elev7"] <- newg[7,"Elev"]
        
        newDF[i,"ghcn8"] <- newg[8,"GHCN_ID"]
        newDF[i,"lat8"] <- newg[8,"Lat"]
        newDF[i,"lon8"] <- newg[8,"Lon"]
        newDF[i,"elev8"] <- newg[8,"Elev"]
        
        newDF[i,"ghcn9"] <- newg[9,"GHCN_ID"]
        newDF[i,"lat9"] <- newg[9,"Lat"]
        newDF[i,"lon9"] <- newg[9,"Lon"]
        newDF[i,"elev9"] <- newg[9,"Elev"]
        
    }
    
    
    write.csv(newDF, "data/sccs_ghcn_station_list.csv", row.names=F)
    
    return(newDF)
}

