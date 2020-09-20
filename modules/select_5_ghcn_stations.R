##############################################################################################################
select_5_ghcn_stations <- function(corDF, gDF, grDF) {
    # corDF: DF with SCCS coordinates and GHCN coordinates
    # gDF: GHCN station list
    # grDF: growing season DF
    
    ### SCCS station list
    newDF <- data.frame(grDF$SCCS_ID, grDF$Lat, grDF$Lon,
                        grDF$Time.focus, grDF$Plant1.start,
                        grDF$Plant1.end,
                        grDF$Plant2.start, 
                        grDF$Plant2.end, 
                        grDF$Plant3.start, 
                        grDF$Plant3.end,
                        grDF$Plant4.start, 
                        grDF$Plant4.end,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0)
    
    colnames(newDF) <- c("sccs_id", "slat", "slon",
                         "focal_yr", "plant1_start", 
                         "plant1_end", 
                         "plant2_start",
                         "plant2_end", 
                         "plant3_start", 
                         "plant3_end", 
                         "plant4_start", 
                         "plant4_end", 
                         "ghcn1", "lat1", "lon1", "elev1", "dist_to_SCCS_km",
                         "ghcn2", "lat2", "lon2", "elev2", "dist_to_GHCN2_km",
                         "ghcn3", "lat3", "lon3", "elev3", "dist_to_GHCN3_km",
                         "ghcn4", "lat4", "lon4", "elev4", "dist_to_GHCN4_km",
                         "ghcn5", "lat5", "lon5", "elev5", "dist_to_GHCN5_km")
    
    
    gDF$GHCN_ID <- as.character(gDF$GHCN_ID)
    
    
    ### Find nearest primary station
    for (i in 1:nrow(newDF)) {
        gDF$dist <- gdist(lat.1=gDF$Lat,
                          lon.1=gDF$Lon,
                          lat.2=newDF$slat[i],
                          lon.2=newDF$slon[i],
                          units = "km")
        
        newg <- gDF[order(gDF$dist),]
        
        newDF[i,"ghcn1"] <- newg[1,"GHCN_ID"]
        newDF[i,"lat1"] <- newg[1,"Lat"]
        newDF[i,"lon1"] <- newg[1,"Lon"]
        newDF[i,"elev1"] <- newg[1,"Elev"]
        newDF[i,"dist_to_SCCS_km"] <- newg[1,"dist"]
    }
    
    ### remove SCCS sites with primary weather station > 300 km
    newDF <- subset(newDF, dist_to_SCCS_km <= 300) 
    
    ### find 4 weather stations close to the primary weather station
    for (i in 1:nrow(newDF)) {
      gDF$dist <- gdist(lat.1=gDF$Lat,
                        lon.1=gDF$Lon,
                        lat.2=newDF$lat1[i],
                        lon.2=newDF$lon1[i],
                        units = "km")
      
      newg <- gDF[order(gDF$dist),]
    
      newDF[i,"ghcn2"] <- newg[2,"GHCN_ID"]
      newDF[i,"lat2"] <- newg[2,"Lat"]
      newDF[i,"lon2"] <- newg[2,"Lon"]
      newDF[i,"elev2"] <- newg[2,"Elev"]
      newDF[i,"dist_to_GHCN2_km"] <- newg[2,"dist"]
        
      newDF[i,"ghcn3"] <- newg[3,"GHCN_ID"]
      newDF[i,"lat3"] <- newg[3,"Lat"]
      newDF[i,"lon3"] <- newg[3,"Lon"]
      newDF[i,"elev3"] <- newg[3,"Elev"]
      newDF[i,"dist_to_GHCN3_km"] <- newg[3,"dist"]
        
      newDF[i,"ghcn4"] <- newg[4,"GHCN_ID"]
      newDF[i,"lat4"] <- newg[4,"Lat"]
      newDF[i,"lon4"] <- newg[4,"Lon"]
      newDF[i,"elev4"] <- newg[4,"Elev"]
      newDF[i,"dist_to_GHCN4_km"] <- newg[4,"dist"]
        
      newDF[i,"ghcn5"] <- newg[5,"GHCN_ID"]
      newDF[i,"lat5"] <- newg[5,"Lat"]
      newDF[i,"lon5"] <- newg[5,"Lon"]
      newDF[i,"elev5"] <- newg[5,"Elev"]
      newDF[i,"dist_to_GHCN5_km"] <- newg[5,"dist"]
        
    }
    
    ### remove SCCS sites with too few weather stations
    outDF <- subset(newDF, dist_to_GHCN2_km <= 300)
    #outDF2 <- subset(newDF, dist_to_GHCN5_km <= 300)
    
    write.csv(outDF, "data/sccs_ghcn_station_list_5_stations.csv", row.names=F)
    
    return(outDF)
}

