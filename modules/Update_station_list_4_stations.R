##############################################################################################################
Update_station_list <- function(station.list.input, sDF) {
    t1 <- matrix(ncol=4, nrow=nrow(sDF), station.list.input[,2])
    t1 <- as.data.frame(t1,stringsAsFactors=F)
    colnames(t1) <- c("s1", "s2", "s3", "s4")
    
    # update stationDF 
    sDF$ghcn1 <- as.character(t1$s1)
    sDF$ghcn2 <- as.character(t1$s2)
    sDF$ghcn3 <- as.character(t1$s3)
    sDF$ghcn4 <- as.character(t1$s4)
    
    sDF<-as.data.frame(sDF, stringsAsFactors=F)
    
    # count number of NAs in each row
    csum <- rowSums(is.na(t1))
    
    # swap positions of NAs on each row
    
    for (i in 1:nrow(sDF)) {
        
        if (is.na(sDF[i,"ghcn4"])) {
            sDF[i,"ghcn4"] <- NA
            sDF[i,"lat4"] <- NA
            sDF[i,"lon4"] <- NA
            sDF[i,"elev4"] <- NA

        }
        
        if (is.na(sDF[i,"ghcn3"])) {
            sDF[i,"ghcn3"] <- sDF[i,"ghcn4"]
            sDF[i,"lat3"] <- sDF[i,"lat4"]
            sDF[i,"lon3"] <- sDF[i,"lon4"]
            sDF[i,"elev3"] <- sDF[i,"elev4"]
            
            sDF[i,"ghcn4"] <- NA
            sDF[i,"lat4"] <- NA
            sDF[i,"lon4"] <- NA
            sDF[i,"elev4"] <- NA

        }
        
        if (is.na(sDF[i,"ghcn2"])) {
            sDF[i,"ghcn2"] <- sDF[i,"ghcn3"]
            sDF[i,"lat2"] <- sDF[i,"lat3"]
            sDF[i,"lon2"] <- sDF[i,"lon3"]
            sDF[i,"elev2"] <- sDF[i,"elev3"]
            
            sDF[i,"ghcn3"] <- sDF[i,"ghcn4"]
            sDF[i,"lat3"] <- sDF[i,"lat4"]
            sDF[i,"lon3"] <- sDF[i,"lon4"]
            sDF[i,"elev3"] <- sDF[i,"elev4"]
            
            sDF[i,"ghcn4"] <- NA
            sDF[i,"lat4"] <- NA
            sDF[i,"lon4"] <- NA
            sDF[i,"elev4"] <- NA
            
        }
        
        if(is.na(sDF[i, "ghcn1"])) {
            sDF[i,"ghcn1"] <- sDF[i,"ghcn2"]
            sDF[i,"lat1"] <- sDF[i,"lat2"]
            sDF[i,"lon1"] <- sDF[i,"lon2"]
            sDF[i,"elev1"] <- sDF[i,"elev2"]
            
            sDF[i,"ghcn2"] <- sDF[i,"ghcn3"]
            sDF[i,"lat2"] <- sDF[i,"lat3"]
            sDF[i,"lon2"] <- sDF[i,"lon3"]
            sDF[i,"elev2"] <- sDF[i,"elev3"]
            
            sDF[i,"ghcn3"] <- sDF[i,"ghcn4"]
            sDF[i,"lat3"] <- sDF[i,"lat4"]
            sDF[i,"lon3"] <- sDF[i,"lon4"]
            sDF[i,"elev3"] <- sDF[i,"elev4"]
            
            sDF[i,"ghcn4"] <- NA
            sDF[i,"lat4"] <- NA
            sDF[i,"lon4"] <- NA
            sDF[i,"elev4"] <- NA

        }
        
        
    }
    
    return(sDF)
}
