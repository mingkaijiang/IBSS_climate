##############################################################################################################
Update_station_list <- function(station.list.input, sDF) {
    t1 <- matrix(ncol=9, nrow=nrow(sDF), station.list.input[,2])
    t1 <- as.data.frame(t1,stringsAsFactors=F)
    colnames(t1) <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9")
    
    # update stationDF 
    sDF$ghcn1 <- as.character(t1$s1)
    sDF$ghcn2 <- as.character(t1$s2)
    sDF$ghcn3 <- as.character(t1$s3)
    sDF$ghcn4 <- as.character(t1$s4)
    sDF$ghcn5 <- as.character(t1$s5)
    sDF$ghcn6 <- as.character(t1$s6)
    sDF$ghcn7 <- as.character(t1$s7)
    sDF$ghcn8 <- as.character(t1$s8)
    sDF$ghcn9 <- as.character(t1$s9)
    
    sDF<-as.data.frame(sDF, stringsAsFactors=F)
    
    # count number of NAs in each row
    csum <- rowSums(is.na(t1))
    
    # swap positions of NAs on each row
    
    for (i in 1:nrow(sDF)) {
        
        if (is.na(sDF[i,"ghcn9"])) {
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        if (is.na(sDF[i,"ghcn8"])) {
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        
        if (is.na(sDF[i,"ghcn7"])) {
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        
        if (is.na(sDF[i,"ghcn6"])) {
            sDF[i,"ghcn6"] <- sDF[i,"ghcn7"]
            sDF[i,"lat6"] <- sDF[i,"lat7"]
            sDF[i,"lon6"] <- sDF[i,"lon7"]
            sDF[i,"elev6"] <- sDF[i,"elev7"]
            
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        
        if (is.na(sDF[i,"ghcn5"])) {
            sDF[i,"ghcn5"] <- sDF[i,"ghcn6"]
            sDF[i,"lat5"] <- sDF[i,"lat6"]
            sDF[i,"lon5"] <- sDF[i,"lon6"]
            sDF[i,"elev5"] <- sDF[i,"elev6"]
            
            sDF[i,"ghcn6"] <- sDF[i,"ghcn7"]
            sDF[i,"lat6"] <- sDF[i,"lat7"]
            sDF[i,"lon6"] <- sDF[i,"lon7"]
            sDF[i,"elev6"] <- sDF[i,"elev7"]
            
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        if (is.na(sDF[i,"ghcn4"])) {
            sDF[i,"ghcn4"] <- sDF[i,"ghcn5"]
            sDF[i,"lat4"] <- sDF[i,"lat5"]
            sDF[i,"lon4"] <- sDF[i,"lon5"]
            sDF[i,"elev4"] <- sDF[i,"elev5"]
            
            sDF[i,"ghcn5"] <- sDF[i,"ghcn6"]
            sDF[i,"lat5"] <- sDF[i,"lat6"]
            sDF[i,"lon5"] <- sDF[i,"lon6"]
            sDF[i,"elev5"] <- sDF[i,"elev6"]
            
            sDF[i,"ghcn6"] <- sDF[i,"ghcn7"]
            sDF[i,"lat6"] <- sDF[i,"lat7"]
            sDF[i,"lon6"] <- sDF[i,"lon7"]
            sDF[i,"elev6"] <- sDF[i,"elev7"]
            
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        if (is.na(sDF[i,"ghcn3"])) {
            sDF[i,"ghcn3"] <- sDF[i,"ghcn4"]
            sDF[i,"lat3"] <- sDF[i,"lat4"]
            sDF[i,"lon3"] <- sDF[i,"lon4"]
            sDF[i,"elev3"] <- sDF[i,"elev4"]
            
            sDF[i,"ghcn4"] <- sDF[i,"ghcn5"]
            sDF[i,"lat4"] <- sDF[i,"lat5"]
            sDF[i,"lon4"] <- sDF[i,"lon5"]
            sDF[i,"elev4"] <- sDF[i,"elev5"]
            
            sDF[i,"ghcn5"] <- sDF[i,"ghcn6"]
            sDF[i,"lat5"] <- sDF[i,"lat6"]
            sDF[i,"lon5"] <- sDF[i,"lon6"]
            sDF[i,"elev5"] <- sDF[i,"elev6"]
            
            sDF[i,"ghcn6"] <- sDF[i,"ghcn7"]
            sDF[i,"lat6"] <- sDF[i,"lat7"]
            sDF[i,"lon6"] <- sDF[i,"lon7"]
            sDF[i,"elev6"] <- sDF[i,"elev7"]
            
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
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
            
            sDF[i,"ghcn4"] <- sDF[i,"ghcn5"]
            sDF[i,"lat4"] <- sDF[i,"lat5"]
            sDF[i,"lon4"] <- sDF[i,"lon5"]
            sDF[i,"elev4"] <- sDF[i,"elev5"]
            
            sDF[i,"ghcn5"] <- sDF[i,"ghcn6"]
            sDF[i,"lat5"] <- sDF[i,"lat6"]
            sDF[i,"lon5"] <- sDF[i,"lon6"]
            sDF[i,"elev5"] <- sDF[i,"elev6"]
            
            sDF[i,"ghcn6"] <- sDF[i,"ghcn7"]
            sDF[i,"lat6"] <- sDF[i,"lat7"]
            sDF[i,"lon6"] <- sDF[i,"lon7"]
            sDF[i,"elev6"] <- sDF[i,"elev7"]
            
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
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
            
            sDF[i,"ghcn4"] <- sDF[i,"ghcn5"]
            sDF[i,"lat4"] <- sDF[i,"lat5"]
            sDF[i,"lon4"] <- sDF[i,"lon5"]
            sDF[i,"elev4"] <- sDF[i,"elev5"]
            
            sDF[i,"ghcn5"] <- sDF[i,"ghcn6"]
            sDF[i,"lat5"] <- sDF[i,"lat6"]
            sDF[i,"lon5"] <- sDF[i,"lon6"]
            sDF[i,"elev5"] <- sDF[i,"elev6"]
            
            sDF[i,"ghcn6"] <- sDF[i,"ghcn7"]
            sDF[i,"lat6"] <- sDF[i,"lat7"]
            sDF[i,"lon6"] <- sDF[i,"lon7"]
            sDF[i,"elev6"] <- sDF[i,"elev7"]
            
            sDF[i,"ghcn7"] <- sDF[i,"ghcn8"]
            sDF[i,"lat7"] <- sDF[i,"lat8"]
            sDF[i,"lon7"] <- sDF[i,"lon8"]
            sDF[i,"elev7"] <- sDF[i,"elev8"]
            
            sDF[i,"ghcn8"] <- sDF[i,"ghcn9"]
            sDF[i,"lat8"] <- sDF[i,"lat9"]
            sDF[i,"lon8"] <- sDF[i,"lon9"]
            sDF[i,"elev8"] <- sDF[i,"elev9"]
            
            sDF[i,"ghcn9"] <- NA
            sDF[i,"lat9"] <- NA
            sDF[i,"lon9"] <- NA
            sDF[i,"elev9"] <- NA
        }
        
        
    }
    
    return(sDF)
}
