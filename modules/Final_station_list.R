##############################################################################################################
## Update the station DF with by incorporating all data processing steps
## i.e. removing stations from the original list
Final_station_list<-function(sourceDir = DAILY.DATA.DIRECTORY, sDF)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    for (i in 1:nrow(sDF)) {
        # GHCN 1
        g <- paste0(sDF[i, "ghcn1"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn1"] <- NA
            sDF[i, "lat1"] <- NA
            sDF[i, "lon1"] <- NA
            sDF[i, "elev1"] <- NA
            print(g)
        } 
        
        # GHCN 2
        g <- paste0(sDF[i, "ghcn2"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn2"] <- NA
            sDF[i, "lat2"] <- NA
            sDF[i, "lon2"] <- NA
            sDF[i, "elev2"] <- NA
            print(g)
        } 
        
        # GHCN 3
        g <- paste0(sDF[i, "ghcn3"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn3"] <- NA
            sDF[i, "lat3"] <- NA
            sDF[i, "lon3"] <- NA
            sDF[i, "elev3"] <- NA
            print(g)
        } 
        
        # GHCN 4
        g <- paste0(sDF[i, "ghcn4"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn4"] <- NA
            sDF[i, "lat4"] <- NA
            sDF[i, "lon4"] <- NA
            sDF[i, "elev4"] <- NA
            print(g)
        } 
        
        # GHCN 5
        g <- paste0(sDF[i, "ghcn5"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn5"] <- NA
            sDF[i, "lat5"] <- NA
            sDF[i, "lon5"] <- NA
            sDF[i, "elev5"] <- NA
            print(g)
        } 
        
        # GHCN 6
        g <- paste0(sDF[i, "ghcn6"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn6"] <- NA
            sDF[i, "lat6"] <- NA
            sDF[i, "lon6"] <- NA
            sDF[i, "elev6"] <- NA
            print(g)
        } 
        
        # GHCN 7
        g <- paste0(sDF[i, "ghcn7"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn7"] <- NA
            sDF[i, "lat7"] <- NA
            sDF[i, "lon7"] <- NA
            sDF[i, "elev7"] <- NA
            print(g)
        } 
        
        # GHCN 8
        g <- paste0(sDF[i, "ghcn8"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn8"] <- NA
            sDF[i, "lat8"] <- NA
            sDF[i, "lon8"] <- NA
            sDF[i, "elev8"] <- NA
            print(g)
        } 
        
        # GHCN 9
        g <- paste0(sDF[i, "ghcn9"], ".csv")
        if (any(DatFiles == g) == FALSE) {
            sDF[i, "ghcn9"] <- NA
            sDF[i, "lat9"] <- NA
            sDF[i, "lon9"] <- NA
            sDF[i, "elev9"] <- NA
            print(g)
        } 
    }
    
    st.list <- c(sDF$ghcn1, sDF$ghcn2, sDF$ghcn3,
                 sDF$ghcn4, sDF$ghcn5, sDF$ghcn6,
                 sDF$ghcn7, sDF$ghcn8, sDF$ghcn9)
    
    st.df <- data.frame(0, st.list)
    colnames(st.df) <- c("id", "station")
    st.df$id <- c(1:length(st.list))
    
    outDF <- Update_station_list(st.df, sDF)
    
    write.csv(outDF, "data/sccs_ghcn_station_list_final.csv")
    
    return(outDF)
}
