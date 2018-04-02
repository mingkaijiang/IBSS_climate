##############################################################################################################
## Update the station DF with by incorporating all data processing steps
## i.e. removing stations from the original list
Final_station_list_4<-function(sourceDir = DAILY.DATA.DIRECTORY, sDF, outname)
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
        
    }
    
    st.list <- c(sDF$ghcn1, sDF$ghcn2, sDF$ghcn3,
                 sDF$ghcn4)
    
    st.df <- data.frame(0, st.list)
    colnames(st.df) <- c("id", "station")
    st.df$id <- c(1:length(st.list))
    
    outDF <- Update_station_list(st.df, sDF)
    
    write.csv(outDF, paste0("data/sccs_ghcn_station_list_final_", outname, ".csv"))
    
    return(outDF)
}
