#### Process the growing season dataset to make one single entry per SCCS society
growing_season_single_entry <- function(grDF) {
    
    # get sccs list
    sccs.list <- unique(grDF$SCCS_ID)
    l <- length(sccs.list)
    
    # unique df
    uDF <- grDF[!duplicated(grDF$SCCS_ID), ] 

    # prepare output df
    outDF <- data.frame(sccs.list, uDF$Time.focus, uDF$Longitude, uDF$Latitude, 
                        NA, NA, NA, NA,
                        NA, NA, NA, NA)
    colnames(outDF) <- c("SCCS_ID", "Time.focus", "Lon", "Lat",
                         "Plant1.start", 
                         "Plant1.end",
                         "Plant2.start", 
                         "Plant2.end", 
                         "Plant3.start", 
                         "Plant3.end", 
                         "Plant4.start", 
                         "Plant4.end")
    
    for (i in sccs.list) {
        temDF <- subset(grDF, SCCS_ID == i)
        l2 <- nrow(temDF)
        
        if (l2 == 1) {
            outDF[outDF$SCCS_ID == i, "Plant1.start"] <- grDF[grDF$SCCS_ID == i, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant1.end"] <- grDF[grDF$SCCS_ID == i, "Plant.end"]

        } else if (l2 == 2) {
            outDF[outDF$SCCS_ID == i, "Plant1.start"] <- temDF[1, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant1.end"] <- temDF[1, "Plant.end"]

            outDF[outDF$SCCS_ID == i, "Plant2.start"] <- temDF[2, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant2.end"] <- temDF[2, "Plant.end"]
        } else if (l2 == 3) {
            outDF[outDF$SCCS_ID == i, "Plant1.start"] <- temDF[1, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant1.end"] <- temDF[1, "Plant.end"]

            outDF[outDF$SCCS_ID == i, "Plant2.start"] <- temDF[2, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant2.end"] <- temDF[2, "Plant.end"]

            outDF[outDF$SCCS_ID == i, "Plant3.start"] <- temDF[3, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant3.end"] <- temDF[3, "Plant.end"]
        } else if (l2 == 4) {
            outDF[outDF$SCCS_ID == i, "Plant1.start"] <- temDF[1, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant1.end"] <- temDF[1, "Plant.end"]

            outDF[outDF$SCCS_ID == i, "Plant2.start"] <- temDF[2, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant2.end"] <- temDF[2, "Plant.end"]

            outDF[outDF$SCCS_ID == i, "Plant3.start"] <- temDF[3, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant3.end"] <- temDF[3, "Plant.end"]

            outDF[outDF$SCCS_ID == i, "Plant4.start"] <- temDF[4, "Plant.start"]
            outDF[outDF$SCCS_ID == i, "Plant4.end"] <- temDF[4, "Plant.end"]
        }  # ignore the rest
        
        
        
        
    }
    
    return(outDF)    
}