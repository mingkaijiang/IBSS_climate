##############################################################################################################
## Compute consecutive dry and wet days based on growing season information
consecutive_day_indices_hunter_gatherer_tmin<-function(sDF,
                                                  sourceDir = DAILY.DATA.DIRECTORY, 
                                                  destDir = DAILY.OUTPUT.DIRECTORY) {
    ## This function calcualtes consecutive day and wet days indices for each GHCN station
    ## based on pre-defined growing season information;
    ## Consecutive dry days: # consecutive no rain days / # days in a season
    ## Consecutive wet days: # of consecutive precipitation days / # days in a season
    ## season is defined as: before growing season, growing season and after growing season
    ## Southern Hemisphere year starts on 1/Jul
    ## This function will return files with different naming criteria as the previous ones
    ## because there are multiple weather stations for the same SCCS site, 
    ## and one weather station could corresponds to multiple SCCS sites as well.
    ## In addition, there are multiple crops for each SCCS site
    ## So the final naming criteria follows: GHCN station ID _ SCCS ID _ Plant 1/2/3/4
    ## For hunter and gatherer societies only
  
    # prepare file list
  
    sourceDir = "data/ghcnd_gap_filled_tmin"
    
    destDir = "data/indices/CSDI_hunter_gatherer"
   
   
    dir.create(destDir, showWarnings = FALSE)

    # get sccs id information
    sccs.id <- sDF$sccs_id
    l <- length(sccs.id)
    
    # update growing season plant sequence
    for (i in 1:nrow(sDF)) {
        
        if (is.na(sDF[i,"plant3_start"])) {
            sDF[i,"plant3_start"] <- sDF[i,"plant4_start"]
            sDF[i,"plant3_end"] <- sDF[i,"plant4_end"]
            
            sDF[i,"plant4_start"] <- NA
            sDF[i,"plant4_end"] <- NA
        }
        
        if (is.na(sDF[i,"plant2_start"])) {
            sDF[i,"plant2_start"] <- sDF[i,"plant3_start"]
            sDF[i,"plant2_end"] <- sDF[i,"plant3_end"]
         
            sDF[i,"plant3_start"] <- sDF[i,"plant4_start"]
            sDF[i,"plant3_end"] <- sDF[i,"plant4_end"]   
            
            sDF[i,"plant4_start"] <- NA
            sDF[i,"plant4_end"] <- NA
        }
        
        if (is.na(sDF[i,"plant1_start"])) {
            sDF[i,"plant1_start"] <- sDF[i,"plant2_start"]
            sDF[i,"plant1_end"] <- sDF[i,"plant2_end"]
            
            sDF[i,"plant2_start"] <- sDF[i,"plant3_start"]
            sDF[i,"plant2_end"] <- sDF[i,"plant3_end"]
            
            sDF[i,"plant3_start"] <- sDF[i,"plant4_start"]
            sDF[i,"plant3_end"] <- sDF[i,"plant4_end"]   
            
            sDF[i,"plant4_start"] <- NA
            sDF[i,"plant4_end"] <- NA
        }
    }
    
    for (i in sccs.id) {
        print(paste0("within i loop ", i))
        # check if there is growing season information
        if (is.na(sDF[sDF$sccs_id == i, "plant1_start"])) {
            print(paste0("No growing season information for sccs id ", i))
            
            ghcn.list <- c(sDF[sDF$sccs_id == i, "ghcn1"], sDF[sDF$sccs_id == i, "ghcn2"],
                           sDF[sDF$sccs_id == i, "ghcn3"], sDF[sDF$sccs_id == i, "ghcn4"],
                           sDF[sDF$sccs_id == i, "ghcn5"], sDF[sDF$sccs_id == i, "ghcn6"],
                           sDF[sDF$sccs_id == i, "ghcn7"], sDF[sDF$sccs_id == i, "ghcn8"],
                           sDF[sDF$sccs_id == i, "ghcn9"])
            ghcn.list <- ghcn.list[!is.na(ghcn.list)]
            ghcn.l <- length(ghcn.list)
            
            # compute consecutive days indices
            ifelse(ghcn.l == 0, print(paste0("No GHCN station for SCCS ", i)), 
                   compute_consecutive_indices_no_growing_season_tmin(ghcn.list, i, sourceDir, destDir))
            
            
        } 
    }
    
}
