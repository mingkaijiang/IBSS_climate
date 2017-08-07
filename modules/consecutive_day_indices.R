##############################################################################################################
## Compute consecutive dry and wet days based on growing season information
consecutive_day_indices<-function(sDF,
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
    
    # prepare file list
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
        # check if there is growing season information
        if (is.na(sDF[sDF$sccs_id == i, "plant1_start"])) {
            print(paste0("No growing season information for sccs id ", i))
        } else if (is.na(sDF[sDF$sccs_id == i, "plant2_start"])) {
            print(paste0("One growing season for sccs id", i))
            # extract plant start and end doy information
            s.doy <- sDF[sDF$sccs_id == i, "plant1_start"]
            e.doy <- sDF[sDF$sccs_id == i, "plant1_end"]
            
            # ghcn station list
            ghcn.list <- c(sDF[sDF$sccs_id == i, "ghcn1"], sDF[sDF$sccs_id == i, "ghcn2"],
                           sDF[sDF$sccs_id == i, "ghcn3"], sDF[sDF$sccs_id == i, "ghcn4"],
                           sDF[sDF$sccs_id == i, "ghcn5"], sDF[sDF$sccs_id == i, "ghcn6"],
                           sDF[sDF$sccs_id == i, "ghcn7"], sDF[sDF$sccs_id == i, "ghcn8"],
                           sDF[sDF$sccs_id == i, "ghcn9"])
            ghcn.list <- ghcn.list[!is.na(ghcn.list)]
            ghcn.l <- length(ghcn.list)
            
            # compute consecutive days indices
            ifelse(ghcn.l == 0, print(paste0("No GHCN station for SCCS ", i)), 
                   compute_consecutive_indices_1(s.doy, e.doy, ghcn.list, i, sourceDir, destDir))
            
        } else if (is.na(sDF[sDF$sccs_id == i, "plant3_start"])) {
            print(paste0("Two growing seasons for sccs id", i))
            
            # extract plant start and end doy information
            s1.doy <- sDF[sDF$sccs_id == i, "plant1_start"]
            e1.doy <- sDF[sDF$sccs_id == i, "plant1_end"]
            s2.doy <- sDF[sDF$sccs_id == i, "plant2_start"]
            e2.doy <- sDF[sDF$sccs_id == i, "plant2_end"]
            
            # ghcn station list
            ghcn.list <- c(sDF[sDF$sccs_id == i, "ghcn1"], sDF[sDF$sccs_id == i, "ghcn2"],
                           sDF[sDF$sccs_id == i, "ghcn3"], sDF[sDF$sccs_id == i, "ghcn4"],
                           sDF[sDF$sccs_id == i, "ghcn5"], sDF[sDF$sccs_id == i, "ghcn6"],
                           sDF[sDF$sccs_id == i, "ghcn7"], sDF[sDF$sccs_id == i, "ghcn8"],
                           sDF[sDF$sccs_id == i, "ghcn9"])
            ghcn.list <- ghcn.list[!is.na(ghcn.list)]
            ghcn.l <- length(ghcn.list)
            
            # compute consecutive days indices
            ifelse(ghcn.l == 0, print(paste0("No GHCN station for SCCS ", i)), 
                   compute_consecutive_indices_2(s1.doy, e1.doy, s2.doy, e2.doy,
                                                 ghcn.list, i, sourceDir, destDir))
            
        } else if (is.na(sDF[sDF$sccs_id == i, "plant4_start"])) {
            print(paste0("Three growing seasons for sccs id", i))
            
            # extract plant start and end doy information
            s1.doy <- sDF[sDF$sccs_id == i, "plant1_start"]
            e1.doy <- sDF[sDF$sccs_id == i, "plant1_end"]
            s2.doy <- sDF[sDF$sccs_id == i, "plant2_start"]
            e2.doy <- sDF[sDF$sccs_id == i, "plant2_end"]
            s3.doy <- sDF[sDF$sccs_id == i, "plant3_start"]
            e3.doy <- sDF[sDF$sccs_id == i, "plant3_end"]
            
            # ghcn station list
            ghcn.list <- c(sDF[sDF$sccs_id == i, "ghcn1"], sDF[sDF$sccs_id == i, "ghcn2"],
                           sDF[sDF$sccs_id == i, "ghcn3"], sDF[sDF$sccs_id == i, "ghcn4"],
                           sDF[sDF$sccs_id == i, "ghcn5"], sDF[sDF$sccs_id == i, "ghcn6"],
                           sDF[sDF$sccs_id == i, "ghcn7"], sDF[sDF$sccs_id == i, "ghcn8"],
                           sDF[sDF$sccs_id == i, "ghcn9"])
            ghcn.list <- ghcn.list[!is.na(ghcn.list)]
            ghcn.l <- length(ghcn.list)
            
            # compute consecutive days indices
            ifelse(ghcn.l == 0, print(paste0("No GHCN station for SCCS ", i)), 
                   compute_consecutive_indices_3(s1.doy, e1.doy, s2.doy, e2.doy, s3.doy, e3.doy,
                                                 ghcn.list, i, sourceDir, destDir))
        } else {
            print(paste0("Four growing seasons for sccs id", i))
            
            # extract plant start and end doy information
            s1.doy <- sDF[sDF$sccs_id == i, "plant1_start"]
            e1.doy <- sDF[sDF$sccs_id == i, "plant1_end"]
            s2.doy <- sDF[sDF$sccs_id == i, "plant2_start"]
            e2.doy <- sDF[sDF$sccs_id == i, "plant2_end"]
            s3.doy <- sDF[sDF$sccs_id == i, "plant3_start"]
            e3.doy <- sDF[sDF$sccs_id == i, "plant3_end"]
            s4.doy <- sDF[sDF$sccs_id == i, "plant4_start"]
            e4.doy <- sDF[sDF$sccs_id == i, "plant4_end"]
            
            # ghcn station list
            ghcn.list <- c(sDF[sDF$sccs_id == i, "ghcn1"], sDF[sDF$sccs_id == i, "ghcn2"],
                           sDF[sDF$sccs_id == i, "ghcn3"], sDF[sDF$sccs_id == i, "ghcn4"],
                           sDF[sDF$sccs_id == i, "ghcn5"], sDF[sDF$sccs_id == i, "ghcn6"],
                           sDF[sDF$sccs_id == i, "ghcn7"], sDF[sDF$sccs_id == i, "ghcn8"],
                           sDF[sDF$sccs_id == i, "ghcn9"])
            ghcn.list <- ghcn.list[!is.na(ghcn.list)]
            ghcn.l <- length(ghcn.list)
            
            # compute consecutive days indices
            ifelse(ghcn.l == 0, print(paste0("No GHCN station for SCCS ", i)), 
                   compute_consecutive_indices_3(s1.doy, e1.doy, s2.doy, e2.doy, 
                                                 s3.doy, e3.doy, s4.doy, e4.doy,
                                                 ghcn.list, i, sourceDir, destDir))
        }
    }
    
}
