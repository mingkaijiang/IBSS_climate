##############################################################################################################
##Calculate coefficient of variation (stdev/mean)
CoefVar<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    dir.create(destDir, showWarnings = FALSE)
    
    ### Create a outdf to store all data in one file
    outDF <- matrix(ncol=10, nrow=length(DatFiles))
    outDF <- as.data.frame(outDF)
    colnames(outDF) <- c("GHCN_ID", "Start_yr", "End_yr", "Yr_range",
                         "Interannual_stdev", 
                         "Interannual_mean", 
                         "Interannual_coef_var",
                         "Intraannual_stdev", 
                         "Intraannual_mean", 
                         "Intraannual_coef_var")
    outDF$GHCN_ID <- sub(".csv", "", DatFiles)
    
    for (i in 1:length(DatFiles)) 
    {
        print(i)
        inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)
        
        dd <- read.csv(inName)
        
        outDF[i,"Start_yr"] <- min(as.numeric(dd$Year))
        outDF[i,"End_yr"] <- max(as.numeric(dd$Year))
        
        # inter-annual
        yr.list <- c(range(dd$Year)[1]:range(dd$Year)[2])
        tempDF <- data.frame(yr.list, NA)
        names(tempDF) <- c("year", "mean")
        
        for (j in yr.list) {
            tempDF[tempDF$year == j, "mean"] <- mean(dd[dd$Year == j, "value"], na.rm=T)
        }
        
        outDF[i,"Interannual_stdev"] <- sd(tempDF$mean,na.rm=T)
        outDF[i,"Interannual_mean"] <- mean(tempDF$mean,na.rm=T)
        
        # intra-annual
        yr.list <- c(range(dd$Year)[1]:range(dd$Year)[2])
        tempDF <- data.frame(yr.list, NA, NA)
        names(tempDF) <- c("year", "sd", "mean")
        for (j in yr.list) {
            tempDF[tempDF$year == j, "sd"] <- sd(dd[dd$Year == j, "value"], na.rm=T)
            tempDF[tempDF$year == j, "mean"] <- mean(dd[dd$Year == j, "value"], na.rm=T)
        }
        outDF[i, "Intraannual_stdev"] <- mean(tempDF$sd)
        outDF[i, "Intraannual_mean"] <- mean(tempDF$mean)
                
    }
    
    outDF[,"Yr_range"] <- outDF[,"End_yr"] - outDF[,"Start_yr"]
    coef_var <- outDF$Interannual_stdev/outDF$Interannual_mean
    outDF[,"Interannual_coef_var"] <- round(coef_var,digits=2)
    outDF[, "Intraannual_coef_var"] <- outDF[, "Intraannual_stdev"] / outDF[, "Intraannual_mean"]
    
    
    write.csv(outDF,paste0(destDir, "/daily_coef_var.csv"))
    
}
