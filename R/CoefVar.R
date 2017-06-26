##############################################################################################################
##Calculate coefficient of variation (stdev/mean)
CoefVar<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    dir.create(destDir, showWarnings = FALSE)
    
    ### Create a outdf to store all data in one file
    outDF <- matrix(ncol=7, nrow=length(DatFiles))
    outDF <- as.data.frame(outDF)
    colnames(outDF) <- c("GHCN_ID", "Start_yr", "End_yr", "Yr_range",
                         "Daily_stdev", 
                         "Daily_mean", 
                         "Daily_coef_var")
    outDF$GHCN_ID <- sub(".csv", "", DatFiles)
    
    for (i in 1:length(DatFiles)) 
    {
        print(i)
        inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)
        
        dd <- read.csv(inName)
        
        outDF[i,"Start_yr"] <- min(as.numeric(dd$Year))
        outDF[i,"End_yr"] <- max(as.numeric(dd$Year))
        
        outDF[i,"Daily_stdev"] <- sd(dd$value,na.rm=T)
        outDF[i,"Daily_mean"] <- mean(dd$value,na.rm=T)
        
    }
    
    outDF[,"Yr_range"] <- outDF[,"End_yr"] - outDF[,"Start_yr"]
    coef_var <- outDF$Daily_stdev/outDF$Daily_mean
    outDF[,"Daily_coef_var"] <- round(coef_var,digits=2)
    
    write.csv(outDF,paste0(destDir, "/daily_coef_var.csv"))
    
}
