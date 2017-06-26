
##############################################################################################################
CoefVar_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    for (thisFile in 1:length(DatFiles)) 
    {
        dir.create(destDir, showWarnings = FALSE)
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        X <- read.table(inName,header=F,sep=" ",col.names=c("year","month",
                                                            "day","prcp"))
        X[X$prcp<=(-99.),"prcp"]<-NA
        
        years <- min(X$year)
        yeare <- max(X$year)
        yearr <- yeare-years
        
        output <- matrix(nrow = (yearr+1), ncol = 7)
        output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
        colnames(output) <- c("climateStart","climateEnd","yrRange","year","STDEV",
                              "meanprcp","coefOfVar")  
        
        for(i in years:yeare){        
            
            mid<-X[X$year==i,"prcp"]
            
            year <- i
            stdev <- round(sd(mid,na.rm=T),digits=2)
            meanpr <- round(mean(mid,na.rm=T),digits=2)
            coef <- round(stdev/meanpr,digits=2)
            
            output$climateStart[i-years+1] <- years
            output$climateEnd[i-years+1] <- yeare
            output$yrRange[i-years+1] <- yearr
            output$year[i-years+1] <- year
            output$STDEV[i-years+1] <- stdev
            output$meanprcp[i-years+1] <- meanpr
            output$coefOfVar[i-years+1] <- coef
        }   #the for statement
        
        write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
        
    }
}
