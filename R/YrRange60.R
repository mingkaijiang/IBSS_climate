##############################################################################################################
YrRange60<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    dir.create(destDir, showWarnings = FALSE)
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName1 <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        unselected <- "E:/IBSS/Output/unselected60"
        dir.create(unselected, showWarnings = FALSE)
        
        outName2 <- file.path(unselected, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.table(inName,header=F,sep=" ")
        
        yrrange <- max(as.numeric(dd$V1)) - min(as.numeric(dd$V1))
        
        if (yrrange > 60)
        {  
            write.table(dd,outName1,append=F,quote=F,sep=" ",na="-99.9",row.names=F,col.names=F)
        }
        else
        {  
            write.table(dd,outName2,append=F,quote=F,sep=" ",na="-99.9",row.names=F,col.names=F)
        }
        
    }
}