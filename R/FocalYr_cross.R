##############################################################################################################
##Cross check annual coefficient of variation with focal year
##delete files without data
##automatically create directory
FocalYr_cross<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    dir.create(destDir, showWarnings = FALSE)
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName1 <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        unselected <- "E:/IBSS/Output/unselected_focal"
        dir.create(unselected, showWarnings = FALSE)
        outName2 <- file.path(unselected,
                              DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.table(inName,header=T,sep=",")
        
        if (is.numeric(dd$year) == T)
        {
            years <- min(dd$year)
            yeare <- max(dd$year)
            yearf <- dd$Focal_year[2]
            
            if (yearf <= yeare & yearf >= years)
            {  
                write.table(dd,outName1,append=F,quote=F,sep=",",row.names=F,col.names=T)
            }
            else
            {  
                write.table(dd,outName2,append=F,quote=F,sep=",",row.names=F,col.names=T)
            }
        }
        else
            unlink(dd, recursive = F, force=F)
    }
}
