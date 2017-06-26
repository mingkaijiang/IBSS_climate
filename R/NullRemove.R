##############################################################################################################
##Cross check annual coefficient of variation with focal year
##delete files without data
##automatically create directory
NullRemove<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    dir.create(destDir, showWarnings = FALSE)
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.table(inName,header=T,sep=",")
        
        if (is.numeric(dd$year) == T)
        {
            write.table(dd,outName,append=F,quote=F,sep=",",row.names=F,col.names=T)
        }
        else
            unlink(dd, recursive = F, force=F)
    }
}
