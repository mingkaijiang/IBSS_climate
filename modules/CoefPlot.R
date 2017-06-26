##############################################################################################################
##Plot annual trend of coefficient of variation and the associated focal year onto the same graph
CoefPlot<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    for (thisFile in 1:length(DatFiles)) 
    {
        dir.create(destDir, showWarnings = FALSE)
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- sub(".csv", ".jpeg", outName)
        
        X <- read.table(inName, sep=",", header=T)
        
        jpeg(outName,width=1024,height=768)
        #    par(mfrow=c(2,2))
        
        plot(X$year,X$coefOfVar, type="b", 
             col = ifelse(X$year == X$Focal_year[2],"red","black"),
             pch = ifelse(X$year == X$Focal_year[2], 19, 1),
             cex = ifelse(X$year == X$Focal_year[2], 2, 1),
             main = "annual coef of variation", xlab = "year", ylab = "coef. of variation")
        
        dev.off()
        
    }
}
