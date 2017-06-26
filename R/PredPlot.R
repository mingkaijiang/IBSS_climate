##############################################################################################################
##Plot 30-year moving average of P, M, C, C/P and M/P for dataset larger than 60 yrs.
##Plot the associated focal year onto graphs
PredPlot<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    dir.create(destDir, showWarnings = FALSE)
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- sub(".csv", ".jpeg", outName)
        
        X <- read.table(inName, sep=",", header=T)
        
        jpeg(outName,width=1024,height=768)
        
        par(mfrow=c(2,2))
        
        plot(X$year,X$P, type="b",col = ifelse(X$year == X$Focal_year[2],"black","red"),
             pch = ifelse(X$year == X$Focal_year[2], 19, 1),
             cex = ifelse(X$year == X$Focal_year[2], 2, 1),
             main="predictability",xlab = "year", ylab = "predictability")
        
        plot(X$year,X$C, type="b",col = ifelse(X$year == X$Focal_year[2],"black","green"),
             pch = ifelse(X$year == X$Focal_year[2], 19, 1),
             cex = ifelse(X$year == X$Focal_year[2], 2, 1),
             main="constancy",xlab = "year", ylab = "constancy")
        
        plot(X$year,X$M, type="b",col = ifelse(X$year == X$Focal_year[2],"black","blue"),
             pch = ifelse(X$year == X$Focal_year[2], 19, 1),
             cex = ifelse(X$year == X$Focal_year[2], 2, 1),
             main="contingency",xlab = "year", ylab = "contingency")
        
        plot(X$year,X$CbyP, type="l",col="purple", main="C/P and M/P",xlab = "year", ylab = "proportion",ylim = c(0,1))
        
        lines(X$year,X$MbyP,col="orange")
        
        #    legend(5,5,c("C/P","M/P"),lty=c(1,1),lwd=c(2.5,2.5),col=c("purple","orange"))
        dev.off()
        
    }
}
