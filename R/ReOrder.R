##############################################################################################################
##Function ReOrder orders the dataset according to Year, Month and Day, 
##and output in space delimited format

ReOrder <- function (sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.INPUT.DIRECTORY) 
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.raw")
    for (thisFile in 1:length(DatFiles)) 
    {
        dir.create(destDir, showWarnings = FALSE)
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- sub(".raw", ".csv", outName)
        X <- read.table(inName, stringsAsFactors = FALSE)
        X <- X[order(X$Year, X$Month, X$Day), ]
        write.table(X, outName, sep = " ",row.names=F,col.names=F, na="-99.9")
    }
}
