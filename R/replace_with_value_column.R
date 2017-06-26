##############################################################################################################
replace_with_value_column <- function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)
    
    targList <- list.files(path = sourceDir, pattern = "\\.csv")
    
    
    for (i in 1:length(targList)) 
    {
        inName <- file.path(sourceDir, targList[i], fsep = .Platform$file.sep)
        outName <- file.path(destDir, targList[i], fsep = .Platform$file.sep)
        
        X <- read.csv(inName)
        colnames(X) <- c("id","date", "Year", "Month", "Day", "value")
        write.csv(X[,2:6],outName, row.names=F)
        
        print(targList[i])
    }
}
