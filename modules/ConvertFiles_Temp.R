
##############################################################################################################
##Read the input file and extract daily PRCP data
##Need to be in the correct directory

ConvertFiles_Temp <- function(sourceDir = DAILY.FILES.DIRECTORY, 
                         stations,
                         destDir = DAILY.DATA.DIRECTORY) 
{
    files <- paste0(stations, ".dly")
    if (!file.exists(destDir)) 
        dir.create(destDir)
    for (thisFile in 1:length(files)) 
    {
        print(thisFile)     
        X <- ReadDailyTMAX(paste0(sourceDir, files[thisFile]))
        fname <- sub(".dly", ".csv", files[thisFile])
        fname <- file.path(destDir, fname, fsep = .Platform$file.sep)
        write.table(X[,1:35], fname, row.names=F)
    }
}
