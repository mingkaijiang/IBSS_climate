##############################################################################################################
##Restructure the data by transforming the data
##Removed excess days within the year
##Solved leap year problem
##Remove ID, Elements columns
##Remove "Day." from all data entries

ReStructureFile <- function (sourceDir = DAILY.DATA.DIRECTORY, 
                             destDir = DAILY.INPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    dir.create(destDir, showWarnings = FALSE)
    
    for (thisFile in 1:length(DatFiles)) 
    {
        print(thisFile)
        
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        X <- read.table(inName, stringsAsFactors = FALSE,sep=" ", header=T)
        
        if (nrow(X) > 0) {
            melted <- melt(X, id.var = c("Id", "Elements", "Year", "Month"), 
                           variable_name = "Day")
            data_upd <- melted[!(melted$Month=="2" & melted$Day== "Day30"),]
            data_upd <- data_upd[!(data_upd$Month=="2" & data_upd$Day== "Day31"),]
            data_upd <- data_upd[!(data_upd$Month=="4" & data_upd$Day== "Day31"),]
            data_upd <- data_upd[!(data_upd$Month=="6" & data_upd$Day== "Day31"),]
            data_upd <- data_upd[!(data_upd$Month=="9" & data_upd$Day== "Day31"),]
            data_upd <- data_upd[!(data_upd$Month=="11" & data_upd$Day== "Day31"),]    
            data_upd <- data_upd[!(leap_year(data_upd$Year)==FALSE & data_upd$Month=="2" 
                                   & data_upd$Day== "Day29"),]
            
            keeps <- c("Year","Month","Day","value")
            data_upd <- data_upd[keeps]
            data_upd <- as.data.frame(sapply(data_upd,gsub,pattern="Day",replacement=""))
            X <- data_upd
            X$Year <- as.numeric(as.character(X$Year))
            X$Month <- as.numeric(as.character(X$Month))
            X$Day <- as.numeric(as.character(X$Day))
            X$value <- as.numeric(as.character(X$value))
            X[is.na(X)] <- -99.9
            
            X <- X[order(X$Year, X$Month, X$Day), ]
            write.csv(X, outName)
        }
        
    }
}
