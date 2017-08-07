##############################################################################################################
## Filter data for Year range > 10 for long-term trend analysis 
## Also check for missing data issue, missing data should not be > 80%
YrRange10<-function(sourceDir = DAILY.DATA.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.table(inName, sep=",", header=T)
        
        yrrange <- max(as.numeric(dd$Year)) - min(as.numeric(dd$Year))
        
        
        if (yrrange < 10)
        {  
            print(DatFiles[thisFile])
            file.remove(paste0(sourceDir, "/", DatFiles[thisFile]))
        }
        
    }
    
    #write.csv(sDF, "data/sccs_ghcn_station_list_final.csv",
    #          col.names=T, row.names=F, sep=",")
}
