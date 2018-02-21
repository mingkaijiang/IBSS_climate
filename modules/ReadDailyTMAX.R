##############################################################################################################
##Read daily prcp data, within function ConvertFiles
ReadDailyPRCP <- function(filename)
{
    fname <- filename
    X <- readLines(fname)
    Elements <- substr(X, 18, 21)
    dex <- grep("(^TMAX).", Elements)
    X <- X[dex]
    Elements <- Elements[dex]
    Id <- substr(X, 1, 11)
    Year <- as.numeric(substr(X, 12, 15))
    Month <- as.numeric(substr(X, 16, 17))
    days <- matrix(NA, nrow = length(X), ncol = 31)
    Q <- matrix(NA, nrow = length(X), ncol = 31)
    day1column <- 22
    q1column <- 27
    day1end <- 26
    q1end <- 29
    increment <- 8
    for (day in 1:31) 
    {
        days[, day] <- as.numeric(substr(X, day1column + (increment * 
                                                              (day - 1)), day1end + (increment * (day - 1))))
    }
    for (Qflag in 1:31) 
    {
        Q[, Qflag] <- substr(X, q1column + (increment * (Qflag - 
                                                             1)), q1end + (increment * (Qflag - 1)))
    }
    days[days == -9999] <- NA
    Q[Q == "   "] <- NA
    Data <- cbind(Id, Elements, Year, Month, days, Q)
    monthDays <- 1:31
    cnames <- paste("Day", monthDays, sep = "")
    qnames <- paste("Q", monthDays, sep = "")
    cnames <- c(cnames, qnames)
    currentNames <- colnames(Data)
    currentNames[5:ncol(Data)] <- cnames
    colnames(Data) <- currentNames
    return(Data)
}    
