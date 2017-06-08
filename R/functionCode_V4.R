###################################Functions for calculating Predictability of extremes#######################
###################################Created by: Mingkai Jiang                           #######################
###################################Examples modified from GhcnDaily, ETCCDI scripts    #######################
###################################Last modified on: 2015-03-02                        #######################
##############################################################################################################
##############################################################################################################
##Version 2.1 of the script to be called by scriptCode_V2.1
##Key updates: 
##1. Modified bin size calculation based on biome specification

##############################################################################################################
##get relevant library functions
library(reshape)
library(reshape2)
library(lubridate) 
library(eeptools)


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

##############################################################################################################
##Calculate coefficient of variation (stdev/mean)
CoefVar<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    dir.create(destDir, showWarnings = FALSE)
    
    ### Create a outdf to store all data in one file
    outDF <- matrix(ncol=7, nrow=length(DatFiles))
    outDF <- as.data.frame(outDF)
    colnames(outDF) <- c("GHCN_ID", "Start_yr", "End_yr", "Yr_range",
                         "Daily_stdev", 
                         "Daily_mean", 
                         "Daily_coef_var")
    outDF$GHCN_ID <- sub(".csv", "", DatFiles)
    
    for (i in 1:length(DatFiles)) 
    {
        print(i)
        inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)

        dd <- read.csv(inName,header=F,skip=1,sep=",",
                       col.names=c("id","year","month","day","prcp"))
        
        outDF[i,"Start_yr"] <- min(as.numeric(dd$year))
        outDF[i,"End_yr"] <- max(as.numeric(dd$year))
        
        outDF[i,"Daily_stdev"] <- sd(dd$prcp,na.rm=T)
        outDF[i,"Daily_mean"] <- mean(dd$prcp,na.rm=T)

    }
    
    outDF[,"Yr_range"] <- outDF[,"End_yr"] - outDF[,"Start_yr"]
    coef_var <- outDF$Daily_stdev/outDF$Daily_mean
    outDF[,"Daily_coef_var"] <- round(coef_var,digits=2)
    
    write.csv(outDF,paste0(destDir, "/daily_coef_var.csv"))
    
}

##############################################################################################################
CoefVar_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    dir.create(destDir, showWarnings = FALSE)
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    X <- read.table(inName,header=F,sep=" ",col.names=c("year","month",
                                                        "day","prcp"))
    X[X$prcp<=(-99.),"prcp"]<-NA
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    output <- matrix(nrow = (yearr+1), ncol = 7)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("climateStart","climateEnd","yrRange","year","STDEV",
                          "meanprcp","coefOfVar")  
    
    for(i in years:yeare){        
      
      mid<-X[X$year==i,"prcp"]
      
      year <- i
      stdev <- round(sd(mid,na.rm=T),digits=2)
      meanpr <- round(mean(mid,na.rm=T),digits=2)
      coef <- round(stdev/meanpr,digits=2)
      
      output$climateStart[i-years+1] <- years
      output$climateEnd[i-years+1] <- yeare
      output$yrRange[i-years+1] <- yearr
      output$year[i-years+1] <- year
      output$STDEV[i-years+1] <- stdev
      output$meanprcp[i-years+1] <- meanpr
      output$coefOfVar[i-years+1] <- coef
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}


##############################################################################################################
##Read the input file and extract daily PRCP data
##Need to be in the correct directory

ConvertFiles <- function(sourceDir = DAILY.FILES.DIRECTORY, 
                         stations,
                         destDir = DAILY.DATA.DIRECTORY) 
{
  files <- paste0(stations, ".dly")
  if (!file.exists(destDir)) 
    dir.create(destDir)
  for (thisFile in 1:length(files)) 
  {
    print(thisFile)     
    X <- ReadDailyPRCP(paste0(sourceDir, files[thisFile]))
    fname <- sub(".dly", ".csv", files[thisFile])
    fname <- file.path(destDir, fname, fsep = .Platform$file.sep)
    write.table(X[,1:35], fname, row.names=F)
  }
}


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

##############################################################################################################
##calculate leap years

leapyear<-function(year)
{
  remainder400 <-trunc(year-400*trunc(year/400))
  remainder100 <-trunc(year-100*trunc(year/100))
  remainder4 <-trunc(year-4*trunc(year/4))
  if (remainder400 == 0) leapyear = T
  else
  {
    if(remainder100 == 0) leapyear = F
    else
    {
      if(remainder4 == 0) leapyear = T
      else leapyear = F
    }
  }
}

##############################################################################################################
##Sort the data as descending order
mysort<- function(x,decreasing){sort.int(x,decreasing=decreasing)}

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

##############################################################################################################
##Calculate percentiles
percentile<-function(n,x,pctile)
  {
  x1 <- x[is.na(x)==F]
  n1 <- length(x1)
  a <- mysort(x1,decreasing=F)
  b <- n1*pctile+0.3333*pctile+0.3333
  bb <- trunc(b)
  percentile<-a[bb]+(b-bb)*(a[bb+1]-a[bb]) 
  }
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

##############################################################################################################
##Calculate predictability of PRCPTOT for whole temporal range
PRCPTOTS_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_PRCPTOTS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "prcptot_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "prcptot_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "prcptot_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "prcptot_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "prcptot_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}


##############################################################################################################
##Calculate predictability of 30-year running mean of prcptots based on > 60 year dataset
PRCPTOTS_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_PRCPTOTS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$prcptot_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$prcptot_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$prcptot_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$prcptot_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$prcptot_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}
##############################################################################################################
##Calculate predictability for R05P based on whole temporal range
R01PS_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_R01PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "r01p_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "r01p_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "r01p_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "r01p_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "r01p_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}

##############################################################################################################
##Calculate predictability of 30-year running mean of R01PS based on > 60 year dataset
R01PS_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_R01PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$r01p_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$r01p_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$r01p_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$r01p_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$r01p_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}


##############################################################################################################
##Calculate predictability for R05P based on whole temporal range
R05PS_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_R05PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "r05p_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "r05p_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "r05p_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "r05p_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "r05p_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}

##############################################################################################################
##Calculate predictability of 30-year running mean of R05PS based on > 60 year dataset
R05PS_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_R05PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$r05p_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$r05p_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$r05p_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$r05p_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$r05p_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}

##############################################################################################################
##Calculate R10 predictability for whole temporal scale
R10S_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    dir.create(destDir, showWarnings = FALSE)
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    ### Create a outdf to store all data in one file
    output <- matrix(ncol=22, nrow=length(DatFiles))
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("GHCN_ID","Start_yr","End_yr","Yr_count",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    output$GHCN_ID <- sub(".csv", "", DatFiles)
    
    for (i in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)
        
        X <- read.csv(inName)
        
        years <- min(X$year)
        yeare <- max(X$year)
        yearr <- yeare-years
        
        # site specific data range
        v.range <- c(X$R10_spr, X$R10_sum, 
                     X$R10_aut, X$R10_win)
        
        max_top <- round_any(max(v.range), 10, f = ceiling)
        min_bot <- round_any(min(v.range), 10, f = floor)
        diff <- max_top - min_bot
        interval <- 10
        
        
        bin <- matrix(0, ncol=6, nrow=interval)
        dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
        
        bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                              min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                              min_bot+0.9*diff,max_top)
        
        breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                   min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                   min_bot+0.9*diff,max_top)
        
        
        spr_cut = cut(X[, "R10_spr"], breaks, include.lowest=TRUE,right=TRUE)
        sum_cut = cut(X[, "R10_sum"], breaks, include.lowest=TRUE,right=TRUE)
        aut_cut = cut(X[, "R10_aut"], breaks, include.lowest=TRUE,right=TRUE)
        win_cut = cut(X[, "R10_win"], breaks, include.lowest=TRUE,right=TRUE)
        
        spr_freq = table(spr_cut)
        sum_freq = table(sum_cut)
        aut_freq = table(aut_cut)
        win_freq = table(win_cut)
        
        bin[,"spr"] <- spr_freq
        bin[,"sum"] <- sum_freq
        bin[,"aut"] <- aut_freq
        bin[,"win"] <- win_freq
        
        bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
        
        col_sum <- sum(table(X[, "R10_spr"]))
        whole_sum <- col_sum*4
        
        
        #uncertainty with respect to time H(X)
        HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
        
        #uncertainty with respect to state H(Y)
        V1 <- bin[,"whole"]/whole_sum
        V2 <- log10(bin[,"whole"]/whole_sum)
        for (i in 1:length(V2))
        {
            if(is.finite(V2[i])==F) V2[i] <- 0
            else V2[i] <- V2[i]
        }
        
        HofY <- -sum(V1*V2)
        
        #uncertainty with respect to interaction of time and state, H(XY)
        M1 <- bin[1:interval,2:5]/whole_sum
        M2 <- log10(M1)
        for (i in 1:length(M2))
        {
            if(is.finite(M2[i])==F) M2[i] <- 0
            else M2[i] <- M2[i]
        }
        
        HofXY <- -sum(M1*M2)
        
        #Conditional uncertainty with regard to state, with time given, HXofY
        HXofY <- HofXY - HofX
        s <- interval
        t <- 4
        
        #predictability (P), constancy(C) and contingency (M)
        P <- 1-(HXofY/log10(s))
        C <- 1-(HofY/log10(s))
        M <- (HofX+HofY-HofXY)/log10(s)
        CoverP <- C/P
        MoverP <- M/P
        
        #mutual information, I(XY)
        IofXY <- HofY - HXofY
        
        #deviation from homogeneity of the columns of the matrix for constancy, GC
        GC <- 2*whole_sum*(log(s)-HofY)
        C_free <- s-1
        
        #deviation from homogeneity of the columns of the matrix for contingency, GM
        GM <- 2*whole_sum*(HofX+HofY-HofXY)
        M_free <- (s-1)*(t-1)
        
        #deviation from homogeneity of the columns of the matrix for predictability, GP
        GP <- GM + GC
        P_free <- (s-1)*t
        
        output[i,"Start_yr"] <- years
        output[i,"End_yr"] <- yeare
        output[i,"Yr_count"] <- col_sum 
        

        output[i,"HofX"] <- HofX
        output[i,"HofY"] <- HofY
        output[i,"HofXY"] <- HofXY
        output[i,"HXofY"] <- HXofY
        output[i,"s"] <- s
        output[i,"t"] <- t
        output[i,"P"] <- P
        output[i,"C"] <- C
        output[i,"M"] <- M
        output[i,"CbyP"] <- CoverP
        output[i,"MbyP"] <- MoverP
        output[i,"Mutual"] <- IofXY
        output[i,"GC"] <- GC
        output[i,"C_freedom"] <- C_free
        output[i,"GM"] <- GM
        output[i,"M_freedom"] <- M_free
        output[i,"GP"] <- GP
        output[i,"P_freedom"] <- P_free
    }
    
    write.csv(output,paste0(destDir, "/R10S_PCM.csv"),row.names=F)
  
 }
##############################################################################################################
##Calculate predictability of 30-year moving average of R10S based on > 60 year dataset
R10S_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_R10S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$R10_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$R10_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$R10_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$R10_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$R10_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}

##############################################################################################################
##Calculate predictability for R20 for whole temporal range
R20S_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_R20S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T, quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "R20_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "R20_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "R20_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "R20_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "R20_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}


##############################################################################################################
##Calculate predictability of 30-year running average of R20S based on > 60 year dataset
R20S_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_R20S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 10, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 10, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$R20_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$R20_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$R20_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$R20_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$R20_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}



##############################################################################################################
##Calculate predictability for R95P based on whole temporal range
R95PS_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_R95PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "r95p_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "r95p_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "r95p_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "r95p_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "r95p_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}

##############################################################################################################
##Calculate predictability of 30-year running mean of R95PS based on > 60 year dataset
R95PS_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_R95PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$r95p_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$r95p_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$r95p_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$r95p_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$r95p_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}

##############################################################################################################
##Calculate predictability for R99P based on whole temporal range
R99PS_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_R99PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "r99p_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "r99p_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "r99p_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "r99p_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "r99p_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}

##############################################################################################################
##Calculate predictability of 30-year running mean of R99PS based on > 60 year dataset
R99PS_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_R99PS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$r99p_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$r99p_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$r99p_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$r99p_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$r99p_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}


##############################################################################################################
##Read daily prcp data, within function ConvertFiles
ReadDailyPRCP <- function(filename)
{
  fname <- filename
  X <- readLines(fname)
  Elements <- substr(X, 18, 21)
  dex <- grep("(^PR).", Elements)
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
  for (thisFile in 1:length(DatFiles)) 
  {
    print(files[thisFile])
    dir.create(destDir, showWarnings = FALSE)
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    #        outName <- sub(".raw", ".txt", DatFiles[thisFile])
    X <- read.table(inName, stringsAsFactors = FALSE,sep=" ", header=T)
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
    X <- X[order(X$Year, X$Month, X$Day), ]
    write.csv(X, outName)
  }
}

##############################################################################################################
##Calculate monthly max 1-d prcp
RX1Day<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    dir.create(destDir, showWarnings = FALSE)
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    dd <- read.table(inName,header=F,col.names=c("year","month","day","prcp"),
                     colClasses=rep("real",4))
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
#    assign("nama",nama,envir=.GlobalEnv)
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
#    assign("ofilename",ofilename,envir=.GlobalEnv)
    
    dd[dd$prcp<=(-99.),"prcp"]<-NA
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)

    startpoint <- startyear-1

    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    len<-yeare-years+1
    aa1<-matrix(NA,12*len,3)
    dimnames(aa1)<-list(NULL,c("year","month","rx1d"))
    aa1[,"year"]<-years:yeare
    aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
    aa1[,"month"]<-1:12
    jjj3=1
    mid<-dd
    for (year in years:yeare)
    {
      aaaa<-mid[mid$year==year,]
      aaaam1<-aaaa[aaaa$month==1,"prcp"]
      aaaam2<-aaaa[aaaa$month==2,"prcp"]
      aaaam3<-aaaa[aaaa$month==3,"prcp"]
      aaaam4<-aaaa[aaaa$month==4,"prcp"]
      aaaam5<-aaaa[aaaa$month==5,"prcp"]
      aaaam6<-aaaa[aaaa$month==6,"prcp"]
      aaaam7<-aaaa[aaaa$month==7,"prcp"]
      aaaam8<-aaaa[aaaa$month==8,"prcp"]
      aaaam9<-aaaa[aaaa$month==9,"prcp"]
      aaaam10<-aaaa[aaaa$month==10,"prcp"]
      aaaam11<-aaaa[aaaa$month==11,"prcp"]
      aaaam12<-aaaa[aaaa$month==12,"prcp"]
      aa1[jjj3,"rx1d"]<-max(aaaam1,na.rm=T)
      aa1[jjj3+1,"rx1d"]<-max(aaaam2,na.rm=T)
      aa1[jjj3+2,"rx1d"]<-max(aaaam3,na.rm=T)
      aa1[jjj3+3,"rx1d"]<-max(aaaam4,na.rm=T)
      aa1[jjj3+4,"rx1d"]<-max(aaaam5,na.rm=T)
      aa1[jjj3+5,"rx1d"]<-max(aaaam6,na.rm=T)
      aa1[jjj3+6,"rx1d"]<-max(aaaam7,na.rm=T)
      aa1[jjj3+7,"rx1d"]<-max(aaaam8,na.rm=T)
      aa1[jjj3+8,"rx1d"]<-max(aaaam9,na.rm=T)
      aa1[jjj3+9,"rx1d"]<-max(aaaam10,na.rm=T)
      aa1[jjj3+10,"rx1d"]<-max(aaaam11,na.rm=T)
      aa1[jjj3+11,"rx1d"]<-max(aaaam12,na.rm=T)
      jjj3=jjj3+12
    }
    
    ofile<-matrix(0,len,14)
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr",
                                 "may","jun","jul","aug","sep","oct","nov",
                                 "dec","annual"))
    ofile<-as.data.frame(ofile)
    
    for(j in years:yeare)
    {
      k <- j-years+1
      ofile[k,1] <- j
      ofile[k,2:13]<-aa1[aa1[,1]==j,3]
      ofile[k,14]<-max(ofile[k,2:13],na.rm=F)
    }
    
    write.table(ofile,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
  }
}

##############################################################################################################
##Calculate max seasonal 1-d prcp
RX1S<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  dir.create(destDir, showWarnings = FALSE)
  
  
  for (thisFile in 1:length(DatFiles)) 
  {
      print(thisFile)
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    dd <- read.csv(inName,header=F,skip=1,col.names=c("id", "year","month","day","prcp"))
    
    dd <- dd[,2:5]
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
    
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
    

    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) {
        dddd<-dddl   
    } else {
        dddd <- ddd
    }
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) {
          dddd1 <- dddl
      } else {
          dddd1 <- ddd
      }
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)
    
    startpoint <- startyear-1
    endpoint <- endyear+1
    
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    len<-yeare-years+1
    aa1<-matrix(NA,4*len,3)
    dimnames(aa1)<-list(NULL,c("year","season","rx1d"))
    aa1[,"year"]<-years:yeare
    aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
    aa1[,"season"]<-1:4
    jjj3=1
    mid<-dd
    for (year in years:yeare)
    {
      aaaa<-mid[mid$year==year,]
      aaaas1<-aaaa[aaaa$month >= 3 & aaaa$month <= 5,"prcp"]
      aaaas2<-aaaa[aaaa$month >= 6 & aaaa$month <= 8,"prcp"]
      aaaas3<-aaaa[aaaa$month >= 9 & aaaa$month <= 11,"prcp"]
      aaaas4<-aaaa[aaaa$month == 12 | aaaa$month == 1 | aaaa$month == 2,"prcp"]
      aa1[jjj3,"rx1d"]<-max(aaaas1,na.rm=T)
      aa1[jjj3+1,"rx1d"]<-max(aaaas2,na.rm=T)
      aa1[jjj3+2,"rx1d"]<-max(aaaas3,na.rm=T)
      aa1[jjj3+3,"rx1d"]<-max(aaaas4,na.rm=T)
      jjj3=jjj3+4
    }
    
    ofile<-matrix(0,len,6)
    dimnames(ofile)<-list(NULL,c("year","spr","sum","aut","win","annual"))
    ofile<-as.data.frame(ofile)
    
    for(j in years:yeare)
    {
      k <- j-years+1
      ofile[k,1] <- j
      ofile[k,2:5]<-aa1[aa1[,1]==j,3]
      ofile[k,6]<-max(ofile[k,2:6],na.rm=F)
    }       
    
    write.csv(ofile,outName, row.names=F)
  }
  
}

##############################################################################################################
##Calculate RX1S predictability based on whole temporal range
RX1S_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_RX1S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}

##############################################################################################################
##Calculate predictability of 30-year moving average of RX1S based on >60 year dataset
RX1S_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_RX1S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
       
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    

    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")

    for(j in 30:yearr){        
      
      spr_cut = cut(X$spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t

      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}


##############################################################################################################
##Calculate max monthly 5-d prcp
RX5Day<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    dd <- read.table(inName,header=F,col.names=c("year","month","day","prcp"),
                     colClasses=rep("real",4))
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
#    assign("nama",nama,envir=.GlobalEnv)
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
#    assign("ofilename",ofilename,envir=.GlobalEnv)
    
    dd[dd$prcp<=(-99.),"prcp"]<-NA
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)
    
    startpoint <- startyear-1

    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    a2<-c(0,0,0,0)
    a1<-dd[,"prcp"]
    a1<-append(a2,a1)
    n<-length(a1)
    a<-rep(0,n)
    for (i in 5:n)
    {
      a[i]<-sum(a1[(i-4):i],na.rm=T)
    }
    a<-a[-(1:4)]
    a<-cbind(dd[,1:2],a)
    
    len<-yeare-years+1
    aa1<-matrix(NA,12*len,3)
    dimnames(aa1)<-list(NULL,c("year","month","rx5d"))
    aa1[,"year"]<-years:yeare
    aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
    aa1[,"month"]<-1:12
    jjj2=1
    for (year in years:yeare)
    {
      aaaa<-a[a$year==year,]
      aaaam1<-aaaa[aaaa$month==1,"a"]
      aaaam2<-aaaa[aaaa$month==2,"a"]
      aaaam3<-aaaa[aaaa$month==3,"a"]
      aaaam4<-aaaa[aaaa$month==4,"a"]
      aaaam5<-aaaa[aaaa$month==5,"a"]
      aaaam6<-aaaa[aaaa$month==6,"a"]
      aaaam7<-aaaa[aaaa$month==7,"a"]
      aaaam8<-aaaa[aaaa$month==8,"a"]
      aaaam9<-aaaa[aaaa$month==9,"a"]
      aaaam10<-aaaa[aaaa$month==10,"a"]
      aaaam11<-aaaa[aaaa$month==11,"a"]
      aaaam12<-aaaa[aaaa$month==12,"a"]
      aa1[jjj2,"rx5d"]<-max(aaaam1,na.rm=T)
      aa1[jjj2+1,"rx5d"]<-max(aaaam2,na.rm=T)
      aa1[jjj2+2,"rx5d"]<-max(aaaam3,na.rm=T)
      aa1[jjj2+3,"rx5d"]<-max(aaaam4,na.rm=T)
      aa1[jjj2+4,"rx5d"]<-max(aaaam5,na.rm=T)
      aa1[jjj2+5,"rx5d"]<-max(aaaam6,na.rm=T)
      aa1[jjj2+6,"rx5d"]<-max(aaaam7,na.rm=T)
      aa1[jjj2+7,"rx5d"]<-max(aaaam8,na.rm=T)
      aa1[jjj2+8,"rx5d"]<-max(aaaam9,na.rm=T)
      aa1[jjj2+9,"rx5d"]<-max(aaaam10,na.rm=T)
      aa1[jjj2+10,"rx5d"]<-max(aaaam11,na.rm=T)
      aa1[jjj2+11,"rx5d"]<-max(aaaam12,na.rm=T)
      jjj2=jjj2+12
    }
    
    ofile<-matrix(0,len,14)
    
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr","may",
                                 "jun","jul","aug","sep","oct","nov","dec",
                                 "annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare)
    {
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:13]<-aa1[aa1[,1]==j,"rx5d"]
      ofile[k,14]<-max(ofile[k,2:13],na.rm=F)
    }
    
    write.table(ofile,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
  }
}

##############################################################################################################
##Calculate seasonal max 5-d prcp

RX5S<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
      print(thisFile)
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    dd <- read.csv(inName,header=F,skip=1,col.names=c("id","year","month","day","prcp"))
    dd <- dd[,2:5]
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
    
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
    
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)
    
    startpoint <- startyear-1
    
    endpoint <- endyear+1
    
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    a2<-c(0,0,0,0)
    a1<-dd[,"prcp"]
    a1<-append(a2,a1)
    n<-length(a1)
    a<-rep(0,n)
    for (i in 5:n)
    {
      a[i]<-sum(a1[(i-4):i],na.rm=T)
    }
    a<-a[-(1:4)]
    a<-cbind(dd[,1:2],a)
    
    len<-yeare-years+1
    aa1<-matrix(NA,4*len,3)
    dimnames(aa1)<-list(NULL,c("year","season","rx5d"))
    aa1[,"year"]<-years:yeare
    aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
    aa1[,"season"]<-1:4
    jjj2=1
    for (year in years:yeare)
    {
      aaaa<-a[a$year==year,]
      aaaas1<-aaaa[aaaa$month >= 3 & aaaa$month <= 5,"a"]
      aaaas2<-aaaa[aaaa$month >= 6 & aaaa$month <= 8,"a"]
      aaaas3<-aaaa[aaaa$month >= 9 & aaaa$month <= 11,"a"]
      aaaas4<-aaaa[aaaa$month == 12 | aaaa$month == 1 | aaaa$month == 2,"a"]
      aa1[jjj2,"rx5d"]<-max(aaaas1,na.rm=T)
      aa1[jjj2+1,"rx5d"]<-max(aaaas2,na.rm=T)
      aa1[jjj2+2,"rx5d"]<-max(aaaas3,na.rm=T)
      aa1[jjj2+3,"rx5d"]<-max(aaaas4,na.rm=T)
      jjj2=jjj2+4
    }
    
    ofile<-matrix(0,len,6)
    
    dimnames(ofile)<-list(NULL,c("year","spr","sum","aut","win","annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare)
    {
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:5]<-aa1[aa1[,1]==j,"rx5d"]
      ofile[k,6]<-max(ofile[k,2:6],na.rm=F)
    }
    
    write.csv(ofile,outName,row.names=F)
  }
}

##############################################################################################################
##Calculate RX5S predictability based on whole temporal range
RX5S_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_RX5S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}


##############################################################################################################
##Calculate predictability of 30-year moving average of RX5S based on > 60 year dataset
RX5S_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_RX5S.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}

##############################################################################################################
##Calculate sum of tot prcp of the year/# of wet days (i.e. prcp > 1mm)
SDII<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    dd <- read.table(inName,header=F,col.names=c("year","month","day","prcp"),
                     colClasses=rep("real",4))
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
#    assign("nama",nama,envir=.GlobalEnv)
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
#    assign("ofilename",ofilename,envir=.GlobalEnv)
    
    dd[dd$prcp<=(-99.),"prcp"]<-NA
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)

    startpoint <- startyear-1

    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    ys=yeare-years+1
    b<-matrix(0,ncol=2,nrow=ys)
    dimnames(b)<-list(NULL,c("year","sdii"))
    b[,"year"]<-c(years:yeare)
    b<-as.data.frame(b)
    year=years
    for (i in 1:ys)
    {
      mid<-dd[dd$year==year,"prcp"]
      mid<-mid[mid>=1]
      b[i,"sdii"]<-mean(mid,na.rm=T)
      year=year+1  
    }
    
    write.table(b,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
  }
}


##############################################################################################################
##Calculate prcp/#of wet days over each season
SDIIS<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    dd <- read.csv(inName,header=F,skip=1,col.names=c("id","year","month","day","prcp"))
    dd <- dd[,2:5]
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
    
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
    
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)
    
    startpoint <- startyear-1
    
    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    ys=yeare-years+1
    b<-matrix(0,ncol=5,nrow=ys)
    dimnames(b)<-list(NULL,c("year","sdii_spr","sdii_sum","sdii_aut","sdii_win"))
    b[,"year"]<-c(years:yeare)
    b<-as.data.frame(b)
    year=years
    for (i in 1:ys)
    {
      mid_spr<-dd[dd$year==year & dd$month >= 3 & dd$month <= 5,"prcp"]
      mid_spr<-mid_spr[mid_spr>=1]
      b[i,"sdii_spr"]<-mean(mid_spr,na.rm=T)
      
      mid_sum<-dd[dd$year==year & dd$month >= 6 & dd$month <= 8,"prcp"]
      mid_sum<-mid_sum[mid_sum>=1]
      b[i,"sdii_sum"]<-mean(mid_sum,na.rm=T)
      
      mid_aut<-dd[dd$year==year & dd$month >= 9 & dd$month <= 11,"prcp"]
      mid_aut<-mid_aut[mid_aut>=1]
      b[i,"sdii_aut"]<-mean(mid_aut,na.rm=T)
      
      mid_win<-dd[dd$year==year & (dd$month ==12 | dd$month == 1 | dd$month == 2),"prcp"]
      mid_win<-mid_win[mid_win>=1]
      b[i,"sdii_win"]<-mean(mid_win,na.rm=T)
      
      year=year+1  
    }
    
    write.csv(b,outName,row.names=F)
  }
}


##############################################################################################################
##Calculate SDIIS predictability based on whole temporal range
SDIIS_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng/biome_SDIIS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    X[X$sdii_spr<=(-99.),"sdii_spr"]<-NA
    X[X$sdii_sum<=(-99.),"sdii_sum"]<-NA
    X[X$sdii_aut<=(-99.),"sdii_aut"]<-NA
    X[X$sdii_win<=(-99.),"sdii_win"]<-NA
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(ncol=32)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","startyear","endyear","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    spr_cut = cut(X[X$WWF_MHTNUM == i, "sdii_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[X$WWF_MHTNUM == i, "sdii_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[X$WWF_MHTNUM == i, "sdii_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[X$WWF_MHTNUM == i, "sdii_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[X$WWF_MHTNUM == i, "sdii_spr"]))
    whole_sum <- col_sum*4
    
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
    
    #uncertainty with respect to state H(Y)
    V1 <- bin[,"whole"]/whole_sum
    V2 <- log10(bin[,"whole"]/whole_sum)
    for (i in 1:length(V2))
    {
      if(is.finite(V2[i])==F) V2[i] <- 0
      else V2[i] <- V2[i]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- bin[1:interval,2:5]/whole_sum
    M2 <- log10(M1)
    for (i in 1:length(M2))
    {
      if(is.finite(M2[i])==F) M2[i] <- 0
      else M2[i] <- M2[i]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 4
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output$Site_ID <- X$Site_ID[2]
    output$Target_FID <- X$TARGET_FID[2]
    output$Lat <- X$Lat[2]
    output$Long <- X$Long[2]
    output$Elev <- X$Elev[2]
    output$Name <- X$Name[2]
    output$SCCS_ID <- X$SCCS_ID[2]
    output$Focal_year <- X$Focal_year[2]
    output$WWF_Num <- X$WWF_MHTNUM[2]
    output$WWF_Name <- X$WWF_MHTNAM[2]
    output$startyear <- years
    output$endyear <- yeare
    output$year_count <- col_sum 
    output$seasons <- whole_sum
    output$HofX <- HofX
    output$HofY <- HofY
    output$HofXY <- HofXY
    output$HXofY <- HXofY
    output$s <- s
    output$t <- t
    output$P <- P
    output$C <- C
    output$M <- M
    output$CbyP <- CoverP
    output$MbyP <- MoverP
    output$Mutual <- IofXY
    output$GC <- GC
    output$C_freedom <- C_free
    output$GM <- GM
    output$M_freedom <- M_free
    output$GP <- GP
    output$P_freedom <- P_free
    
    write.table(output,outName,append=F,sep=",",row.names=F)
  }
}

##############################################################################################################
##Calculate predictability of 30-year running mean of SDIIS based on > 60 year dataset
SDIIS_pred_move<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    biome_prcp <- read.table("E:/IBSS/Output/biome_prcp_rng_60/biome_SDIIS.csv",header=T,sep=",")
    X <- read.table(inName, sep=",", header=T,quote="")
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    i <- X$WWF_MHTNUM[2]
    
    max_top <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmax"], 1000, f = ceiling)
    min_bot <- round_any(biome_prcp[biome_prcp$WWF_Num == i, "prcpmin"], 1000, f = floor)
    diff <- max_top - min_bot
    interval <- 10
    
    
    bin <- matrix(0, ncol=6, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","spr","sum","aut","win","whole"))
    
    bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                          min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                          min_bot+0.9*diff,max_top)
    
    breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
               min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
               min_bot+0.9*diff,max_top)
    
    
    output <- matrix(nrow=(yearr-30),ncol=31)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("Site_ID","Target_ID","Lat","Long","Elev","Name","SCCS_ID","Focal_year",
                          "WWF_Num","WWF_Name","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
    
    for(j in 30:yearr){        
      
      spr_cut = cut(X$sdii_spr[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      sum_cut = cut(X$sdii_sum[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      aut_cut = cut(X$sdii_aut[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      win_cut = cut(X$sdii_win[(j-30):j], breaks, include.lowest=TRUE,right=TRUE)
      
      spr_freq = table(spr_cut)
      sum_freq = table(sum_cut)
      aut_freq = table(aut_cut)
      win_freq = table(win_cut)
      
      bin[,"spr"] <- spr_freq
      bin[,"sum"] <- sum_freq
      bin[,"aut"] <- aut_freq
      bin[,"win"] <- win_freq
      
      bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
      
      col_sum <- sum(table(X$sdii_spr[(j-30):j]))
      whole_sum <- col_sum*4
      
      #uncertainty with respect to time H(X)
      HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*4
      
      #uncertainty with respect to state H(Y)
      V1 <- bin[,"whole"]/whole_sum
      V2 <- log10(bin[,"whole"]/whole_sum)
      for (k in 1:length(V2))
      {
        if(is.finite(V2[k])==F) V2[k] <- 0
        else V2[k] <- V2[k]
      }
      
      HofY <- -sum(V1*V2)
      
      #uncertainty with respect to interaction of time and state, H(XY)
      M1 <- bin[1:interval,2:5]/whole_sum
      M2 <- log10(M1)
      for (k in 1:length(M2))
      {
        if(is.finite(M2[k])==F) M2[k] <- 0
        else M2[k] <- M2[k]
      }
      
      HofXY <- -sum(M1*M2)
      
      #Conditional uncertainty with regard to state, with time given, HXofY
      HXofY <- HofXY - HofX
      s <- interval
      t <- 4
      
      #predictability (P), constancy(C) and contingency (M)
      P <- 1-(HXofY/log10(s))
      C <- 1-(HofY/log10(s))
      M <- (HofX+HofY-HofXY)/log10(s)
      CoverP <- C/P
      MoverP <- M/P
      
      #mutual information, I(XY)
      IofXY <- HofY - HXofY
      
      #deviation from homogeneity of the columns of the matrix for constancy, GC
      GC <- 2*whole_sum*(log(s)-HofY)
      C_free <- s-1
      
      #deviation from homogeneity of the columns of the matrix for contingency, GM
      GM <- 2*whole_sum*(HofX+HofY-HofXY)
      M_free <- (s-1)*(t-1)
      
      #deviation from homogeneity of the columns of the matrix for predictability, GP
      GP <- GM + GC
      P_free <- (s-1)*t
      
      output$Site_ID <- X$Site_ID[2]
      output$Target_FID <- X$TARGET_FID[2]
      output$Lat <- X$Lat[2]
      output$Long <- X$Long[2]
      output$Elev <- X$Elev[2]
      output$Name <- X$Name[2]
      output$SCCS_ID <- X$SCCS_ID[2]
      output$Focal_year <- X$Focal_year[2]
      output$WWF_Num <- X$WWF_MHTNUM[2]
      output$WWF_Name <- X$WWF_MHTNAM[2]
      output$year[j-30] <- (j+years)
      output$year_count[j-30] <- col_sum 
      output$seasons[j-30] <- whole_sum
      output$HofX[j-30] <- HofX
      output$HofY[j-30] <- HofY
      output$HofXY[j-30] <- HofXY
      output$HXofY[j-30] <- HXofY
      output$s[j-30] <- s
      output$t[j-30] <- t
      output$P[j-30] <- P
      output$C[j-30] <- C
      output$M[j-30] <- M
      output$CbyP[j-30] <- CoverP
      output$MbyP[j-30] <- MoverP
      output$Mutual[j-30] <- IofXY
      output$GC[j-30] <- GC
      output$C_freedom[j-30] <- C_free
      output$GM[j-30] <- GM
      output$M_freedom[j-30] <- M_free
      output$GP[j-30] <- GP
      output$P_freedom[j-30] <- P_free
    }   #the for statement
    
    write.table(output,outName,append=F,quote=F,sep=",",row.names=F)
    
  }
}

##############################################################################################################

##Threshold-based indices
##including r95p, r99p, prcptot, r10, r20

ThrInd<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) 
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    #        outName <- sub(".csv", ".txt", outName)
    dd <- read.table(inName,header=F,col.names=c("year","month","day","prcp"),
                     colClasses=rep("real",4))
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
#    assign("nama",nama,envir=.GlobalEnv)
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
#    assign("ofilename",ofilename,envir=.GlobalEnv)
    
    dd[dd$prcp<=(-99.),"prcp"]<-NA
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)

    startpoint <- startyear-1

    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    prcptmp<-dd[dd$year >= startyear 
                & dd$year <= endyear 
                & dd$prcp>=1,"prcp"]
    
    prcptmp<-prcptmp[is.na(prcptmp)==F]
    
    len<-length(prcptmp)
    prcp95<-percentile(len,prcptmp,0.95)
    prcp99<-percentile(len,prcptmp,0.99)
    
    ys<-yeare-years+1
    
    dp<-matrix(0,ys,4)
    dimnames(dp)<-list(NULL,c("year","r95p","r99p","prcptot"))
    dp[,"year"]<-years:yeare
    for(i in years:yeare)
    {
      dp[(i-years+1),"r95p"]<-sum(dd[dd$year==i&dd$prcp>prcp95,"prcp"],na.rm=T)
      dp[(i-years+1),"r99p"]<-sum(dd[dd$year==i&dd$prcp>prcp99,"prcp"],na.rm=T)
      dp[(i-years+1),"prcptot"]<-sum(dd[dd$year==i&dd$prcp>=1,"prcp"],na.rm=T)
    }
    dp<-as.data.frame(dp)
    R20<-rep(0,ys)
    target<-as.data.frame(cbind(dp,R20))
    for (year in years:yeare)
    {
      mid<-dd[dd$year==year,"prcp"]
      mid<-mid[is.na(mid)==F]
      target[target$year==year,"R20"]<-length(mid[mid>=20])
    }
    R10<-rep(0,ys)
    target2<-as.data.frame(cbind(target,R10))
    for (year in years:yeare)
    {
      mid<-dd[dd$year==year,"prcp"]
      mid<-mid[is.na(mid)==F]
      target2[target2$year==year,"R10"]<-length(mid[mid>=10])
    }
    write.table(target2,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
  }
}


##############################################################################################################
ThrIndS<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) 
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    dd <- read.csv(inName,header=F,skip=1,col.names=c("id","year","month","day","prcp"))
    dd <- dd[,2:5]
    
    nama<-substr(inName,start=1,stop=(nchar(inName)-4))
    outdirtmp <- strsplit(inName,"/")[[1]]
    
    ofilename<-substr(outdirtmp[length(outdirtmp)],
                      start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
    
    
    ddd<-matrix(NA,365,4)
    dddl<-matrix(NA,366,4)
    dimnames(ddd)<-list(NULL,c("year","month","day","prcp"))
    dimnames(dddl)<-list(NULL,c("year","month","day","prcp"))
    
    ddd[1:31,"month"]<-1
    ddd[1:31,"day"]<-c(1:31)
    ddd[32:59,"month"]<-2
    ddd[32:59,"day"]<-c(1:28)
    ddd[60:90,"month"]<-3
    ddd[60:90,"day"]<-c(1:31)
    ddd[91:120,"month"]<-4
    ddd[91:120,"day"]<-c(1:30)
    ddd[121:151,"month"]<-5
    ddd[121:151,"day"]<-c(1:31)
    ddd[152:181,"month"]<-6
    ddd[152:181,"day"]<-c(1:30)
    ddd[182:212,"month"]<-7
    ddd[182:212,"day"]<-c(1:31)
    ddd[213:243,"month"]<-8
    ddd[213:243,"day"]<-c(1:31)
    ddd[244:273,"month"]<-9
    ddd[244:273,"day"]<-c(1:30)
    ddd[274:304,"month"]<-10
    ddd[274:304,"day"]<-c(1:31)
    ddd[305:334,"month"]<-11
    ddd[305:334,"day"]<-c(1:30)
    ddd[335:365,"month"]<-12
    ddd[335:365,"day"]<-c(1:31)
    
    dddl[1:31,"month"]<-1
    dddl[1:31,"day"]<-c(1:31)
    dddl[32:60,"month"]<-2
    dddl[32:60,"day"]<-c(1:29)
    dddl[61:91,"month"]<-3
    dddl[61:91,"day"]<-c(1:31)
    dddl[92:121,"month"]<-4
    dddl[92:121,"day"]<-c(1:30)
    dddl[122:152,"month"]<-5
    dddl[122:152,"day"]<-c(1:31)
    dddl[153:182,"month"]<-6
    dddl[153:182,"day"]<-c(1:30)
    dddl[183:213,"month"]<-7
    dddl[183:213,"day"]<-c(1:31)
    dddl[214:244,"month"]<-8
    dddl[214:244,"day"]<-c(1:31)
    dddl[245:274,"month"]<-9
    dddl[245:274,"day"]<-c(1:30)
    dddl[275:305,"month"]<-10
    dddl[275:305,"day"]<-c(1:31)
    dddl[306:335,"month"]<-11
    dddl[306:335,"day"]<-c(1:30)
    dddl[336:366,"month"]<-12
    dddl[336:366,"day"]<-c(1:31)
    
    years<-dd[1,1]
    yeare<-dd[dim(dd)[1],1]
    
    if (leapyear(years)) 
      dddd<-dddl 
    else 
      dddd<-ddd
    
    dddd[,"year"]<-years
    
    for (year in years:yeare)
    {                  
      if (leapyear(year)) 
        dddd1 <- dddl 
      else 
        dddd1 <- ddd
      
      dddd1[,"year"]<-year
      
      if (year!=years) 
        dddd<-rbind(dddd,dddd1) 
    }
    
    dddd<-as.data.frame(dddd)
    dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
    dddd2<-dddd2[,-(4)]
    dimnames(dddd2)[[2]]<-c("year","month","day","prcp")
    tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
    dd<-dddd2[order(tmporder),]
    
    startyear <- as.numeric(years)
    endyear <- as.numeric(yeare)
    
    startpoint <- startyear-1
    
    endpoint <- endyear+1
    
    dd$prcp[is.na(dd$prcp)] = median(dd$prcp, na.rm=T)
    
    prcptmp_spr<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & dd$month >= 3
                    & dd$month <= 5
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_spr<-prcptmp_spr[is.na(prcptmp_spr)==F]
    
    prcptmp_sum<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & dd$month >= 6
                    & dd$month <= 8
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_sum<-prcptmp_sum[is.na(prcptmp_sum)==F]
    
    prcptmp_aut<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & dd$month >= 9
                    & dd$month <= 11
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_aut<-prcptmp_aut[is.na(prcptmp_aut)==F]
    
    
    prcptmp_win<-dd[dd$year >= startyear 
                    & dd$year <= endyear 
                    & (dd$month == 12 | dd$month == 1 | dd$month == 2)
                    & dd$prcp>=1,"prcp"]
    
    prcptmp_win<-prcptmp_win[is.na(prcptmp_win)==F]
    
    
    len_spr<-length(prcptmp_spr)
    len_sum<-length(prcptmp_sum)
    len_aut<-length(prcptmp_aut)
    len_win<-length(prcptmp_win)
    
    prcp95spr<-percentile(len_spr,prcptmp_spr,0.95)
    prcp99spr<-percentile(len_spr,prcptmp_spr,0.99)
    prcp95sum<-percentile(len_sum,prcptmp_sum,0.95)
    prcp99sum<-percentile(len_sum,prcptmp_sum,0.99)
    prcp95aut<-percentile(len_aut,prcptmp_aut,0.95)
    prcp99aut<-percentile(len_aut,prcptmp_aut,0.99)
    prcp95win<-percentile(len_win,prcptmp_win,0.95)
    prcp99win<-percentile(len_win,prcptmp_win,0.99)
    
    prcp05spr<-percentile(len_spr,prcptmp_spr,0.05)
    prcp01spr<-percentile(len_spr,prcptmp_spr,0.01)
    prcp05sum<-percentile(len_sum,prcptmp_sum,0.05)
    prcp01sum<-percentile(len_sum,prcptmp_sum,0.01)
    prcp05aut<-percentile(len_aut,prcptmp_aut,0.05)
    prcp01aut<-percentile(len_aut,prcptmp_aut,0.01)
    prcp05win<-percentile(len_win,prcptmp_win,0.05)
    prcp01win<-percentile(len_win,prcptmp_win,0.01)
    
    ys<-yeare-years+1
    
    dp<-matrix(0,ys,21)
    dimnames(dp)<-list(NULL,c("year","r95p_spr","r95p_sum","r95p_aut","r95p_win",
                              "r99p_spr","r99p_sum","r99p_aut","r99p_win",
                              "r05p_spr","r05p_sum","r05p_aut","r05p_win",
                              "r01p_spr","r01p_sum","r01p_aut","r01p_win",
                              "prcptot_spr","prcptot_sum","prcptot_aut","prcptot_win"))
    dp[,"year"]<-years:yeare
    for(i in years:yeare)
    {
      dp[(i-years+1),"r95p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                           dd$prcp > prcp95spr,"prcp"],na.rm=T)
      dp[(i-years+1),"r95p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                           dd$prcp > prcp95sum,"prcp"],na.rm=T)
      dp[(i-years+1),"r95p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                           dd$prcp > prcp95aut,"prcp"],na.rm=T)
      dp[(i-years+1),"r95p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                           dd$prcp > prcp95win,"prcp"],na.rm=T)
      
      dp[(i-years+1),"r99p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month <= 5 &
                                           dd$prcp > prcp99spr,"prcp"],na.rm=T)
      dp[(i-years+1),"r99p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month <= 8 &
                                           dd$prcp > prcp99sum,"prcp"],na.rm=T)
      dp[(i-years+1),"r99p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month <= 11 &
                                           dd$prcp > prcp99aut,"prcp"],na.rm=T)
      dp[(i-years+1),"r99p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) &
                                           dd$prcp > prcp99win,"prcp"],na.rm=T)
      
      dp[(i-years+1),"r05p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                           dd$prcp <= prcp05spr,"prcp"],na.rm=T)
      dp[(i-years+1),"r05p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                           dd$prcp <= prcp05sum,"prcp"],na.rm=T)
      dp[(i-years+1),"r05p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                           dd$prcp <= prcp05aut,"prcp"],na.rm=T)
      dp[(i-years+1),"r05p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                           dd$prcp <= prcp05win,"prcp"],na.rm=T)
      
      dp[(i-years+1),"r01p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                           dd$prcp <= prcp01spr,"prcp"],na.rm=T)
      dp[(i-years+1),"r01p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                           dd$prcp <= prcp01sum,"prcp"],na.rm=T)
      dp[(i-years+1),"r01p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                           dd$prcp <= prcp01aut,"prcp"],na.rm=T)
      dp[(i-years+1),"r01p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                           dd$prcp <= prcp01win,"prcp"],na.rm=T)
      
      dp[(i-years+1),"prcptot_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                              dd$prcp >= 1,"prcp"],na.rm=T)
      dp[(i-years+1),"prcptot_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                              dd$prcp >= 1,"prcp"],na.rm=T)
      dp[(i-years+1),"prcptot_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                              dd$prcp >= 1,"prcp"],na.rm=T)
      dp[(i-years+1),"prcptot_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month== 1 | dd$month==2) & 
                                              dd$prcp >= 1,"prcp"],na.rm=T)
    }
    
    dp<-as.data.frame(dp)
    
    R20_spr<-rep(0,ys)
    R20_sum<-rep(0,ys)
    R20_aut<-rep(0,ys)
    R20_win<-rep(0,ys)
    
    target<-as.data.frame(cbind(dp,R20_spr,R20_sum,R20_aut,R20_win))
    
    for (year in years:yeare)
    {
      mid_spr<-dd[dd$year==year & dd$month >= 3 & dd$month <= 5,"prcp"]
      mid_spr<-mid_spr[is.na(mid_spr)==F]
      target[target$year==year,"R20_spr"]<-length(mid_spr[mid_spr>=20])
      
      mid_sum<-dd[dd$year==year & dd$month >= 6 & dd$month <= 8,"prcp"]
      mid_sum<-mid_sum[is.na(mid_sum)==F]
      target[target$year==year,"R20_sum"]<-length(mid_sum[mid_sum>=20])
      
      mid_aut<-dd[dd$year==year & dd$month >= 9 & dd$month <= 11,"prcp"]
      mid_aut<-mid_aut[is.na(mid_aut)==F]
      target[target$year==year,"R20_aut"]<-length(mid_aut[mid_aut>=20])
      
      mid_win<-dd[dd$year==year & (dd$month == 12 | dd$month == 1 | dd$month == 2),"prcp"]
      mid_win<-mid_win[is.na(mid_win)==F]
      target[target$year==year,"R20_win"]<-length(mid_win[mid_win>=20])
    }
    
    R10_spr<-rep(0,ys)
    R10_sum<-rep(0,ys)
    R10_aut<-rep(0,ys)
    R10_win<-rep(0,ys)
    
    target2<-as.data.frame(cbind(target,R10_spr,R10_sum,R10_aut,R10_win))
    
    for (year in years:yeare)
    {
      mid_spr<-dd[dd$year==year & dd$month >= 3 & dd$month <= 5,"prcp"]
      mid_spr<-mid_spr[is.na(mid_spr)==F]
      target2[target2$year==year,"R10_spr"]<-length(mid_spr[mid_spr>=10])
      
      mid_sum<-dd[dd$year==year & dd$month >= 6 & dd$month <= 8,"prcp"]
      mid_sum<-mid_sum[is.na(mid_sum)==F]
      target2[target2$year==year,"R10_sum"]<-length(mid_sum[mid_sum>=10])
      
      mid_aut<-dd[dd$year==year & dd$month >= 9 & dd$month <= 11,"prcp"]
      mid_aut<-mid_aut[is.na(mid_aut)==F]
      target2[target2$year==year,"R10_aut"]<-length(mid_aut[mid_aut>=10])
      
      mid_win<-dd[dd$year==year & (dd$month == 12 | dd$month == 1 | dd$month == 2),"prcp"]
      mid_win<-mid_win[is.na(mid_win)==F]
      target2[target2$year==year,"R10_win"]<-length(mid_win[mid_win>=10])
    }
    write.table(target2,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
  }
}

##############################################################################################################
##Filter data for Year range > 19 for long-term trend analysis - modified
YrRange10<-function(sourceDir = DAILY.DATA.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName1 <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    dd <- read.table(inName,header=T,sep=" ")
    
    yrrange <- max(as.numeric(dd$Year)) - min(as.numeric(dd$Year))
    
      if (yrrange < 10)
      {  
        print(files[thisFile])
      }

  }
}

##############################################################################################################
YrRange60<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName1 <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    unselected <- "E:/IBSS/Output/unselected60"
    dir.create(unselected, showWarnings = FALSE)
    
    outName2 <- file.path(unselected, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    dd <- read.table(inName,header=F,sep=" ")
    
    yrrange <- max(as.numeric(dd$V1)) - min(as.numeric(dd$V1))
    
    if (yrrange > 60)
    {  
      write.table(dd,outName1,append=F,quote=F,sep=" ",na="-99.9",row.names=F,col.names=F)
    }
    else
    {  
      write.table(dd,outName2,append=F,quote=F,sep=" ",na="-99.9",row.names=F,col.names=F)
    }
    
  }
}


##############################################################################################################
##############################################################################################################
##############################################################################################################


##############################################################################################################