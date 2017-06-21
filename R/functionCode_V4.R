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
library(hyfo)

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
  
  ### Create a outdf to store all data in one file
  output <- matrix(ncol=22, nrow=length(DatFiles))
  output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
  colnames(output) <- c("GHCN_ID","Start_yr","End_yr","Yr_count",
                        "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
  
  output$GHCN_ID <- sub(".csv", "", DatFiles)
  
  for (j in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[j], fsep = .Platform$file.sep)

    X <- read.csv(inName)
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    # site specific data range
    v.range <- c(X$prcptot_spr, X$prcptot_sum, 
                 X$prcptot_aut, X$prcptot_win)
    
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
    
    spr_cut = cut(X[, "prcptot_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[, "prcptot_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[, "prcptot_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[, "prcptot_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[, "prcptot_spr"]))
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
    
    output[j,"Start_yr"] <- years
    output[j,"End_yr"] <- yeare
    output[j,"Yr_count"] <- col_sum 
    
    
    output[j,"HofX"] <- HofX
    output[j,"HofY"] <- HofY
    output[j,"HofXY"] <- HofXY
    output[j,"HXofY"] <- HXofY
    output[j,"s"] <- s
    output[j,"t"] <- t
    output[j,"P"] <- P
    output[j,"C"] <- C
    output[j,"M"] <- M
    output[j,"CbyP"] <- CoverP
    output[j,"MbyP"] <- MoverP
    output[j,"Mutual"] <- IofXY
    output[j,"GC"] <- GC
    output[j,"C_freedom"] <- C_free
    output[j,"GM"] <- GM
    output[j,"M_freedom"] <- M_free
    output[j,"GP"] <- GP
    output[j,"P_freedom"] <- P_free
  }
  
  write.csv(output,paste0(destDir, "/PRCPTOT_PCM.csv"),row.names=F)
  
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
    
    for (j in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[j], fsep = .Platform$file.sep)
        
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
        
        output[j,"Start_yr"] <- years
        output[j,"End_yr"] <- yeare
        output[j,"Yr_count"] <- col_sum 
        

        output[j,"HofX"] <- HofX
        output[j,"HofY"] <- HofY
        output[j,"HofXY"] <- HofXY
        output[j,"HXofY"] <- HXofY
        output[j,"s"] <- s
        output[j,"t"] <- t
        output[j,"P"] <- P
        output[j,"C"] <- C
        output[j,"M"] <- M
        output[j,"CbyP"] <- CoverP
        output[j,"MbyP"] <- MoverP
        output[j,"Mutual"] <- IofXY
        output[j,"GC"] <- GC
        output[j,"C_freedom"] <- C_free
        output[j,"GM"] <- GM
        output[j,"M_freedom"] <- M_free
        output[j,"GP"] <- GP
        output[j,"P_freedom"] <- P_free
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
  
  ### Create a outdf to store all data in one file
  output <- matrix(ncol=22, nrow=length(DatFiles))
  output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
  colnames(output) <- c("GHCN_ID","Start_yr","End_yr","Yr_count",
                        "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
  
  output$GHCN_ID <- sub(".csv", "", DatFiles)
  
  for (j in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[j], fsep = .Platform$file.sep)

    X <- read.csv(inName)
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    # site specific data range
    v.range <- c(X$R20_spr, X$R20_sum, 
                 X$R20_aut, X$R20_win)
    
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
    
    spr_cut = cut(X[, "R20_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[, "R20_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[, "R20_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[, "R20_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    spr_freq = table(spr_cut)
    sum_freq = table(sum_cut)
    aut_freq = table(aut_cut)
    win_freq = table(win_cut)
    
    bin[,"spr"] <- spr_freq
    bin[,"sum"] <- sum_freq
    bin[,"aut"] <- aut_freq
    bin[,"win"] <- win_freq
    
    bin[,"whole"] = (bin[,"spr"]+bin[,"sum"]+bin[,"aut"]+bin[,"win"])
    
    col_sum <- sum(table(X[, "R20_spr"]))
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
    
    output[j,"Start_yr"] <- years
    output[j,"End_yr"] <- yeare
    output[j,"Yr_count"] <- col_sum 
    
    
    output[j,"HofX"] <- HofX
    output[j,"HofY"] <- HofY
    output[j,"HofXY"] <- HofXY
    output[j,"HXofY"] <- HXofY
    output[j,"s"] <- s
    output[j,"t"] <- t
    output[j,"P"] <- P
    output[j,"C"] <- C
    output[j,"M"] <- M
    output[j,"CbyP"] <- CoverP
    output[j,"MbyP"] <- MoverP
    output[j,"Mutual"] <- IofXY
    output[j,"GC"] <- GC
    output[j,"C_freedom"] <- C_free
    output[j,"GM"] <- GM
    output[j,"M_freedom"] <- M_free
    output[j,"GP"] <- GP
    output[j,"P_freedom"] <- P_free
  }
  write.csv(output,paste0(destDir, "/R20S_PCM.csv"),row.names=F)
  
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
  
  ### Create a outdf to store all data in one file
  output <- matrix(ncol=22, nrow=length(DatFiles))
  output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
  colnames(output) <- c("GHCN_ID","Start_yr","End_yr","Yr_count",
                        "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
  
  output$GHCN_ID <- sub(".csv", "", DatFiles)
  
  for (j in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[j], fsep = .Platform$file.sep)

    X <- read.csv(inName)
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    # site specific data range
    v.range <- c(X$r95p_spr, X$r95p_sum, 
                 X$r95p_aut, X$r95p_win)
    
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
    
    spr_cut = cut(X[, "r95p_spr"], breaks, include.lowest=TRUE,right=TRUE)
    sum_cut = cut(X[, "r95p_sum"], breaks, include.lowest=TRUE,right=TRUE)
    aut_cut = cut(X[, "r95p_aut"], breaks, include.lowest=TRUE,right=TRUE)
    win_cut = cut(X[, "r95p_win"], breaks, include.lowest=TRUE,right=TRUE)
    
    
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
    
    output[j,"Start_yr"] <- years
    output[j,"End_yr"] <- yeare
    output[j,"Yr_count"] <- col_sum 
    
    
    output[j,"HofX"] <- HofX
    output[j,"HofY"] <- HofY
    output[j,"HofXY"] <- HofXY
    output[j,"HXofY"] <- HXofY
    output[j,"s"] <- s
    output[j,"t"] <- t
    output[j,"P"] <- P
    output[j,"C"] <- C
    output[j,"M"] <- M
    output[j,"CbyP"] <- CoverP
    output[j,"MbyP"] <- MoverP
    output[j,"Mutual"] <- IofXY
    output[j,"GC"] <- GC
    output[j,"C_freedom"] <- C_free
    output[j,"GM"] <- GM
    output[j,"M_freedom"] <- M_free
    output[j,"GP"] <- GP
    output[j,"P_freedom"] <- P_free
  }
  
  write.csv(output,paste0(destDir, "/R95P_PCM.csv"),row.names=F)
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
  
  ### Create a outdf to store all data in one file
  output <- matrix(ncol=22, nrow=length(DatFiles))
  output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
  colnames(output) <- c("GHCN_ID","Start_yr","End_yr","Yr_count",
                        "HofX","HofY","HofXY","HXofY","s","t","P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
  
  output$GHCN_ID <- sub(".csv", "", DatFiles)
  
  for (j in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[j], fsep = .Platform$file.sep)

    X <- read.csv(inName)
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years
    
    # site specific data range
    v.range <- c(X$r99p_spr, X$r99p_sum, 
                 X$r99p_aut, X$r99p_win)
    
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
    print(DatFiles[thisFile])

    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)

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
    X$Year <- as.numeric(as.character(X$Year))
    X$Month <- as.numeric(as.character(X$Month))
    X$Day <- as.numeric(as.character(X$Day))
    X$value <- as.numeric(as.character(X$value))
    X[is.na(X)] <- -99.9
    
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
## Filter data for Year range > 10 for long-term trend analysis 
## Also check for missing data issue, missing data should not be > 80%
YrRange10<-function(sourceDir = DAILY.DATA.DIRECTORY)
{
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
  
  for (thisFile in 1:length(DatFiles)) 
  {
    inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
    
    dd <- read.table(inName, sep=" ", header=T)

    yrrange <- max(as.numeric(dd$Year)) - min(as.numeric(dd$Year))
    
    
      if (yrrange < 2)
      {  
        print(DatFiles[thisFile])
        file.remove(paste0(sourceDir, "/", DatFiles[thisFile]))
      }

  }
}

##############################################################################################################
## Filter data for Year range > 10 for long-term trend analysis 
## Also check for missing data issue, missing data should not be > 80%
Missing_check<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.csv(inName,header=F,skip = 1, sep=",")
        colnames(dd) <- c("id", "Year", "Month", "Day", "value")
        
        yrrange <- max(as.numeric(dd$Year)) - min(as.numeric(dd$Year))
        
        # check for missing data issue
        target <- yrrange * 365
        d2 <- dd[complete.cases(dd),]
        reality <- nrow(d2)
        miss_percent <- (target - reality) / target
        
        if (yrrange < 10 || miss_percent > 0.2)
        {  
            print(DatFiles[thisFile])
            file.remove(paste0(sourceDir, "/", DatFiles[thisFile]))
        }
        
    }
    
    file.copy(from=sourceDir, to=destDir, 
              overwrite = recursive, recursive = FALSE, 
              copy.mode = TRUE)
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
### Gap filling using the hyfo package as an easy solution
Gap_Fill <- function(stationDF = STATION.DATAFRAME, threshold, 
                     sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)

    targList <- paste0(stationDF$ghcn1,".csv")
    supList2 <- paste0(stationDF$ghcn2,".csv")
    supList3 <- paste0(stationDF$ghcn3,".csv")
    supList4 <- paste0(stationDF$ghcn4,".csv")
    supList5 <- paste0(stationDF$ghcn5,".csv")
    supList6 <- paste0(stationDF$ghcn6,".csv")
    supList7 <- paste0(stationDF$ghcn7,".csv")
    supList8 <- paste0(stationDF$ghcn8,".csv")
    supList9 <- paste0(stationDF$ghcn9,".csv")
    
    for (i in 1:length(targList)) 
    {
        ## read in 1st file
        inName <- file.path(sourceDir, targList[i], fsep = .Platform$file.sep)
        outName <- file.path(destDir, targList[i], fsep = .Platform$file.sep)
        
        if(file.exists(inName)) {
            X <- read.csv(inName)
            X$date <- as.Date(paste(X$Year, X$Month, X$Day, sep="-"),
                              format = "%Y-%m-%d")
            
            modDF <- data.frame(X$date, X$value)
            colnames(modDF) <- c("date", "value")
            
            modDF[modDF$value == -99.9, "value"] <- NA
        } else {
            modDF <- NA
        }
        
        
        ## read 2nd file
        inName2 <- file.path(sourceDir, supList2[i], fsep = .Platform$file.sep)
        outName2 <- file.path(destDir, supList2[i], fsep = .Platform$file.sep)
        
        if(file.exists(inName2)) {
            X2 <- read.csv(inName2)
            X2$date <- as.Date(paste(X2$Year, X2$Month, X2$Day, sep="-"),
                               format = "%Y-%m-%d")
            
            modDF2 <- data.frame(X2$date, X2$value)
            colnames(modDF2) <- c("date", "value")
            
            modDF2[modDF2$value == -99.9, "value"] <- NA
        } else { 
            modDF2 <- modDF
        }
        
        if (threshold >= 2) {
            
            ## read 3rd file
            inName3 <- file.path(sourceDir, supList3[i], fsep = .Platform$file.sep)
            outName3 <- file.path(destDir, supList3[i], fsep = .Platform$file.sep)
            
            if(file.exists(inName3)) {
                X3 <- read.csv(inName3)
                X3$date <- as.Date(paste(X3$Year, X3$Month, X3$Day, sep="-"),
                                   format = "%Y-%m-%d")
                
                modDF3 <- data.frame(X3$date, X3$value)
                colnames(modDF3) <- c("date", "value")
                
                modDF3[modDF3$value == -99.9, "value"] <- NA
            } else { 
                modDF3 <- modDF 
            }
            
            if (threshold >= 3) {
                ## read 4th file
                inName4 <- file.path(sourceDir, supList4[i], fsep = .Platform$file.sep)
                outName4 <- file.path(destDir, supList4[i], fsep = .Platform$file.sep)
                
                if(file.exists(inName4)) {
                    X4 <- read.csv(inName4)
                    X4$date <- as.Date(paste(X4$Year, X4$Month, X4$Day, sep="-"),
                                       format = "%Y-%m-%d")
                    
                    modDF4 <- data.frame(X4$date, X4$value)
                    colnames(modDF4) <- c("date", "value")
                    
                    modDF4[modDF4$value == -99.9, "value"] <- NA
                } else { modDF4 <- modDF }
                
                if (threshold >= 4) {
                    ## read in 5th file
                    inName5 <- file.path(sourceDir, supList5[i], fsep = .Platform$file.sep)
                    outName5 <- file.path(destDir, supList5[i], fsep = .Platform$file.sep)
                    
                    if(file.exists(inName5)) {
                        X5 <- read.csv(inName5)
                        X5$date <- as.Date(paste(X5$Year, X5$Month, X5$Day, sep="-"),
                                           format = "%Y-%m-%d")
                        
                        modDF5 <- data.frame(X5$date, X5$value)
                        colnames(modDF5) <- c("date", "value")
                        
                        modDF5[modDF5$value == -99.9, "value"] <- NA
                    } else { modDF5 <- modDF }
                    
                    if (threshold >= 5) {
                        ## read in 6th file
                        inName6 <- file.path(sourceDir, supList6[i], fsep = .Platform$file.sep)
                        outName6 <- file.path(destDir, supList6[i], fsep = .Platform$file.sep)
                        
                        if(file.exists(inName6)) {
                            X6 <- read.csv(inName6)
                            X6$date <- as.Date(paste(X6$Year, X6$Month, X6$Day, sep="-"),
                                               format = "%Y-%m-%d")
                            
                            modDF6 <- data.frame(X6$date, X6$value)
                            colnames(modDF6) <- c("date", "value")
                            
                            modDF6[modDF6$value == -99.9, "value"] <- NA
                        } else { modDF6 <- modDF }
                        
                        if (threshold >= 6) {
                            ## read in 7th file
                            inName7 <- file.path(sourceDir, supList7[i], fsep = .Platform$file.sep)
                            outName7 <- file.path(destDir, supList7[i], fsep = .Platform$file.sep)
                            
                            if(file.exists(inName7)) {
                                X7 <- read.csv(inName7)
                                X7$date <- as.Date(paste(X7$Year, X7$Month, X7$Day, sep="-"),
                                                   format = "%Y-%m-%d")
                                
                                modDF7 <- data.frame(X7$date, X7$value)
                                colnames(modDF7) <- c("date", "value")
                                
                                modDF7[modDF7$value == -99.9, "value"] <- NA
                            } else { modDF7 <- modDF }
                            
                            if (threshold >= 7) {
                                ## read in 8th file
                                inName8 <- file.path(sourceDir, supList8[i], fsep = .Platform$file.sep)
                                outName8 <- file.path(destDir, supList8[i], fsep = .Platform$file.sep)
                                
                                if(file.exists(inName8)) {
                                    X8 <- read.csv(inName8)
                                    X8$date <- as.Date(paste(X8$Year, X8$Month, X8$Day, sep="-"),
                                                       format = "%Y-%m-%d")
                                    
                                    modDF8 <- data.frame(X8$date, X8$value)
                                    colnames(modDF8) <- c("date", "value")
                                    
                                    modDF8[modDF8$value == -99.9, "value"] <- NA
                                } else { 
                                    modDF8 <- modDF 
                                }
                                
                                if (threshold >= 8) {
                                    ## read in 9th file
                                    inName9 <- file.path(sourceDir, supList9[i], fsep = .Platform$file.sep)
                                    outName9 <- file.path(destDir, supList9[i], fsep = .Platform$file.sep)
                                    
                                    if(file.exists(inName9)) {
                                        X9 <- read.csv(inName9)
                                        X9$date <- as.Date(paste(X9$Year, X9$Month, X9$Day, sep="-"),
                                                           format = "%Y-%m-%d")
                                        
                                        modDF9 <- data.frame(X9$date, X9$value)
                                        colnames(modDF9) <- c("date", "value")
                                        
                                        modDF9[modDF9$value == -99.9, "value"] <- NA
                                    } else { 
                                        modDF9 <- modDF 
                                    }
                                    
                                } # 8
                            }     # 7
                        }         # 6
                    }             # 5
                }                 # 4
            }                     # 3
        }                         # 2
        
 
        # Find minimum start date
        startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                         modDF4$date, modDF5$date, modDF6$date, 
                         modDF7$date, modDF8$date, modDF9$date)
        
        # find maximum end date
        endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                       modDF4$date, modDF5$date, modDF6$date, 
                       modDF7$date, modDF8$date, modDF9$date)
        
        # create new datamframe 
        t.series <- seq.Date(from = startDate, to = endDate,
                             by = "day")
        testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                              "s6", "s7", "s8", "s9")
        
        # assign station values onto the dataframe
        for (j in modDF$date) {
            testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
        }
        
        for (j in modDF2$date) {
            testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
        }
        
        for (j in modDF3$date) {
            testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
        }
        
        for (j in modDF4$date) {
            testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
        }
        
        for (j in modDF5$date) {
            testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
        }
        
        for (j in modDF6$date) {
            testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
        }
        
        for (j in modDF7$date) {
            testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
        }
        
        for (j in modDF8$date) {
            testDF[testDF$date == j, "s8"] <- modDF8[modDF8$date == j, "value"]
        }
        
        for (j in modDF9$date) {
            testDF[testDF$date == j, "s9"] <- modDF9[modDF9$date == j, "value"]
        }
        
        
        # delete row entries where the entire row are full with NAs
        nr1 <- nrow(testDF[complete.cases(testDF),])
        nr2 <- nrow(testDF)
        
        decision.time <- n1/n2
            
        if (threshold >=8) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date, modDF6$date, 
                             modDF7$date, modDF8$date, modDF9$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date, modDF6$date, 
                           modDF7$date, modDF8$date, modDF9$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                  "s6", "s7", "s8", "s9")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            for (j in modDF6$date) {
                testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
            }
            
            for (j in modDF7$date) {
                testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
            }
            
            for (j in modDF8$date) {
                testDF[testDF$date == j, "s8"] <- modDF8[modDF8$date == j, "value"]
            }
            
            for (j in modDF9$date) {
                testDF[testDF$date == j, "s9"] <- modDF9[modDF9$date == j, "value"]
            }
            
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:10]))<=threshold, ]  # != 9 was the original setting, this new number seems strict!!!
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:10]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            if(csum[[4]]/csum[[1]] <= 0.1) {
                test4$s4 <- test4$s1
            }
            
            if(csum[[5]]/csum[[1]] <= 0.1) {
                test4$s5 <- test4$s1
            }
            
            if(csum[[6]]/csum[[1]] <= 0.1) {
                test4$s6 <- test4$s1
            }
            
            if(csum[[7]]/csum[[1]] <= 0.1) {
                test4$s7 <- test4$s1
            }
            
            if(csum[[8]]/csum[[1]] <= 0.1) {
                test4$s8 <- test4$s1
            }
            
            if(csum[[9]]/csum[[1]] <= 0.1) {
                test4$s9 <- test4$s1
            }
            
        } else if (threshold >= 7) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date, modDF6$date, 
                             modDF7$date, modDF8$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date, modDF6$date, 
                           modDF7$date, modDF8$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                  "s6", "s7", "s8")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            for (j in modDF6$date) {
                testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
            }
            
            for (j in modDF7$date) {
                testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
            }
            
            for (j in modDF8$date) {
                testDF[testDF$date == j, "s8"] <- modDF8[modDF8$date == j, "value"]
            }
            
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:9]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:9]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            if(csum[[4]]/csum[[1]] <= 0.1) {
                test4$s4 <- test4$s1
            }
            
            if(csum[[5]]/csum[[1]] <= 0.1) {
                test4$s5 <- test4$s1
            }
            
            if(csum[[6]]/csum[[1]] <= 0.1) {
                test4$s6 <- test4$s1
            }
            
            if(csum[[7]]/csum[[1]] <= 0.1) {
                test4$s7 <- test4$s1
            }
            
            if(csum[[8]]/csum[[1]] <= 0.1) {
                test4$s8 <- test4$s1
            }

        } else if (threshold >= 6) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date, modDF6$date, 
                             modDF7$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date, modDF6$date, 
                           modDF7$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                  "s6", "s7")
            
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            for (j in modDF6$date) {
                testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
            }
            
            for (j in modDF7$date) {
                testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:8]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:8]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            if(csum[[4]]/csum[[1]] <= 0.1) {
                test4$s4 <- test4$s1
            }
            
            if(csum[[5]]/csum[[1]] <= 0.1) {
                test4$s5 <- test4$s1
            }
            
            if(csum[[6]]/csum[[1]] <= 0.1) {
                test4$s6 <- test4$s1
            }
            
            if(csum[[7]]/csum[[1]] <= 0.1) {
                test4$s7 <- test4$s1
            }
            
            
        } else if (threshold >= 5) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date, modDF6$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date, modDF6$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                                  "s6")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            for (j in modDF6$date) {
                testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:7]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:7]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            if(csum[[4]]/csum[[1]] <= 0.1) {
                test4$s4 <- test4$s1
            }
            
            if(csum[[5]]/csum[[1]] <= 0.1) {
                test4$s5 <- test4$s1
            }
            
            if(csum[[6]]/csum[[1]] <= 0.1) {
                test4$s6 <- test4$s1
            }
        

            
        } else if (threshold >= 4) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date, modDF5$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date, modDF5$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            for (j in modDF5$date) {
                testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:6]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:6]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            if(csum[[4]]/csum[[1]] <= 0.1) {
                test4$s4 <- test4$s1
            }
            
            if(csum[[5]]/csum[[1]] <= 0.1) {
                test4$s5 <- test4$s1
            }
            
            
        } else if (threshold >= 3) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                             modDF4$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                           modDF4$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3", "s4")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            for (j in modDF4$date) {
                testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:5]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:5]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            if(csum[[4]]/csum[[1]] <= 0.1) {
                test4$s4 <- test4$s1
            }
            
            
        } else if (threshold >= 2) {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date, modDF3$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date, modDF3$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2", "s3")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            for (j in modDF3$date) {
                testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:4]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:4]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
            if(csum[[3]]/csum[[1]] <= 0.1) {
                test4$s3 <- test4$s1
            }
            
            
        } else {
            # Find minimum start date
            startDate <- min(modDF$date, modDF2$date)
            
            # find maximum end date
            endDate <- max(modDF$date, modDF2$date)
            
            # create new datamframe 
            t.series <- seq.Date(from = startDate, to = endDate,
                                 by = "day")
            testDF <- data.frame(t.series, NA, NA)
            colnames(testDF) <- c("date", "s1", "s2")
            
            # assign station values onto the dataframe
            for (j in modDF$date) {
                testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
            }
            
            for (j in modDF2$date) {
                testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
            }
            
            # delete row entries where the entire row are full with NAs
            test2 <- testDF[rowSums(is.na(testDF[,2:3]))<=threshold, ]  
            
            test3 <- subset(test2, date <= max(modDF$date))
            test4 <- subset(test3, date >= min(modDF$date))
            
            # check column sums to ensure there's still data left for each ghcn station
            csum <- colSums(!is.na(test4[,2:3]))
            
            # if >90% are NAs, simply repeat column 1 values 
            if(csum[[2]]/csum[[1]] <= 0.1) {
                test4$s2 <- test4$s1
            }
            
        }

        
        # gap fill the rest missing values
        test5 <- fillGap(test4, corPeriod="daily")
        
        # re-create the dataframe over the entire period with no missing values
        t.series <- seq.Date(from = min(modDF$date), to = max(modDF$date),
                             by = "day")
        outDF <- data.frame(t.series, NA)
        colnames(outDF) <- c("date", "value")
        
        t.series <- seq.Date(from = min(modDF2$date), to = max(modDF2$date),
                             by = "day")
        outDF2 <- data.frame(t.series, NA)
        colnames(outDF2) <- c("date", "value")
        
        # assign values from output filled df
        for (j in test5$Date) {
            outDF[outDF$date == j, "value"] <- test5[test5$Date == j, "s1"]
            outDF2[outDF2$date == j, "value"] <- test5[test5$Date == j, "s2"]
        }
        
        outDF$Year <- as.numeric(format(outDF$date, "%Y"))
        outDF$Month <- as.numeric(format(outDF$date, "%m"))
        outDF$Day <- as.numeric(format(outDF$date, "%d"))
        
        outDF2$Year <- as.numeric(format(outDF2$date, "%Y"))
        outDF2$Month <- as.numeric(format(outDF2$date, "%m"))
        outDF2$Day <- as.numeric(format(outDF2$date, "%d"))
        
        outDF$value[is.na(outDF$value)] <- -99.9
        outDF2$value[is.na(outDF2$value)] <- -99.9
        
        outDF <- outDF[,c("date", "Year", "Month", "Day", "value")]
        colnames(outDF) <- c("date", "Year", "Month", "Day", "value")
        write.csv(outDF,outName, row.names=F)
        
        outDF2 <- outDF2[,c("date", "Year", "Month", "Day", "value")]
        colnames(outDF2) <- c("date", "Year", "Month", "Day", "value")
        write.csv(outDF2,outName2, row.names=F)
        
        
        if (threshold >= 2) {
            t.series <- seq.Date(from = min(modDF3$date), to = max(modDF3$date),
                                 by = "day")
            outDF3 <- data.frame(t.series, NA)
            colnames(outDF3) <- c("date", "value")
            
            # assign values from output filled df
            for (j in test5$Date) {
                outDF3[outDF3$date == j, "value"] <- test5[test5$Date == j, "s3"]
            }
            
            outDF3$Year <- as.numeric(format(outDF3$date, "%Y"))
            outDF3$Month <- as.numeric(format(outDF3$date, "%m"))
            outDF3$Day <- as.numeric(format(outDF3$date, "%d"))
            
            outDF3$value[is.na(outDF3$value)] <- -99.9
            
            outDF3 <- outDF3[,c("date", "Year", "Month", "Day", "value")]
            colnames(outDF3) <- c("date", "Year", "Month", "Day", "value")
            write.csv(outDF3,outName3, row.names=F)
            
            if (threshold >= 3) {
                
                t.series <- seq.Date(from = min(modDF4$date), to = max(modDF4$date),
                                     by = "day")
                outDF4 <- data.frame(t.series, NA)
                colnames(outDF4) <- c("date", "value")
                
                # assign values from output filled df
                for (j in test5$Date) {
                    outDF4[outDF4$date == j, "value"] <- test5[test5$Date == j, "s4"]
                }
                
                outDF4$Year <- as.numeric(format(outDF4$date, "%Y"))
                outDF4$Month <- as.numeric(format(outDF4$date, "%m"))
                outDF4$Day <- as.numeric(format(outDF4$date, "%d"))
                
                outDF4$value[is.na(outDF4$value)] <- -99.9
                
                outDF4 <- outDF4[,c("date", "Year", "Month", "Day", "value")]
                colnames(outDF4) <- c("date", "Year", "Month", "Day", "value")
                write.csv(outDF4,outName4, row.names=F)
                
                if (threshold >= 4) {
                    t.series <- seq.Date(from = min(modDF5$date), to = max(modDF5$date),
                                         by = "day")
                    outDF5 <- data.frame(t.series, NA)
                    colnames(outDF5) <- c("date", "value")
                    
                    # assign values from output filled df
                    for (j in test5$Date) {
                        outDF5[outDF5$date == j, "value"] <- test5[test5$Date == j, "s5"]
                    }
                    
                    outDF5$Year <- as.numeric(format(outDF5$date, "%Y"))
                    outDF5$Month <- as.numeric(format(outDF5$date, "%m"))
                    outDF5$Day <- as.numeric(format(outDF5$date, "%d"))
                    
                    outDF5$value[is.na(outDF5$value)] <- -99.9
                    
                    outDF5 <- outDF5[,c("date", "Year", "Month", "Day", "value")]
                    colnames(outDF5) <- c("date", "Year", "Month", "Day", "value")
                    write.csv(outDF5,outName5, row.names=F)
                    
                    if (threshold >= 5) {
                        t.series <- seq.Date(from = min(modDF6$date), to = max(modDF6$date),
                                             by = "day")
                        outDF6 <- data.frame(t.series, NA)
                        colnames(outDF6) <- c("date", "value")
                        
                        # assign values from output filled df
                        for (j in test5$Date) {
                            outDF6[outDF6$date == j, "value"] <- test5[test5$Date == j, "s6"]
                        }
                        
                        outDF6$Year <- as.numeric(format(outDF6$date, "%Y"))
                        outDF6$Month <- as.numeric(format(outDF6$date, "%m"))
                        outDF6$Day <- as.numeric(format(outDF6$date, "%d"))
                        
                        outDF6$value[is.na(outDF6$value)] <- -99.9
                        
                        outDF6 <- outDF6[,c("date", "Year", "Month", "Day", "value")]
                        colnames(outDF6) <- c("date", "Year", "Month", "Day", "value")
                        write.csv(outDF6,outName6, row.names=F)
                        
                        if (threshold >= 6) {
                            t.series <- seq.Date(from = min(modDF7$date), to = max(modDF7$date),
                                                 by = "day")
                            outDF7 <- data.frame(t.series, NA)
                            colnames(outDF7) <- c("date", "value")
                            
                            # assign values from output filled df
                            for (j in test5$Date) {
                                outDF7[outDF7$date == j, "value"] <- test5[test5$Date == j, "s7"]
                            }
                            
                            outDF7$Year <- as.numeric(format(outDF7$date, "%Y"))
                            outDF7$Month <- as.numeric(format(outDF7$date, "%m"))
                            outDF7$Day <- as.numeric(format(outDF7$date, "%d"))
                            
                            outDF7$value[is.na(outDF7$value)] <- -99.9
                            
                            outDF7 <- outDF7[,c("date", "Year", "Month", "Day", "value")]
                            colnames(outDF7) <- c("date", "Year", "Month", "Day", "value")
                            write.csv(outDF7,outName7, row.names=F)
                            
                            if (threshold >= 7) {
                                t.series <- seq.Date(from = min(modDF8$date), to = max(modDF8$date),
                                                     by = "day")
                                outDF8 <- data.frame(t.series, NA)
                                colnames(outDF8) <- c("date", "value")
                                
                                # assign values from output filled df
                                for (j in test5$Date) {
                                    outDF8[outDF8$date == j, "value"] <- test5[test5$Date == j, "s8"]
                                }
                                
                                outDF8$Year <- as.numeric(format(outDF8$date, "%Y"))
                                outDF8$Month <- as.numeric(format(outDF8$date, "%m"))
                                outDF8$Day <- as.numeric(format(outDF8$date, "%d"))
                                
                                outDF8$value[is.na(outDF8$value)] <- -99.9
                                
                                outDF8 <- outDF8[,c("date", "Year", "Month", "Day", "value")]
                                colnames(outDF8) <- c("date", "Year", "Month", "Day", "value")
                                write.csv(outDF8,outName8, row.names=F)
                                
                                if (threshold >= 8) {
                                    t.series <- seq.Date(from = min(modDF9$date), to = max(modDF9$date),
                                                         by = "day")
                                    outDF9 <- data.frame(t.series, NA)
                                    colnames(outDF9) <- c("date", "value")
                                    
                                    # assign values from output filled df
                                    for (j in test5$Date) {
                                        outDF9[outDF9$date == j, "value"] <- test5[test5$Date == j, "s9"]
                                    }
                                    
                                    outDF9$Year <- as.numeric(format(outDF9$date, "%Y"))
                                    outDF9$Month <- as.numeric(format(outDF9$date, "%m"))
                                    outDF9$Day <- as.numeric(format(outDF9$date, "%d"))
                                    
                                    outDF9$value[is.na(outDF9$value)] <- -99.9
                                    
                                    outDF9 <- outDF9[,c("date", "Year", "Month", "Day", "value")]
                                    colnames(outDF9) <- c("date", "Year", "Month", "Day", "value")
                                    write.csv(outDF9,outName9, row.names=F)
                                } # 8
                            } # 7
                        } # 6
                    } # 5
                } # 4
            } # 3
        } # 2

        print(targList[i])
        
    }
    
}

##############################################################################################################
### Gap filling using the hyfo package as an easy solution
Gap_Fill_within_station <- function(stationDF = STATION.DATAFRAME, threshold, 
                                    sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {
    
    dir.create(destDir, showWarnings = FALSE)
    
    targList <- paste0(stationDF$ghcn1,".csv")
    
    for (i in 1:length(targList)) 
    {
        inName <- file.path(sourceDir, targList[i], fsep = .Platform$file.sep)
        outName <- file.path(destDir, targList[i], fsep = .Platform$file.sep)
        
        # read in 1st file
        if(file.exists(inName)) {
            X <- read.csv(inName)
            X$date <- as.Date(paste(X$Year, X$Month, X$Day, sep="-"),
                              format = "%Y-%m-%d")
            
            modDF <- data.frame(X$date, X$value)
            colnames(modDF) <- c("date", "value")
            
            modDF[modDF$value == -99.9, "value"] <- NA
        } else {
            modDF <- NA
        }

        
        # Find minimum start date
        startDate <- min(modDF$date, modDF2$date, modDF3$date, 
                         modDF4$date, modDF5$date, modDF6$date, 
                         modDF7$date, modDF8$date, modDF9$date)
        
        # find maximum end date
        endDate <- max(modDF$date, modDF2$date, modDF3$date, 
                       modDF4$date, modDF5$date, modDF6$date, 
                       modDF7$date, modDF8$date, modDF9$date)
        
        # create new datamframe 
        t.series <- seq.Date(from = startDate, to = endDate,
                             by = "day")
        testDF <- data.frame(t.series, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        colnames(testDF) <- c("date", "s1", "s2", "s3", "s4", "s5",
                              "s6", "s7", "s8", "s9")
        
        # assign station values onto the dataframe
        for (j in modDF$date) {
            testDF[testDF$date == j, "s1"] <- modDF[modDF$date == j, "value"]
        }
        
        for (j in modDF2$date) {
            testDF[testDF$date == j, "s2"] <- modDF2[modDF2$date == j, "value"]
        }
        
        for (j in modDF3$date) {
            testDF[testDF$date == j, "s3"] <- modDF3[modDF3$date == j, "value"]
        }
        
        for (j in modDF4$date) {
            testDF[testDF$date == j, "s4"] <- modDF4[modDF4$date == j, "value"]
        }
        
        for (j in modDF5$date) {
            testDF[testDF$date == j, "s5"] <- modDF5[modDF5$date == j, "value"]
        }
        
        for (j in modDF6$date) {
            testDF[testDF$date == j, "s6"] <- modDF6[modDF6$date == j, "value"]
        }
        
        for (j in modDF7$date) {
            testDF[testDF$date == j, "s7"] <- modDF7[modDF7$date == j, "value"]
        }
        
        for (j in modDF8$date) {
            testDF[testDF$date == j, "s8"] <- modDF8[modDF8$date == j, "value"]
        }
        
        for (j in modDF9$date) {
            testDF[testDF$date == j, "s9"] <- modDF9[modDF9$date == j, "value"]
        }
        
        # delete row entries where the entire row are full with NAs
        test2 <- testDF[rowSums(is.na(testDF[,2:10]))<=threshold, ]  # != 9 was the original setting, this new number seems strict!!!
        
        
        test3 <- subset(test2, date <= max(modDF$date))
        test4 <- subset(test3, date >= min(modDF$date))
        
        # check column sums to ensure there's still data left for each ghcn station
        csum <- colSums(!is.na(test4[,2:10]))
        
        # if >90% are NAs, simply repeat column 1 values 
        if(csum[[2]]/csum[[1]] <= 0.1) {
            test4$s2 <- test4$s1
        }
        
        if(csum[[3]]/csum[[1]] <= 0.1) {
            test4$s3 <- test4$s1
        }
        
        if(csum[[4]]/csum[[1]] <= 0.1) {
            test4$s4 <- test4$s1
        }
        
        if(csum[[5]]/csum[[1]] <= 0.1) {
            test4$s5 <- test4$s1
        }
        
        if(csum[[6]]/csum[[1]] <= 0.1) {
            test4$s6 <- test4$s1
        }
        
        if(csum[[7]]/csum[[1]] <= 0.1) {
            test4$s7 <- test4$s1
        }
        
        if(csum[[8]]/csum[[1]] <= 0.1) {
            test4$s8 <- test4$s1
        }
        
        if(csum[[9]]/csum[[1]] <= 0.1) {
            test4$s9 <- test4$s1
        }
        
        # gap fill the rest missing values
        test5 <- fillGap(test4, corPeriod="daily")
        
        # re-create the dataframe over the entire period with no missing values
        t.series <- seq.Date(from = min(modDF$date), to = max(modDF$date),
                             by = "day")
        outDF <- data.frame(t.series, NA)
        colnames(outDF) <- c("date", "s1")
        
        for (j in test5$Date) {
            outDF[outDF$date == j, "s1"] <- test5[test5$Date == j, "s1"]
        }
        
        outDF$Year <- as.numeric(format(outDF$date, "%Y"))
        outDF$Month <- as.numeric(format(outDF$date, "%m"))
        outDF$Day <- as.numeric(format(outDF$date, "%d"))
        
        outDF2 <- outDF[,c("date", "Year", "Month", "Day", "s1")]
        colnames(outDF2) <- c("date", "Year", "Month", "Day", "value")
        write.csv(outDF2,outName)
        
        print(targList[i])
        
    }  # i loop
    
}
##############################################################################################################
select_9_ghcn_stations <- function(corDF, gDF) {
    
    ### libraries
    library(sp)
    library(rgeos)
    library(Imap)
    
    
    ### SCCS station list
    corDF$SCCSID <- as.character(corDF$SCCSID)
    sDF <- corDF[!duplicated(corDF$SCCSID), ] 
    
    newDF <- data.frame(sDF$SCCSID, sDF$slat, sDF$slon,
                        0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0)
    
    colnames(newDF) <- c("sccs_id", "slat", "slon",
                         "ghcn1", "lat1", "lon1", "elev1",
                         "ghcn2", "lat2", "lon2", "elev2",
                         "ghcn3", "lat3", "lon3", "elev3",
                         "ghcn4", "lat4", "lon4", "elev4",
                         "ghcn5", "lat5", "lon5", "elev5",
                         "ghcn6", "lat6", "lon6", "elev6",
                         "ghcn7", "lat7", "lon7", "elev7",
                         "ghcn8", "lat8", "lon8", "elev8",
                         "ghcn9", "lat9", "lon9", "elev9")
    

    gDF$GHCN_ID <- as.character(gDF$GHCN_ID)
    
    
    ### Find nearest 9 stations 
    for (i in 1:nrow(newDF)) {
        gDF$dist <- gdist(lat.1=gDF$Lat,
                          lon.1=gDF$Lon,
                          lat.2=newDF$slat[i],
                          lon.2=newDF$slon[i])
        
        newg <- gDF[order(gDF$dist),]
        
        newDF[i,"ghcn1"] <- newg[1,"GHCN_ID"]
        newDF[i,"lat1"] <- newg[1,"Lat"]
        newDF[i,"lon1"] <- newg[1,"Lon"]
        newDF[i,"elev1"] <- newg[1,"Elev"]
        
        newDF[i,"ghcn2"] <- newg[2,"GHCN_ID"]
        newDF[i,"lat2"] <- newg[2,"Lat"]
        newDF[i,"lon2"] <- newg[2,"Lon"]
        newDF[i,"elev2"] <- newg[2,"Elev"]
        
        newDF[i,"ghcn3"] <- newg[3,"GHCN_ID"]
        newDF[i,"lat3"] <- newg[3,"Lat"]
        newDF[i,"lon3"] <- newg[3,"Lon"]
        newDF[i,"elev3"] <- newg[3,"Elev"]
        
        newDF[i,"ghcn4"] <- newg[4,"GHCN_ID"]
        newDF[i,"lat4"] <- newg[4,"Lat"]
        newDF[i,"lon4"] <- newg[4,"Lon"]
        newDF[i,"elev4"] <- newg[4,"Elev"]
        
        newDF[i,"ghcn5"] <- newg[5,"GHCN_ID"]
        newDF[i,"lat5"] <- newg[5,"Lat"]
        newDF[i,"lon5"] <- newg[5,"Lon"]
        newDF[i,"elev5"] <- newg[5,"Elev"]
        
        newDF[i,"ghcn6"] <- newg[6,"GHCN_ID"]
        newDF[i,"lat6"] <- newg[6,"Lat"]
        newDF[i,"lon6"] <- newg[6,"Lon"]
        newDF[i,"elev6"] <- newg[6,"Elev"]
        
        newDF[i,"ghcn7"] <- newg[7,"GHCN_ID"]
        newDF[i,"lat7"] <- newg[7,"Lat"]
        newDF[i,"lon7"] <- newg[7,"Lon"]
        newDF[i,"elev7"] <- newg[7,"Elev"]
        
        newDF[i,"ghcn8"] <- newg[8,"GHCN_ID"]
        newDF[i,"lat8"] <- newg[8,"Lat"]
        newDF[i,"lon8"] <- newg[8,"Lon"]
        newDF[i,"elev8"] <- newg[8,"Elev"]
        
        newDF[i,"ghcn9"] <- newg[9,"GHCN_ID"]
        newDF[i,"lat9"] <- newg[9,"Lat"]
        newDF[i,"lon9"] <- newg[9,"Lon"]
        newDF[i,"elev9"] <- newg[9,"Elev"]
        
    }
    

    write.csv(newDF, "data/sccs_ghcn_station_list.csv", row.names=F)
    
    return(newDF)
}


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

##############################################################################################################