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
