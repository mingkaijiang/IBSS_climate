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
