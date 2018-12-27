##############################################################################################################
##Calculate consecutive warm days predictability based on whole temporal range
consec_warm_pred_hunter_gatherer<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
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
        is.na(X)<-sapply(X, is.infinite)
        X[is.na(X)]<- 0
        
        years <- min(X$year)
        yeare <- max(X$year)
        yearr <- yeare-years
        
        # site specific data range
        v.range <- c(X$warm_djf, X$warm_mam, 
                     X$warm_jja, X$warm_son)
        
        test <- max(v.range) - min(v.range)
        
        sep.value <- ifelse(test <= 0.01, 0.001, 
                            ifelse(test > 0.01 & test <= 0.1, 0.01,
                                   ifelse(test > 0.1 & test <= 1, 0.1, 1.0)))
        
        max_top <- round_any(max(v.range), sep.value, f = ceiling)
        min_bot <- round_any(min(v.range), sep.value, f = floor)
        diff <- max_top - min_bot
        interval <- 10
        
        if(diff > 0) {
            bin <- matrix(0, ncol=6, nrow=interval)
            dimnames(bin) <- list(NULL,c("bin_size","djf","mam","jja","son","whole"))
            
            bin[,"bin_size"] <- c(min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                                  min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                                  min_bot+0.9*diff,max_top)
            
            breaks = c(min_bot,min_bot+0.1*diff,min_bot+0.2*diff,min_bot+0.3*diff,min_bot+0.4*diff,
                       min_bot+0.5*diff,min_bot+0.6*diff,min_bot+0.7*diff,min_bot+0.8*diff,
                       min_bot+0.9*diff,max_top)
            
            djf_cut = cut(X[, "warm_djf"], breaks, include.lowest=TRUE,right=TRUE)
            mam_cut = cut(X[, "warm_mam"], breaks, include.lowest=TRUE,right=TRUE)
            jja_cut = cut(X[, "warm_jja"], breaks, include.lowest=TRUE,right=TRUE)
            son_cut = cut(X[, "warm_son"], breaks, include.lowest=TRUE,right=TRUE)
            
            djf_freq = table(djf_cut)
            mam_freq = table(mam_cut)
            jja_freq = table(jja_cut)
            son_freq = table(son_cut)
            
            bin[,"djf"] <- djf_freq
            bin[,"mam"] <- mam_freq
            bin[,"jja"] <- jja_freq
            bin[,"son"] <- son_freq
            
            bin[,"whole"] = (bin[,"djf"]+bin[,"mam"]+bin[,"jja"]+bin[,"son"])
            
            col_sum <- sum(table(X[, "warm_djf"]))
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
    }
    write.csv(output,paste0(destDir, "/Consec_wsdi_PCM_hunter_gatherer.csv"),row.names=F)
    
}
