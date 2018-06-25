##############################################################################################################
##Calculate PDSI predictability, based on Rui's data 
## Each row represents one year (jan to dec)
## Every 114 lines is a society
## Order of site in OSCCSID.txt
PM_pred<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {   
    
    ### Create out directory
    dir.create(destDir, showWarnings = FALSE)
    
    ### read in society ID 
    sccs.id <- read.table("data/PE_PDSI/OSCCSID.txt", col.names="SCCSID", skip=1)
    
    ### number of society
    l <- length(sccs.id$SCCSID)
    
    ### Create a outdf to store all data in one file
    output <- matrix(ncol=4, nrow=l)
    output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
    colnames(output) <- c("SCCS_ID","P","C","M")
    
    output$SCCS_ID <- sccs.id$SCCSID
    
    ### Read in PDSI data
    myDF <- read.table("data/PE_PDSI/craig_drought_pm.txt")
    
    for (j in 1:l) 
    {
        l1 <- 114 * (j - 1) + 1
        l2 <- 114* j
            
        X <- myDF[l1:l2,]
        
        if (X[1,1] < -1 | X[1,1]> 1) {
            output[j,"P"] <- "NA"
            output[j,"C"] <- "NA"
            output[j,"M"] <- "NA"
        } else {
            bin <- matrix(0, ncol=14, nrow=10)
            dimnames(bin) <- list(NULL,c("bin_size","jan","feb","mar","apr",
                                         "may","jun","jul","aug",
                                         "sep","oct","nov","dec",
                                         "whole"))
            
            bin[,"bin_size"] <- seq(-0.8, 1, by=0.2)
            
            breaks = seq(-1,1, by=0.2)
            
            jan_cut = cut(X[, 1], breaks, include.lowest=TRUE,right=TRUE)
            feb_cut = cut(X[, 2], breaks, include.lowest=TRUE,right=TRUE)
            mar_cut = cut(X[, 3], breaks, include.lowest=TRUE,right=TRUE)
            apr_cut = cut(X[, 4], breaks, include.lowest=TRUE,right=TRUE)
            may_cut = cut(X[, 5], breaks, include.lowest=TRUE,right=TRUE)
            jun_cut = cut(X[, 6], breaks, include.lowest=TRUE,right=TRUE)
            jul_cut = cut(X[, 7], breaks, include.lowest=TRUE,right=TRUE)
            aug_cut = cut(X[, 8], breaks, include.lowest=TRUE,right=TRUE)
            sep_cut = cut(X[, 9], breaks, include.lowest=TRUE,right=TRUE)
            oct_cut = cut(X[, 10], breaks, include.lowest=TRUE,right=TRUE)
            nov_cut = cut(X[, 11], breaks, include.lowest=TRUE,right=TRUE)
            dec_cut = cut(X[, 12], breaks, include.lowest=TRUE,right=TRUE)
            
            bin[,"jan"] <- table(jan_cut)
            bin[,"feb"] <- table(feb_cut)
            bin[,"mar"] <- table(mar_cut)
            bin[,"apr"] <- table(apr_cut)
            bin[,"may"] <- table(may_cut)
            bin[,"jun"] <- table(jun_cut)
            bin[,"jul"] <- table(jul_cut)
            bin[,"aug"] <- table(aug_cut)
            bin[,"sep"] <- table(sep_cut)
            bin[,"oct"] <- table(oct_cut)
            bin[,"nov"] <- table(nov_cut)
            bin[,"dec"] <- table(dec_cut)
            
            bin[,"whole"] = (bin[,"jan"]+bin[,"feb"]+bin[,"mar"]+bin[,"apr"]+
                                 bin[,"may"]+bin[,"jun"]+bin[,"jul"]+bin[,"aug"]+
                                 bin[,"sep"]+bin[,"oct"]+bin[,"nov"]+bin[,"dec"])
            
            col_sum <- sum(table(X[, 1]))
            whole_sum <- col_sum*12
            
            #uncertainty with respect to time H(X)
            HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
            
            #uncertainty with respect to state H(Y)
            V1 <- bin[,"whole"]/whole_sum
            V2 <- log10(bin[,"whole"]/whole_sum)
            for (i in 1:length(V2)) {
                if(is.finite(V2[i])==F) V2[i] <- 0
                else V2[i] <- V2[i]
            }
            
            HofY <- -sum(V1*V2)
            
            #uncertainty with respect to interaction of time and state, H(XY)
            M1 <- bin[1:10,2:13]/whole_sum
            M2 <- log10(M1)
            for (i in 1:length(M2)){
                if(is.finite(M2[i])==F) M2[i] <- 0
                else M2[i] <- M2[i]
            }
            
            HofXY <- -sum(M1*M2)
            
            #Conditional uncertainty with regard to state, with time given, HXofY
            HXofY <- HofXY - HofX
            s <- 10
            t <- 12
            
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
            
            output[j,"P"] <- P
            output[j,"C"] <- C
            output[j,"M"] <- M
            
        }
    }
    
    write.csv(output,paste0(destDir, "/PM_PCM.csv"),row.names=F)
    
}
