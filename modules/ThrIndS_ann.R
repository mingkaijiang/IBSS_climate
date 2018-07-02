##############################################################################################################
ThrIndS_ann<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) 
{
    dir.create(destDir, showWarnings = FALSE)
    DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.csv(inName)
        colnames(dd)<-c("id","year","month","day","prcp")
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
            dddd<-ddd
        }

        dddd[,"year"]<-years
        
        for (year in years:yeare) {                  
            if (leapyear(year)) {
                dddd1 <- dddl 
            } else {
                dddd1 <- ddd
            }
            
            dddd1[,"year"]<-year
            
            if (year!=years){
                dddd<-rbind(dddd,dddd1) 
            } 
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
        
        prcptmp_ann<-dd[dd$year >= startyear 
                        & dd$year <= endyear 
                        & dd$prcp>=1,"prcp"]
        
        prcptmp_ann<-prcptmp_ann[is.na(prcptmp_ann)==F]
        
        
        len_spr<-length(prcptmp_spr)
        len_sum<-length(prcptmp_sum)
        len_aut<-length(prcptmp_aut)
        len_win<-length(prcptmp_win)
        len_ann<-length(prcptmp_ann)
        
        prcp95spr<-quantile(prcptmp_spr,0.95)
        prcp99spr<-quantile(prcptmp_spr,0.99)
        prcp95sum<-quantile(prcptmp_sum,0.95)
        prcp99sum<-quantile(prcptmp_sum,0.99)
        prcp95aut<-quantile(prcptmp_aut,0.95)
        prcp99aut<-quantile(prcptmp_aut,0.99)
        prcp95win<-quantile(prcptmp_win,0.95)
        prcp99win<-quantile(prcptmp_win,0.99)
        prcp95ann<-quantile(prcptmp_ann,0.95)
        prcp99ann<-quantile(prcptmp_ann,0.99)
        
        prcp05spr<-quantile(prcptmp_spr,0.05)
        prcp01spr<-quantile(prcptmp_spr,0.01)
        prcp05sum<-quantile(prcptmp_sum,0.05)
        prcp01sum<-quantile(prcptmp_sum,0.01)
        prcp05aut<-quantile(prcptmp_aut,0.05)
        prcp01aut<-quantile(prcptmp_aut,0.01)
        prcp05win<-quantile(prcptmp_win,0.05)
        prcp01win<-quantile(prcptmp_win,0.01)
        prcp05ann<-quantile(prcptmp_win,0.05)
        prcp01ann<-quantile(prcptmp_win,0.01)
        
        ys<-yeare-years+1
        
        dp<-matrix(0,ys,26)
        dimnames(dp)<-list(NULL,c("year","r95p_spr","r95p_sum","r95p_aut","r95p_win","r95p_ann",
                                  "r99p_spr","r99p_sum","r99p_aut","r99p_win","r99p_ann",
                                  "r05p_spr","r05p_sum","r05p_aut","r05p_win","r05p_ann",
                                  "r01p_spr","r01p_sum","r01p_aut","r01p_win","r01p_ann",
                                  "prcptot_spr","prcptot_sum","prcptot_aut","prcptot_win","prcptot_ann"))
        dp[,"year"]<-years:yeare
        for(i in years:yeare) {
            dp[(i-years+1),"r95p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                                   dd$prcp > prcp95spr,"prcp"])
            dp[(i-years+1),"r95p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                                   dd$prcp > prcp95sum,"prcp"])
            dp[(i-years+1),"r95p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                                   dd$prcp > prcp95aut,"prcp"])
            dp[(i-years+1),"r95p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                                   dd$prcp > prcp95win,"prcp"])
            dp[(i-years+1),"r95p_ann"]<-sum(dd[dd$year == i & dd$prcp > prcp95ann,"prcp"])
                                                 
            
            dp[(i-years+1),"r99p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month <= 5 &
                                                   dd$prcp > prcp99spr,"prcp"])
            dp[(i-years+1),"r99p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month <= 8 &
                                                   dd$prcp > prcp99sum,"prcp"])
            dp[(i-years+1),"r99p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month <= 11 &
                                                   dd$prcp > prcp99aut,"prcp"])
            dp[(i-years+1),"r99p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) &
                                                   dd$prcp > prcp99win,"prcp"])
            dp[(i-years+1),"r99p_ann"]<-sum(dd[dd$year == i & dd$prcp > prcp99ann,"prcp"])
                                                
            
            
            
            dp[(i-years+1),"r05p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                                   dd$prcp <= prcp05spr,"prcp"])
            dp[(i-years+1),"r05p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                                   dd$prcp <= prcp05sum,"prcp"])
            dp[(i-years+1),"r05p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                                   dd$prcp <= prcp05aut,"prcp"])
            dp[(i-years+1),"r05p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                                   dd$prcp <= prcp05win,"prcp"])
            dp[(i-years+1),"r05p_ann"]<-sum(dd[dd$year == i & dd$prcp <= prcp05ann,"prcp"])
                          
            
            
            
            dp[(i-years+1),"r01p_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                                   dd$prcp <= prcp01spr,"prcp"])
            dp[(i-years+1),"r01p_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                                   dd$prcp <= prcp01sum,"prcp"])
            dp[(i-years+1),"r01p_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                                   dd$prcp <= prcp01aut,"prcp"])
            dp[(i-years+1),"r01p_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month == 1 | dd$month==2) & 
                                                   dd$prcp <= prcp01win,"prcp"])
            dp[(i-years+1),"r01p_ann"]<-sum(dd[dd$year == i & dd$prcp <= prcp01ann,"prcp"])
                                                 
            
            dp[(i-years+1),"prcptot_spr"]<-sum(dd[dd$year == i & dd$month >= 3 & dd$month<= 5 & 
                                                      dd$prcp >= 1,"prcp"])
            dp[(i-years+1),"prcptot_sum"]<-sum(dd[dd$year == i & dd$month >= 6 & dd$month<= 8 & 
                                                      dd$prcp >= 1,"prcp"])
            dp[(i-years+1),"prcptot_aut"]<-sum(dd[dd$year == i & dd$month >= 9 & dd$month<= 11 & 
                                                      dd$prcp >= 1,"prcp"])
            dp[(i-years+1),"prcptot_win"]<-sum(dd[dd$year == i & (dd$month == 12 | dd$month== 1 | dd$month==2) & 
                                                      dd$prcp >= 1,"prcp"])
            dp[(i-years+1),"prcptot_ann"]<-sum(dd[dd$year == i & dd$prcp >= 1,"prcp"])
                                                    
        }
        
        dp<-as.data.frame(dp)
        
        R20_spr<-rep(0,ys)
        R20_sum<-rep(0,ys)
        R20_aut<-rep(0,ys)
        R20_win<-rep(0,ys)
        R20_ann<-rep(0,ys)
        
        target<-as.data.frame(cbind(dp,R20_spr,R20_sum,R20_aut,R20_win,R20_ann))
        
        for (year in years:yeare) {
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
            
            mid_ann<-dd[dd$year==year,"prcp"]
            mid_win<-mid_ann[is.na(mid_ann)==F]
            target[target$year==year,"R20_ann"]<-length(mid_win[mid_ann>=20])
        }
        
        R10_spr<-rep(0,ys)
        R10_sum<-rep(0,ys)
        R10_aut<-rep(0,ys)
        R10_win<-rep(0,ys)
        R10_ann<-rep(0,ys)
        
        target2<-as.data.frame(cbind(target,R10_spr,R10_sum,R10_aut,R10_win,R10_ann))
        
        for (year in years:yeare) {
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
            
            mid_ann<-dd[dd$year==year,"prcp"]
            mid_ann<-mid_ann[is.na(mid_ann)==F]
            target2[target2$year==year,"R10_ann"]<-length(mid_ann[mid_ann>=10])
        }
        write.table(target2,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
    }
}
