##############################################################################################################
ThrIndS_tmax <- function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY) {

  dir.create(destDir, showWarnings = FALSE)
  DatFiles <- list.files(path = sourceDir, pattern = "\\.csv")
    
    options(max.print=1000000)
    
   # dd <- read.csv("data/ghcnd_gap_filled/CA001054920.csv")
   # str(dd)
   # colnames(dd)<-c("id","year","month","day","tmax")
   # print(dd$tmax[dd$year == 1898])
   # print(dd$tmax[dd$year == 1898 & dd$month >= 3 & dd$month<= 5])
   # print(dd$tmax[dd$year == 1898 & dd$month >= 9 & dd$month<= 11])
    
    for (thisFile in 1:length(DatFiles)) 
    {
        inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        outName <- file.path(destDir, DatFiles[thisFile], fsep = .Platform$file.sep)
        
        dd <- read.csv(inName)
        colnames(dd)<-c("id","year","month","day","tmax")
        dd <- dd[,2:5]
        
        nama<-substr(inName,start=1,stop=(nchar(inName)-4))
        outdirtmp <- strsplit(inName,"/")[[1]]
        
        ofilename<-substr(outdirtmp[length(outdirtmp)],
                          start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
        
        
        ddd<-matrix(NA,365,4)
        dddl<-matrix(NA,366,4)
        dimnames(ddd)<-list(NULL,c("year","month","day","tmax"))
        dimnames(dddl)<-list(NULL,c("year","month","day","tmax"))
        
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
            
            if (year!=years) {
                dddd<-rbind(dddd,dddd1) 
            }
        }
        
        dddd<-as.data.frame(dddd)
        dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
        dddd2<-dddd2[,-(4)]
        dimnames(dddd2)[[2]]<-c("year","month","day","tmax")
        tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
        dd<-dddd2[order(tmporder),]
        
        startyear <- as.numeric(years)
        endyear <- as.numeric(yeare)
        
        startpoint <- startyear-1
        
        endpoint <- endyear+1
        
        dd$tmax[is.na(dd$tmax)] = median(dd$tmax, na.rm=T)
        
        tmaxtmp_spr<-dd[dd$year >= startyear 
                        & dd$year <= endyear 
                        & dd$month >= 3
                        & dd$month <= 5, "tmax"]
#                        & dd$tmax>=1,"tmax"]
        
        tmaxtmp_spr<-tmaxtmp_spr[is.na(tmaxtmp_spr)==F]
        
        tmaxtmp_sum<-dd[dd$year >= startyear 
                        & dd$year <= endyear 
                        & dd$month >= 6
                        & dd$month <= 8, "tmax" ]
#                        & dd$tmax>=1,"tmax"]
        
        tmaxtmp_sum<-tmaxtmp_sum[is.na(tmaxtmp_sum)==F]
        
        tmaxtmp_aut<-dd[dd$year >= startyear 
                        & dd$year <= endyear 
                        & dd$month >= 9
                        & dd$month <= 11, "tmax" ]
#                        & dd$tmax>=1,"tmax"]
        
        tmaxtmp_aut<-tmaxtmp_aut[is.na(tmaxtmp_aut)==F]
        
        
        tmaxtmp_win<-dd[dd$year >= startyear 
                        & dd$year <= endyear 
                        & (dd$month == 12 | dd$month == 1 | dd$month == 2), "tmax" ]
#                        & dd$tmax>=1,"tmax"]
        
        tmaxtmp_win<-tmaxtmp_win[is.na(tmaxtmp_win)==F]
        
        
        len_spr<-length(tmaxtmp_spr)
        len_sum<-length(tmaxtmp_sum)
        len_aut<-length(tmaxtmp_aut)
        len_win<-length(tmaxtmp_win)
        
        
        ys<-yeare-years+1
        dp<-matrix(0,ys,5)
        dimnames(dp)<-list(NULL,c("year","tmax_spr","tmax_sum","tmax_aut","tmax_win"))
        dp[,"year"]<-years:yeare
        
        
        for(i in years:yeare)
        {
          
          dp[(i-years+1),"tmax_spr"]<-max(dd$tmax[dd$year == i & dd$month >= 3 & dd$month<= 5],na.rm=T)/10.0
         
          if(inName == "data/ghcnd_gap_filled/CA001054920.csv" && i == 1898) { print(dd$tmax[dd$year == i & dd$month >= 9 & dd$month<= 11],na.rm=T)}
          dp[(i-years+1),"tmax_sum"]<-max(dd$tmax[dd$year == i & dd$month >= 6 & dd$month<= 8],na.rm=T)/10.0
          dp[(i-years+1),"tmax_aut"]<-max(dd$tmax[dd$year == i & dd$month >= 9 & dd$month<= 11],na.rm=T)/10.0
          dp[(i-years+1),"tmax_win"]<-max(dd$tmax[dd$year == i & (dd$month == 12 | dd$month== 1 | dd$month==2)],na.rm=T)/10.0
        }
        dp<-as.data.frame(dp)
        
        
        target<-as.data.frame(dp)

        
  
        write.table(target,outName,append=F,quote=F,sep=",",na="-99.9",row.names=F)
    }
}
