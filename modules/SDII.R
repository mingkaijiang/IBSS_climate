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

