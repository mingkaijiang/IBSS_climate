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