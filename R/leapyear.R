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
