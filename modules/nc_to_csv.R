inFile <- paste("data/cpc/precip.day.1981-2010.ltm.nc")

### read in cpc file as an example
require(ncdf4)
# process file
ncDF <- nc_open(inFile) 

print(ncDF)

t <- ncvar_get(ncDF,varid = "time" )
nt <- dim(t)

tmp.array <- ncvar_get(ncDF)
#dim(tmp.array)

lon <- ncvar_get(ncDF, "lon")
nlon <- dim(lon)
#head(lon)

lat <- ncvar_get(ncDF, "lat")
nlat <- dim(lat)


yr <- ncvar_get(ncDF, "nbnds")
