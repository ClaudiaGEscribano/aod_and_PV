## Create a raster with daytime temperature from maximum, minimun and average daily temperature from models or satelite.

library(raster)
library(zoo)

## The reference equation for daytime temp is in Crook et al.

fooTday <- function(Tasmax, Tasmin, Tavg, tt){

    Tmax <- stack(Tasmax, varname='tasmax')
    Tmin <- stack(Tasmin, varname='tasmin')
    Tavg <- stack(Tavg, varname='tas')

    Tmax <- setZ(Tmax, tt)
    Tmin <- setZ(Tmin, tt)
    Tavg <- setZ(Tavg, tt)

    Tmm <- zApply(Tavg, by=as.yearmon, 'mean')
    DTR <- Tmax-Tmin
    DTR4 <- DTR/4
    DTR4 <- setZ(DTR4, tt)

    l <- lapply(as.yearmon(getZ(Tmm)), FUN=function(x)
        DTR4[[which(as.yearmon(getZ(DTR4)) == x)]] + Tmm[[which(getZ(Tmm) == x)]])

    a <- brick(unlist(l, recursive=TRUE))
    names(a) <- tt
    return(a)
}    
