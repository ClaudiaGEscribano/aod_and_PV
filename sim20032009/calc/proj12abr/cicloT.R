## Este script es para crear un raster con la temperatura diurna a partir de los datos de temperatura máxima y mínima del modelo.

library(raster)
library(zoo)

## La ecuación que vamos a seguir es la ecuación de la referencia de Crook et al.

## tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')

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

##    DTR4[[which(as.yearmon(getZ(DTR4)) == "ene 2003")]]+Tmm[[which(getZ(Tmm) == "ene 2003")]]
    
##    DTR4[[which(as.yearmon(getZ(DTR4)) == "ene 2003")]]+Tmm[[which(getZ(Tmm) == "ene 2003")]]
