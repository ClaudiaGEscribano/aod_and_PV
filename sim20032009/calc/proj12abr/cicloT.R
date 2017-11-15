## Este script es para crear un raster con la temperatura diurna a partir de los datos de temperatura máxima y mínima del modelo.

library(raster)
library(zoo)

## La ecuación que vamos a seguir es la ecuación de la referencia de Crook et al.

function <- fooTday(Tmax, Tmin, Tavg, tt){

    Tmax <- stack(Tmax, varname='tas')
    Tmin <- stack(Tmin, varname='tas')
    Tavg <- stack(Tavg, varname='tas')
    
    Tmax <- setZ(Tmax, tt)
    Tmin <- setZ(Tmin, tt)
    Tavg <- stack(Tavg, tt)
    
    Tmm <- zApply(Tavg, by=as.yearmon, 'mean')
    DTR <- Tmax-Tmin
    DTR4 <- DTR/4

    
    foo  <- function(x) {Tmm + (Tmax-Tmin)/4
    
