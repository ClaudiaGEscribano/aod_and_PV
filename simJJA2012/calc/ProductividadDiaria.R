## Daily productivity with intradaily solar radiation data

library(raster)
library(solaR)
library(parallel)

## load the data

SIS <-  stack("../data/NO/NO_rsds_3h_jja2012.nc")
SIS <- SIS*3

Tas <- stack("../data/NO/NO_tas_3h_jja2012.nc2")

tt <- seq(as.POSIXct("2012-06-01 00:00"),as.POSIXct("2012-08-31 00:00"), 10800) ## 108000 son los segundos que tienen 3 horas.

## SOURCE ##

source(fooProd.R)

## Datos de latitud

lat <- raster("../data/NO/NO_rsds_3h_jja2012.nc", varname='lat')

SIS <- setZ(SIS, tt)
Tas <- setZ(Tas, tt)
