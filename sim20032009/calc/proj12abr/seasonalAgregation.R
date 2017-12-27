## Este script agrupa las salidas mensuales

library(zoo)
library(raster)

source('../../figs/proj13abr/projectInfo.R')
## Leo los datos de salida de las simulaciones que vaya a representar.

fixedAER <- stack("outputTciclo/fixed_caer_monthlyProd_tday_20032009.grd")
fixedNO <- stack("outputTciclo/fixed_cno_monthlyProd_tday_20032009.grd")
oneAER <- stack("outputTciclo/oneAxis_caer_monthlyProd_tday_20032009.grd")
oneNO <- stack("outputTciclo/oneAxis_cno_monthlyProd_tday_20032009.grd")
twoAER <- stack("outputTciclo/twoAxis_caer_monthlyProd_tday_20032009.grd")
twoNO <- stack("outputTciclo/twoAxes_cno_monthlyProd_tday_20032009.grd")

fooSeason <- function(data, a, b, c){
    idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
    datamm <- setZ(data, idx)
 
    monthIndex <- function(k){
    c <- c(1:7)
    j <- k
    for (i in 1:7){
        c[i] <- j
        j <- j+12}
    return(c)
}
 
    lista <- list()
    for (i in 1:12) lista[[i]] <- monthIndex(i)

    Meses <- lapply(lista, FUN=function(x) subset(datamm, x))
    a <- Meses[[a]]
    b <- Meses[[b]]
    c <- Meses[[c]]

    season <- stack(a,b,c)
    season <- mean(season)
    return(season)
}

## winter ##

invierno <- fooSeason(fixedAER, 1,2,12)
writeRaster(invierno, filename='outputTciclo/DJF_fixed_caer.grd', overwrite=TRUE)
invierno <- fooSeason(fixedNO, 1,2,12)
writeRaster(invierno, filename='outputTciclo/DJF_fixed_cno.grd', overwrite=TRUE)
invierno <- fooSeason(oneAER, 1,2,12)
writeRaster(invierno, filename='outputTciclo/DJF_one_caer.grd', overwrite=TRUE)
invierno <- fooSeason(oneNO, 1,2,12)
writeRaster(invierno, filename='outputTciclo/DJF_one_cno.grd', overwrite=TRUE)
invierno <- fooSeason(twoAER, 1,2,12)
writeRaster(invierno, filename='outputTciclo/DJF_two_caer.grd', overwrite=TRUE)
invierno <- fooSeason(twoNO, 1,2,12)
writeRaster(invierno, filename='outputTciclo/DJF_two_cno.grd', overwrite=TRUE)

## spring ##

primavera <- fooSeason(fixedAER, 3,4,5)
writeRaster(primavera, filename='outputTciclo/MAM_fixed_caer.grd', overwrite=TRUE)
primavera <- fooSeason(fixedNO, 3,4,5)
writeRaster(primavera, filename='outputTciclo/MAM_fixed_cno.grd', overwrite=TRUE)
primavera <- fooSeason(oneAER, 3,4,5)
writeRaster(primavera, filename='outputTciclo/MAM_one_caer.grd', overwrite=TRUE)
primavera <- fooSeason(oneNO, 3,4,5)
writeRaster(primavera, filename='outputTciclo/MAM_one_cno.grd', overwrite=TRUE)
primavera <- fooSeason(twoAER, 3,4,5)
writeRaster(primavera, filename='outputTciclo/MAM_two_caer.grd', overwrite=TRUE) 
primavera <- fooSeason(twoNO, 3,4,5)
writeRaster(primavera, filename='outputTciclo/MAM_two_cno.grd', overwrite=TRUE)
 
## summer ##

verano <- fooSeason(fixedAER, 6,7,8)
writeRaster(verano, filename='outputTciclo/JJA_fixed_caer.grd', overwrite=TRUE)
verano <- fooSeason(fixedNO, 6,7,8)
writeRaster(verano, filename='outputTciclo/JJA_fixed_cno.grd', overwrite=TRUE)
verano <- fooSeason(oneAER, 6,7,8)
writeRaster(verano, filename='outputTciclo/JJA_one_caer.grd', overwrite=TRUE)
verano <- fooSeason(oneNO, 6,7,8)
writeRaster(verano, filename='outputTciclo/JJA_one_cno.grd', overwrite=TRUE)
verano <- fooSeason(twoAER, 6,7,8)
writeRaster(verano, filename='outputTciclo/JJA_two_caer.grd', overwrite=TRUE) 
verano <- fooSeason(twoNO, 6,7,8)
writeRaster(verano, filename='outputTciclo/JJA_two_cno.grd', overwrite=TRUE)

## autumn ##

otom  <- fooSeason(fixedAER, 9,10,11)
writeRaster(otom, filename='outputTciclo/SON_fixed_caer.grd', overwrite=TRUE)
otom <- fooSeason(fixedNO, 9,10,11)
writeRaster(otom, filename='outputTciclo/SON_fixed_cno.grd', overwrite=TRUE)
otom <- fooSeason(oneAER, 9,10,11)
writeRaster(otom, filename='outputTciclo/SON_one_caer.grd', overwrite=TRUE)
otom <- fooSeason(oneNO, 9,10,11)
writeRaster(otom, filename='outputTciclo/SON_one_cno.grd', overwrite=TRUE)
otom <- fooSeason(twoAER, 9,10,11)
writeRaster(otom, filename='outputTciclo/SON_two_caer.grd', overwrite=TRUE) 
otom <- fooSeason(twoNO, 9,10,11)
writeRaster(otom, filename='outputTciclo/SON_two_cno.grd', overwrite=TRUE)
