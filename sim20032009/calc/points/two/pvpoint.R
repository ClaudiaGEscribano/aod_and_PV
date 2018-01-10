## compute pv production in a point of the domain in order to compare with real data

## LOAD SSR DATA FROM MODELS ##

library(raster)
library(solaR)

## Tengo que proyectar los datos del modelo para poder extraer el punto

## 1. Asigno proyección a modelos con malla lcc ##

crslcc <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
crslonlat <- CRS("+proj=longlat +datum=WGS84")    

fooproj <- function(rutadatos, proj, var){
    lat <- raster(rutadatos, varname='lat')
    lon <- raster(rutadatos, varname='lon')

    plat <- rasterToPoints(lat)
    plon <- rasterToPoints(lon)
    plonlat <- cbind(plon[,3], plat[,3])

    splonlat <- SpatialPoints(plonlat,proj4string=crs("+proj=longlat +datum=WGS84"))
    lonlat <- spTransform(splonlat, CRSobj = proj)                                                                      
    s <- stack(rutadatos, varname=var)
    projection(s) <- proj
    extent(s) <- extent(lonlat)

    return(s)
}

AER <- fooproj("/home/datos/aod/sim20032009/data/C-AER/rsds_day_20032009.nc", crslcc, 'rsds')
AER <- AER*24 

NO <- fooproj("/home/datos/aod/sim20032009/data/C-NO/rsds_no_day_20032009.nc", crslcc, 'rsds')
NO <- NO*24 

## Si la función tiene proyeccion regular:

fooprojRegular <- function(rutadatos, proj, var){
    s <- stack(rutadatos, varname=var)
    lat <- init(s, 'y')
    lon <- init(s, 'x')

    plat <- rasterToPoints(lat)
    plon <- rasterToPoints(lon)
    plonlat <- cbind(plon[,3], plat[,3])

    splonlat <- SpatialPoints(plonlat,proj4string=crs("+proj=longlat +datum=WGS84"))

    projection(s) <- proj
    extent(s) <- extent(splonlat)

    return(s)
}

sissat <- fooprojRegular("/home/datos/aod/sim20032009/data/SAT/SISdm20032009_med44.nc", crslonlat, 'SIS')
sissat <- sissat*24 

#### TEMPERATURA ##

source("/home/claudia/aod_and_PV/sim20032009/calc/proj12abr/cicloT.R")
tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')

Tasmax <- '/home/datos/aod/sim20032009/data/C-AER/tmax/caer_tasmax_day_20032009.nc'
Tasmin <- '/home/datos/aod/sim20032009/data/C-AER/tmin/caer_tmin_day_20032009.nc'
Tavg <- '/home/datos/aod/sim20032009/data/C-AER/tas_day_20032009.nc' 
 
Tas <- fooTday(Tasmax, Tasmin, Tavg, tt)

Tasmax <- '/home/datos/aod/sim20032009/data/C-NO/tmax/cno_tasmax_day_20032009.nc'
Tasmin <- '/home/datos/aod/sim20032009/data/C-NO/tmin/cno_tmin_day_20032009.nc'
Tavg <- '/home/datos/aod/sim20032009/data/C-NO/tas_no_day_20032009.nc' 
 
Tasno <- fooTday(Tasmax, Tasmin, Tavg, tt)

Tasmax <- '/home/datos/aod/sim20032009/data/TEMP/tx_0.50_20032009.nc'
Tasmin <- '/home/datos/aod/sim20032009/data/TEMP/tn_0.50_20032009.nc'
Tavg <- '/home/datos/aod/sim20032009/data/TEMP/tg_0.50_20032009.nc' 

## función ciclo diurnoi de temp para los datos de ECAD

fooTdayREG <- function(Tasmax, Tasmin, Tavg, tt){

    Tmax <- stack(Tasmax, varname='tx')
    Tmin <- stack(Tasmin, varname='tn')
    Tavg <- stack(Tavg, varname='tg')

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

TasSAT <- fooTdayREG(Tasmax, Tasmin, Tavg, tt)

## Asigno la proyección a TAS

projection(Tas) <- projection(AER)
extent(Tas) <- extent(AER)
 
projection(Tasno) <- projection(AER)
extent(Tasno) <- extent(AER)

projection(TasSAT) <- projection(sissat)
extent(TasSAT) <- extent(sissat)

##

names(AER) <- tt
names(NO) <- tt
names(Tas) <- tt
names(Tasno) <- tt

AER <- setZ(AER, tt)
NO <- setZ(NO, tt)
Tas <- setZ(Tas, tt)
Tasno <- setZ(Tasno, tt)

SAT <- setZ(sissat, tt)
names(SAT) <- tt
TasSAT <- setZ(TasSAT, tt)
names(TasSAT) <-  tt

## EXTRACT THE DATA AT THE POINT ##

## latitud de la planta:

lat <- 37.4
lon <- -5.66

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

sis <- extract(AER, bsrnlonlat, method="simple")
tas <- extract(Tas, bsrnlonlat, method="simple")
sisno <- extract(NO, bsrnlonlat, method="simple")
tasno <- extract(Tasno, bsrnlonlat, method="simple")

## extraer en SAT

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))

sisSAT <- extract(SAT, bsrnlonlat, method="simple")
tasSAT <- extract(TasSAT, bsrnlonlat, method="simple")
#######################################
## FUNCIÓN CALCULO DE PRODUCTIVIDAD
#######################################

## data from the installation ##

Vmn <- 46.08
Imn <- 4.49
Iscn <- 4.7
Vocn <- 57.6
Ncs <- 96
Ncp <- 1

Nmp <- 12
Nms <- 11

Pg <- Nmp*Nms*(Vmn*Imn)

Pinv <- 25000
Vmin <- 405

fooProd <- function(data, modeTrk = 'two', timePeriod = 'month'){
    ## Number of days
    n <- (length(data) - 1)/2
    lat <- data[1]
    g0 <- data[2:(n + 1)]
    Ta <- data[(n + 2):(2*n + 1)]
    BD <- zoo(data.frame(G0 = g0, Ta = Ta),
              order.by = tt)
    Prod <- prodGCPV(lat = lat,
                     modeRad = 'bd',
                     dataRad= list(lat = lat, file = BD),
                     modeTrk = modeTrk,
                     #beta=12,
                     module=list(Vocn=57.6, Iscn=4.7, Vmn=46.08, Imn=4.49, Ncs=96, Ncp=1),
                     generator=list(Nms=11,Nmp=12),
                     inverter=list(Ki=c(0.01,0.025,0.05),Pinv=25000,Vmin=405)
                     )
    switch(timePeriod,
           year = as.data.frameY(Prod)['Yf'],
           month = as.data.frameM(Prod)['Yf']
           )
}

sis <- as.vector(sis)
tas <- as.vector(tas)
xx <- c(lat, sis, tas)

sisno <- as.vector(sisno)
tasno <- as.vector(tasno)
xxno <- c(lat, sisno,tasno)

sisSAT <- as.vector(sisSAT)
tasSAT <- as.vector(tasSAT)
xxSAT <- c(lat, sisSAT,tasSAT)

xProd <- fooProd(xx, timePeriod = 'year')
xProdM <- fooProd(xx, timePeriod = 'month')

xProdno <- fooProd(xx, timePeriod = 'year')
xProdMno <- fooProd(xxno, timePeriod = 'month')

xProdsat <- fooProd(xxSAT, timePeriod = 'year')
xProdMsat <- fooProd(xxSAT, timePeriod = 'month')
