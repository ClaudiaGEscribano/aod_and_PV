## compute pv production in a point of the domain in order to compare with real data

library(raster)
library(solaR)

###############################################
## 1. LOAD RSDS DATA FROM MODELS ##
###############################################

## 1.1 Tengo que proyectar los datos del modelo para poder extraer el punto. Los datos de satélite están en una malla latlon regular, por lo que usaré fooProjregular para asignarle la proyección.  Los datos de las simulaciones de RCM están en proyección lcc, por lo que necesito la función fooproj para asignarlos la proyección.

## PROJECT RASTERS ##

## projections

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
crslcc <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

## función para proyectar si los datos son regulares, como en el sat.

fooProjregular <- function(rutaDatos, var){
    s <- stack(rutaDatos, varname=var)

    lat <- init(s, 'y')
    lon <- init(s, 'x')

    plat <- rasterToPoints(lat)
    plon <- rasterToPoints(lon)
    plonlat <- cbind(plon[,3], plat[,3])

    splonlat <- SpatialPoints(plonlat,proj4string=CRS("+proj=longlat +datum=WGS84"))

    projection(s) <- projection(splonlat)
    extent(s) <- extent(splonlat)

    return(s)
}            

SAT <- fooProjregular('/home/datos/aod/sim20032009/data/SAT/SISdm20032009_med44.nc', var='SIS')
SAT <- SAT*24

## función si los datos son en malla no regular (modelo)

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
 
SISS <- fooproj('/home/datos/aod/sim20032009/data/C-AER/rsds_day_20032009.nc', crslcc, var='rsds')
SISS <- SISS*24

SISSno <- fooproj('/home/datos/aod/sim20032009/data/C-NO/rsds_no_day_20032009.nc', crslcc, var='rsds')
SISSno <- SISSno*24

## TEMPERATURA DIURNA ##

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

projection(TasSAT) <- projection(SAT)
extent(TasSAT) <- extent(SAT)

projection(Tas) <- projection(SISS)
projection(Tasno) <- projection(SISS)

extent(Tas) <- extent(SISS)
extent(Tasno) <- extent(SISS)

#Tas <- fooproj('../../../data/C-AER/tas_day_20032009.nc', crslcc, var='tas')
#Tasno <- fooproj('../../../data/C-NO/tas_no_day_20032009.nc', crslcc, var='tas')

## Añado indice temporal

tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')

names(SISS) <- tt
names(Tas) <- tt
names(SAT) <- tt
names(SISSno) <- tt
names(Tasno) <- tt
names(TasSAT) <- tt

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)
SISSno <- setZ(SISSno, tt)
Tasno <- setZ(Tasno, tt)
SAT <- setZ(SAT, tt)
TasSAT <- setZ(TasSAT, tt)

## 1.2 EXTRACT THE DATA AT THE POINT ##

## latitud de la planta:

lat <- 41.1
lon <- 1.19

#lat <- 37.4
#lon <- -5.66

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

## radiación y temperatura en la latitud de la planta según los modelos (o satélite)
 
sis <- extract(SISS, bsrnlonlat, method="simple")
tas <- extract(Tas, bsrnlonlat, method="simple")

sisno <- extract(SISSno, bsrnlonlat, method="simple")
tasno <- extract(Tasno, bsrnlonlat, method="simple")

## extraer en SAT

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))

sisSAT <- extract(SAT, bsrnlonlat, method="simple")
tasSAT <- extract(TasSAT, bsrnlonlat, method="simple")

## ¿Tienen sentido los datos de temperatura diurnos que hemos calculado? con summary(t(tas)) y summary(t(tasno)) podemos ver los valores de estos vectores que se encuentran denro de rangos lógicos.

#######################################
## 2. FUNCIÓN CALCULO DE PRODUCTIVIDAD
#######################################

## data from the installation ##

lat <- 41.1
alfa <- 0
beta <- 12

## (3) Inversores ACEF de 100 kW
Pinv <- 100000
Vmin <- 450
## Ramas por inversor
Nmp <- 27
## Módulos en serie
Nms <- 35
## Modulo I-106
Vocn <- 21.6
Vmn <- 17.4
Iscn <- 6.54
Imn <- 6.09
Ncs <- 36
Ncp <- 1

Pg <- Nmp * Nms * (Vmn * Imn)/1000 ## kwp

 
fooProd <- function(data, modeTrk = 'fixed', timePeriod=''){
    ## Number of days
    n <- (length(data) - 1)/2
    lat <- data[1]
    g0 <- data[2:(n + 1)]
    Ta <- data[(n + 2):(2*n + 1)]
    BD <- zoo(data.frame(G0 = g0, Ta = 25),
              order.by = tt)
    Prod <- prodGCPV(lat = lat,
                     modeRad = 'bd',
                     dataRad= list(lat = lat, file = BD),
                     modeTrk = modeTrk,
                     beta=12,
                     alfa=0,
                     module=list(Vocn=21.6, Iscn=6.54, Vmn=17.4, Imn=6.09, Ncs=36, Ncp=1),
                     generator=list(Nms=35,Nmp=27),
                     inverter=list(Pinv=100000,Vmin=450)
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
xxno <- c(lat, sisno, tasno)

sisSAT <- as.vector(sisSAT)
tasSAT <- as.vector(tasSAT)
xxSAT <- c(lat, sisSAT,tasSAT)

xProdY <- fooProd(xx, timePeriod = 'year') 
xProd <- fooProd(xx, timePeriod = 'month') 

xProdYno <- fooProd(xxno, timePeriod='year')
xProdno <- fooProd(xxno, timePeriod = 'month') 

xProdsat <- fooProd(xxSAT, timePeriod = 'year')
xProdMsat <- fooProd(xxSAT, timePeriod = 'month')

######################################

## Función para calcular la productividad con datos de radiación efectiva de la planta.

fooProd <- function(data){
    ## Number of days
    n <- (length(data))/2
    #lat <- data[1]
    Gin <- data[1:n]
    Ta <- data[(n + 1):(2*n)]
    BD <- zoo(data.frame(Gef = Gin, Ta = 25),
              order.by = tt)
    Prod <- fProd(inclin=BD,
                     module=list(Vocn=21.6, Iscn=6.54, Vmn=17.4, Imn=6.09, Ncs=36, Ncp=1),
                     generator=list(Nms=35,Nmp=27),
                     inverter=list(Pinv=100000,Vmin=450)
                     )
}


load('../photocampa.Rdata')
G <- photocampa$G
Ta <- photocampa$G

G <- as.vector(G)
Ta < as.vector(Ta)
xx <- c(G, Ta)

xProd <- fooProd(xx)
Prod <- xProd$Pac

## la salida está en W/m2

Prod <- Prod/1000 ## kW
Yf <- Prod/Pg
mon <- aggregate(Yf, by=as.yearmon, 'mean')
