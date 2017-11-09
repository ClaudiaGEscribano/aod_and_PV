## compute pv production in a point of the domain in order to compare with real data

library(raster)
library(solaR)

###############################################
## 1. LOAD RSDS DATA FROM MODELS ##
###############################################

## 1.1 Tengo que proyectar los datos del modelo para poder extraer el punto

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

SAT <- fooProjregular('../../../data/SAT/SISdm20032009_med44.nc', var='SIS')
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
 
SISS <- fooproj('../../../data/C-AER/rsds_day_20032009.nc', crslcc, var='rsds')
SISS <- SISS*24

SISSno <- fooproj('../../../data/C-NO/rsds_no_day_20032009.nc', crslcc, var='rsds')
SISSno <- SISSno*24

Tas <- fooproj('../../../data/C-AER/tas_day_20032009.nc', crslcc, var='tas')
Tasno <- fooproj('../../../data/C-NO/tas_no_day_20032009.nc', crslcc, var='tas')

## Añado indice temporal

tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')

names(SISS) <- tt
names(Tas) <- tt
names(SAT) <- tt
names(SISSno) <- tt
names(Tasno) <- tt

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)
SISSno <- setZ(SISSno, tt)
Tasno <- setZ(Tasno, tt)
SAT <- setZ(SAT, tt)

## 1.2 EXTRACT THE DATA AT THE POINT ##

## latitud de la planta:

lat <- 41.1
lon <- 1.19

#lat <- 37.4
#lon <- -5.66

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

sis <- extract(SISS, bsrnlonlat, method="simple")
tas <- extract(Tas, bsrnlonlat, method="simple")

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
    BD <- zoo(data.frame(G0 = g0, Ta = Ta),
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

xProd <- fooProd(xx, timePeriod = 'year') 
xProd <- fooProd(xx, timePeriod = 'month') 
