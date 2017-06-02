## compute pv production in a point of the domain in order to compare with real data

## LOAD RSDS DATA FROM MODELS ##

## Tengo que proyectar los datos del modelo para poder extraer el punto

## CAER ##

SISS <- brick('../../../data/C-AER/rsds_day_20032009.nc', varname='rsds')
SISS*24
Tas <- brick('../../../data/C-AER/tas_day_20032009.nc')
 
 ## time steps
tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
names(SISS) <- tt
names(Tas) <- tt

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)

## PROJECT THE 2 RASTERS ##

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
latsis <- raster('../../../data/C-AER/rsds_day_20032009.nc', varname='lat')
lonsis <- raster('../../../data/C-AER/rsds_day_20032009.nc', varname='lon')

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

plat <- rasterToPoints(latsis)
plon <- rasterToPoints(lonsis)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
lonlat <- spTransform(lonlat, CRSobj = mycrs) 
projection(SISS) <- projection(lonlat)
extent(SISS) <- extent(lonlat)

## proyecto la temperatura

projection(Tas) <- projection(lonlat)
extent(Tas) <- extent(lonlat)

## EXTRACT THE DATA AT THE POINT ##

## latitud de la planta:

lat <- 41.1
lon <- 1.19

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

sis <- extract(SISS, bsrnlonlat, method="simple")
tas <- extract(Tas, bsrnlonlat, method="simple")

#######################################
## FUNCIÓN CALCULO DE PRODUCTIVIDAD
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

fooProd <- function(data, modeTrk = 'fixed', timePeriod = 'month'){
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

xProd <- fooProd(xx) timePeriod = 'year') 
xProd <- fooProd(xx) timePeriod = 'month')
