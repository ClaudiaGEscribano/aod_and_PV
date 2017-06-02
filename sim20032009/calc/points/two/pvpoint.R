## compute pv production in a point of the domain in order to compare with real data

## LOAD RSDS DATA FROM MODELS ##

## Tengo que proyectar los datos del modelo para poder extraer el punto

## CAER ##

SISS <- brick('../../../data/C-AER/rsds_day_20032009.nc', varname='rsds')
SISS <- SISS*24
Tas <- brick('../../../data/C-AER/tas_day_20032009.nc')

## time steps
tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
names(SISS) <- tt
names(Tas) <- tt

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)

## PROJECT THE 2 RASTERS ##

## mascara ##

mascara <- raster("../../../figs/masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../../../figs/masque_terre_mer.nc", varname='lat')
maslon <- raster("../../../figs/masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
maslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)
##

projection(SISS) <- projection(mascara)
extent(SISS) <- extent(mascara)


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

projection(Tas) <- projection(mascara)
extent(Tas) <- extent(mascara)

## EXTRACT THE DATA AT THE POINT ##

## latitud de la planta:

lat <- 37.4
lon <- -5.66

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

sis <- extract(SISS, bsrnlonlat, method="simple")
tas <- extract(Tas, bsrnlonlat, method="simple")

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
                     beta=12,
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

xProd <- fooProd(xx, timePeriod = 'year')
xProd <- fooProd(xx, timePeriod = 'month')
