## This script extract yearly mean data of the rasters at certain points to be compared with the stations or pv plant location data.

library(raster)
library(rasterVis)

## cargo el SpatialPoints con lat y lon de las estaciones proyectado:

load('bsrnlonlatSpatialPoints.Rdata')
mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

##########################################################
## CAER
#########################################################
 
rsds <- stack("../../data/C-AER/rsds_day_20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
rsds <- setZ(rsds, idx)
 
## defino el raster del modelo bien:

rsdslat <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lat')
rsdslon <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdslat)
prsdslon <- rasterToPoints(rsdslon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
prsdslonlat <- SpatialPoints(rsdslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(prsdslonlat, CRSobj = mycrs) 
extent(rsds) <- extent(prsdslonlat)

## Hago las medias anuales de la simulación C-AER

year <- function(x) as.numeric(format(x, '%y'))
rsdsY <- zApply(rsds, by=year, fun='mean')

## extraigo los valores anuales en los puntos donde están las estaciones:

bsrn_rsdsY_caer <- extract(rsdsY, bsrnlonlat, method="simple")
save(bsrn_rsdsY_caer, file='bsrn_rsdsY_caer.Rdata')
    
##########################################################
## CNO
#########################################################
 
rsdsno <- stack("../../data/C-NO/rsds_no_day_20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
rsdsno <- setZ(rsdsno, idx)

## defino el raster del modelo bien:

rsdsnolat <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lat')
rsdsnolon <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdsnolat)
prsdslon <- rasterToPoints(rsdsnolon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
prsdslonlat <- SpatialPoints(rsdslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(prsdslonlat, CRSobj = mycrs) 
extent(rsdsno) <- extent(prsdslonlat)

## Hago las medias anuales de la simulación C-AER

rsdsYno <- zApply(rsdsno, by=year, fun='mean')

## extraigo los valores anuales en los puntos donde están las estaciones:

bsrn_rsdsY_cno <- extract(rsdsYno, bsrnlonlat, method='simple')
save(bsrn_rsdsY_cno, file='bsrn_rsdsY_cno.Rdata')

###########################################################
## 3. SAT
###########################################################

## Para extraer los valores de satélite puedo hacerlo directamente con la lon lat porque es una malla regular. ¿Debería hacerlo con la proyectada para asegurarme de que estoy tomando las mismas celdas?

## primero sin proyectar.

## datos latlon

lat <- c(51.9711, 50.2167, 44.083,42.816,52.21,46.815,30.8597,22.7903)
lon <- c(4.9267, -5.3167, 5.059, -1.601, 14.122, 6.944, 34.7794, 5.5292)
bsrn <- c("cabaw", "camborne", "carpentras", "cener", "lindenberg", "pyrene", "sedeboker","tamanrasset")

bsrnlonlat <- data.frame(bsrn, lon, lat)

## datos SIS

SIS <- stack("../../data/SAT/SISdm20032009eur.nc", varname='SIS')
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
SIS <- setZ(SIS, idx)

year <- function(x) as.numeric(format(x, '%y'))
SISY <- zApply(SIS, by=year, fun='mean')

bsrn_rsdsY_sat <- extract(SISY, SpatialPoints(cbind(lon, lat)))
save(bsrn_rsdsY_sat, file='bsrn_rsdsY_sat.Rdata')
