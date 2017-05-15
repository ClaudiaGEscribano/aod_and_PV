## Script para extraer el ciclo anual en los puntos donde están las estaciones.

library(raster)
library(rasterVis)
library(zoo)

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

## Specify the lonlat as spatial points with projection as long/lat
prsdslonlat <- SpatialPoints(rsdslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(prsdslonlat, CRSobj = mycrs) 
extent(rsds) <- extent(prsdslonlat)

## Hago las medias anuales de la simulación C-AER

month <- function(x) as.numeric(format(x, '%m'))
rsdsCiclo <- zApply(rsds, by=month, fun='mean')


## extraigo los valores mensuales en los puntos donde están las estaciones:

bsrn_rsdsCiclo_caer <- extract(rsdsCiclo, bsrnlonlat, method="simple")
save(bsrn_rsdsCiclo_caer, file='bsrn_rsdsCiclo_caer.Rdata')

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

rsdsCiclono <- zApply(rsdsno, by=month, fun='mean')

## extraigo los valores anuales en los puntos donde están las estaciones:
 
bsrn_rsdsCiclo_cno <- extract(rsdsCiclono, bsrnlonlat, method='simple')
save(bsrn_rsdsCiclo_cno, file='bsrn_rsdsCiclo_cno.Rdata')

############################################################
## SIS
############################################################

lat <- c(51.9711, 50.2167, 44.083,42.816,52.21,46.815,30.8597,22.7903)
lon <- c(4.9267, -5.3167, 5.059, -1.601, 14.122, 6.944, 34.7794, 5.5292)
bsrn <- c("cabaw", "camborne", "carpentras", "cener", "lindenberg", "pyrene", "sedeboker","tamanrasset")

bsrnlonlat <- data.frame(bsrn, lon, lat)

## datos SIS

SIS <- stack("../../data/SAT/SISdm20032009eur.nc", varname='SIS')
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
SIS <- setZ(SIS, idx)

month <- function(x) as.numeric(format(x, '%m')) 
SISCiclo <- zApply(SIS, by=month, fun='mean')

bsrn_rsdsCiclo_sat <- extract(SISCiclo, SpatialPoints(cbind(lon, lat)))
save(bsrn_rsdsCiclo_sat, file='bsrn_rsdsCiclo_sat.Rdata')
