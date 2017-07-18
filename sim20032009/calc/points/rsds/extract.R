## This script is for extract the data from raster in the lat/long points that I define and use it for calc.

library(raster)

## Lat lon data of the BSRN estations

lat <- c(51.9711, 50.2167, 44.083,42.816,52.21,46.815,30.8597,22.7903)
lon <- c(4.9267, -5.3167, 5.059, -1.601, 14.122, 6.944, 34.7794, 5.5292)
bsrn <- c("cabaw", "camborne", "carpentras", "cener", "lindenberg", "pyrene", "sedeboker","tamanrasset")
 
bsrnlonlat <- data.frame(bsrn, lon, lat)

## MASCARA ##

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

mascara <- raster("../../figs/masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../../figs/masque_terre_mer.nc", varname='lat')
maslon <- raster("../../figs/masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
maslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

maslonlat
extent(maslonlat)

pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)
                                        # Take a look
pmaslonlat
extent(pmaslonlat)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)

##########################################################
## 1. SIS satelite
##########################################################

SIS <- stack("../../data/SAT/SISdm20032009eur.nc", varname='SIS')

## I extract all the points converting the lat lon into SpatialPoints.

uno <- subset(SIS, 1:10)
a <- extract(SIS, SpatialPoints(cbind(lon, lat)))

############################################################
## 2. MODELO: CAER
############################################################

## En este caso tendré que ver si se comporta de la misma manera cuando la proyección es otra.

## Necestio darle toda la info antes te poder extraer los valores, es decir, extraer los putnos lon lat del raster, proyectarlos a mycrs, asignarselo al raster, darle la extension correcta etc. Una vez hecho esto puedo extraer estos valores.

## load data:

rsds <- stack("../../data/C-AER/rsds_day_20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
rsds <- setZ(rsds, idx)
rsds <- rsds*24

## defino el raster del modelo bien:

rsdslat <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lat')
rsdslon <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdslat)
prsdslon <- rasterToPoints(rsdslon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdslonlat <- SpatialPoints(rsdslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)
                               
projection(rsds) <- mycrs
extent(rsds) <- extent(prsdslonlat)

## Ahora proyecto los puntos que quiero extraer a LCC

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

bsrnRSDScaer <- extract(rsds, bsrnlonlat, method='simple')


## Guardo el objeto bsrnlonlat porque es es el SpatialPoints que contiene los puntos de las estaciones proyectados y puedo usarlo para extraer de cualquier raster. Si necesito añadir más puntos porque obtenga datos de más estaciones vuelvo a este script para añadirlos en este objeto.

save(bsrnlonlat, file='bsrnlonlatSpatialPoints.Rdata')

## página de stackoverflow con info:

## https://gis.stackexchange.com/questions/200417/very-basic-question-on-extracting-data-from-tif-raster-layer-in-r-projection-n

 
#########################

## Ahora puedo extraer los puntos del raster del modelo. Si paso la máscara, el punto de Inglaterra se queda fuera. Es probable que esto sea debido a que al proyectar la máscara esa sea una celda límite. Seguramente no sea una estación que seleccrionaremos.

## puntos modelo CAER:

save(bsrnRSDScaer, file='bsrn_rsds_caer.Rdata')

######################################################
## 3.  modelo CNO:
########################################################

rsdsno <- stack("../../data/C-NO/rsds_no_day_20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
rsdsno <- setZ(rsdsno, idx)
rsdsno <- rsdsno*24

## defino el raster del modelo bien:

rsdsnolat <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lat')
rsdsnolon <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdsnolat)
prsdslon <- rasterToPoints(rsdsnolon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdslonlat <- SpatialPoints(rsdslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)
                               
projection(rsdsno) <- mycrs
extent(rsds) <- extent(prsdslonlat)

## Ahora extraigo los mismos puntos que para la otra simulación

bsrnRSDScno <- extract(rsdsno, bsrnlonlat, method='simple')
save(bsrnRSDScno, file='bsrn_rsds_cno.Rdata')

## SATELITE:

## ! Estos datos no están multiplicados por 24.

## Primero extraigo directamente lat y lon, puesto que la proyección de los datos de satélite es una malla regular. Después proyectare los datos de satélite a lcc como para los cálculos, de tal manera que me aseguro de que los puntos que estoy sacando son los mismos en los 3 casos.

bsrnSISnoproy <- a
save(bsrnSISnoproy, file='bsrn_sis_noproy.Rdata')

