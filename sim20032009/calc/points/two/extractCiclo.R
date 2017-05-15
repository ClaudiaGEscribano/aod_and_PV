## Script para extraer el ciclo anual en los puntos donde están las estaciones.

library(raster)
library(rasterVis)
library(zoo)

## cargo el SpatialPoints con lat y lon de las estaciones proyectado:

load('../rsds/bsrnlonlatSpatialPoints.Rdata')
mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

## mascara data to project pvoutput data

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

##########################################################
## CAER
#########################################################

two <- stack("../../proj12abr/twoAxes_caer_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
two <- setZ(two, idx)
 
## defino el raster del modelo bien:

projection(two) <- projection(mascara)
extent(two) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER

month <- function(x) as.numeric(format(x, '%m'))
twoCiclo <- zApply(two, by=month, fun='mean')

## extraigo los valores mensuales en los puntos donde están las estaciones:

bsrn_twoCiclo_caer <- extract(twoCiclo, bsrnlonlat, method="simple")
save(bsrn_twoCiclo_caer, file='bsrn_twoCiclo_caer.Rdata')

##########################################################
## CNO
#########################################################

twono <- stack("../../proj12abr/twoAxes_cno_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twono <- setZ(twono, idx) 
 
## defino el raster del modelo bien:

projection(twono) <- projection(mascara)
extent(twono) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER
 
twoCiclono <- zApply(twono, by=month, fun='mean')

## extraigo los valores anuales en los puntos donde están las estaciones:
 
bsrn_twoCiclo_cno <- extract(twoCiclono, bsrnlonlat, method='simple')
save(bsrn_twoCiclo_cno, file='bsrn_twoCiclo_cno.Rdata')

############################################################
## SAT
############################################################

twosat <- stack("../../proj12abr/twoAxes_sat_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twosat <- setZ(twosat, idx) 

lat <- c(51.9711, 50.2167, 44.083,42.816,52.21,46.815,30.8597,22.7903)
lon <- c(4.9267, -5.3167, 5.059, -1.601, 14.122, 6.944, 34.7794, 5.5292)
bsrn <- c("cabaw", "camborne", "carpentras", "cener", "lindenberg", "pyrene", "sedeboker","tamanrasset")

bsrnlonlat <- data.frame(bsrn, lon, lat)

month <- function(x) as.numeric(format(x, '%m')) 
twoCiclosat <- zApply(twosat, by=month, fun='mean')

bsrn_twoCiclo_sat <- extract(twoCiclosat, SpatialPoints(cbind(lon, lat)))
save(bsrn_twoCiclo_sat, file='bsrn_twoCiclo_sat.Rdata')
