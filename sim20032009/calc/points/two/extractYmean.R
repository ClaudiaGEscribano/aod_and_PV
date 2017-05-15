## Extract the yearly productivity of fixed panels in the lat lon indicated

library(raster)
library(rasterVis)

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
 
two <- stack("../../proj12abr/twoAxes_caer_yearlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
two <- setZ(two, idx)
 
## defino el raster del modelo bien:

projection(two) <- projection(mascara)
extent(two) <- extent(mascara)

bsrn_twoY_caer <- extract(two, bsrnlonlat, method="simple")
save(bsrn_twoY_caer, file='bsrn_twoY_caer.Rdata')

##########################################################
## CNO
#########################################################
 
twono <- stack("../../proj12abr/twoAxes_cno_yearlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
twono <- setZ(twono, idx)
 
## defino el raster del modelo bien:

projection(twono) <- projection(mascara)
extent(twono) <- extent(mascara)

bsrn_twoY_cno <- extract(twono, bsrnlonlat, method="simple")
save(bsrn_twoY_cno, file='bsrn_twoY_cno.Rdata')

##########################################################
## SAT
#########################################################

twosat <- stack("../../proj12abr/twoAxes_sat_yearlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
twosat <- setZ(twosat, idx)

## datos latlon

lat <- c(51.9711, 50.2167, 44.083,42.816,52.21,46.815,30.8597,22.7903)
lon <- c(4.9267, -5.3167, 5.059, -1.601, 14.122, 6.944, 34.7794, 5.5292)
bsrn <- c("cabaw", "camborne", "carpentras", "cener", "lindenberg", "pyrene", "sedeboker","tamanrasset")

bsrnlonlat <- data.frame(bsrn, lon, lat)

bsrn_twoY_sat <- extract(twosat, SpatialPoints(cbind(lon, lat)))
save(bsrn_twoY_sat, file='bsrn_twoY_sat.Rdata')
