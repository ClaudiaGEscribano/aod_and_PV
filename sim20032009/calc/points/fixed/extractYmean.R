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
 
fixed <- stack("../../proj12abr/fixed_caer_yearlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
fixed <- setZ(fixed, idx)
 
## defino el raster del modelo bien:

projection(fixed) <- projection(mascara)
extent(fixed) <- extent(mascara)

bsrn_fixedY_caer <- extract(fixed, bsrnlonlat, method="simple")
save(bsrn_fixedY_caer, file='bsrn_fixedY_caer.Rdata')

##########################################################
## CNO
#########################################################
 
fixedno <- stack("../../proj12abr/fixed_cno_yearlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
fixed <- setZ(fixed, idx)
 
## defino el raster del modelo bien:

projection(fixedno) <- projection(mascara)
extent(fixedno) <- extent(mascara)

bsrn_fixedY_cno <- extract(fixedno, bsrnlonlat, method="simple")
save(bsrn_fixedY_cno, file='bsrn_fixedY_cno.Rdata')

##########################################################
## SAT
#########################################################

fixedsat <- stack("../../proj12abr/fixed_sat_yearlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
fixedsat <- setZ(fixedsat, idx)

## datos latlon

lat <- c(51.9711, 50.2167, 44.083,42.816,52.21,46.815,30.8597,22.7903)
lon <- c(4.9267, -5.3167, 5.059, -1.601, 14.122, 6.944, 34.7794, 5.5292)
bsrn <- c("cabaw", "camborne", "carpentras", "cener", "lindenberg", "pyrene", "sedeboker","tamanrasset")

bsrnlonlat <- data.frame(bsrn, lon, lat)

bsrn_fixedY_sat <- extract(fixedsat, SpatialPoints(cbind(lon, lat)))
save(bsrn_fixedY_sat, file='bsrn_fixedY_sat.Rdata')
