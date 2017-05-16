## calculo de la radiación por regiones para ver las diferencias entre modelos y modelos y satelite

library(raster)

## MASCARA

zonas <- raster("zonas.grd")

## FIXED ##
 
fixedY <- stack("../proj12abr/fixed_caer_yearlyProd_temp_20032009.grd") ## C-AER
fixedYno <- stack("../proj12abr/fixed_cno_yearlyProd_temp_20032009.grd")
fixedYsat <- stack("../proj12abr/fixed_sat_yearlyProd_temp_20032009.grd")

## mask

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

#mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

mascara <- raster("../../figs/masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../../figs/masque_terre_mer.nc", varname='lat')
maslon <- raster("../../figs/masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
maslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)

## Una vez que tenemos el raster de la máscara con la extensión y la proyección bien definida, proyectamos el raster dl satélite en lat lon a la nueva proyección.

newproj <- projectExtent(mascara, mycrs) 
fixedYsat <- projectRaster(fixedYsat, newproj)

projection(fixedYno) <- projection(mascara)
extent(fixedYno) <- extent(fixedYno)

############################
 
## series anuales por zonas:
 
sat_fixed_yearlyMean_zones <- zonal(fixedYsat, zonas, fun='mean')
save(sat_fixed_yearlyMean_zones, file='sat_fixed_yearlyMean_zones.Rdata') 

## ciclo anual

fixedYCiclosat <- stack("../proj12abr/fixed_sat_monthlyProd_temp_20032009.grd")
fixedYCiclosat <- projectRaster(fixedYCiclosat, newproj)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"),'month')
fixedYCiclosat <- setZ(fixedYCiclosat,idx)

month <- function(x) as.numeric(format(x, '%m'))
fixedYCiclosat <-  zApply(fixedYCiclosat, by=month, 'mean')

sat_fixed_cicloMean_zones <- zonal(fixedYCiclosat, zonas, fun='mean')
save(sat_fixed_cicloMean_zones, file='sat_fixed_cicloMean_zones.Rdata')

##################################

## CAER ##

## asigno la proyeccion al raster correctamente:

## defino el raster del modelo bien:

rsdslat <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lat')
rsdslon <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdslat)
prsdslon <- rasterToPoints(rsdslon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)
projection(fixedY) <- projection(prsdslonlat)
extent(fixedY) <- extent(prsdslonlat)

## series anuales por zonas

caer_fixed_yearlyMean_zones <- zonal(fixedY, zonas, fun='mean')
save(caer_fixed_yearlyMean_zones, file='caer_fixed_yearlyMean_zones.Rdata')

## ciclo anual

fixedYCiclocaer <- stack("../proj12abr/fixed_caer_monthlyProd_temp_20032009.grd")
projection(fixedYCiclocaer) <- projection(prsdslonlat)
extent(fixedYCiclocaer) <- extent(prsdslonlat)

fixedYCiclocaer <- setZ(fixedYCiclocaer,idx)

month <- function(x) as.numeric(format(x, '%m'))
fixedYCiclocaer <-  zApply(fixedYCiclocaer, by=month, 'mean')

caer_fixed_cicloMean_zones <- zonal(fixedYCiclocaer, zonas, fun='mean')
save(caer_fixed_cicloMean_zones, file='caer_fixed_cicloMean_zones.Rdata')

## CNO ##

## defino el raster del modelo bien:

rsdslat <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lat')
rsdslon <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdslat)
prsdslon <- rasterToPoints(rsdslon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)
projection(fixedYno) <- projection(prsdslonlat)
extent(fixedYno) <- extent(prsdslonlat)

## Hago las medias anuales de la simulación C-AER

## series anuales por zonas

cno_fixed_yearlyMean_zones <- zonal(fixedYno, zonas, fun='mean')
save(cno_fixed_yearlyMean_zones, file='cno_fixed_yearlyMean_zones.Rdata')

## ciclo anual

fixedYCiclocno <- stack("../proj12abr/fixed_cno_monthlyProd_temp_20032009.grd")
projection(fixedYCiclocno) <- projection(prsdslonlat)
extent(fixedYCiclocno) <- extent(prsdslonlat)

fixedYCiclocno <- setZ(fixedYCiclocno,idx)

month <- function(x) as.numeric(format(x, '%m'))
fixedYCiclocno <-  zApply(fixedYCiclocno, by=month, 'mean')


cno_fixed_cicloMean_zones <- zonal(fixedYCiclocno, zonas, fun='mean')
save(cno_fixed_cicloMean_zones, file='cno_fixed_cicloMean_zones.Rdata')

##################################
## DIFERENCIAS ##

Dif_fixed_caer_sat_zonas <- caer_fixed_yearlyMean_zones[,2:8]- sat_fixed_yearlyMean_zones[,2:8]
Dif_fixed_caer_sat_zonas <- as.data.frame(Dif_fixed_caer_sat_zonas)
Dif_fixed_caer_sat_zonas$zonas <- 1:9

Dif_fixed_cno_sat_zonas <- cno_fixed_yearlyMean_zones[,2:8]- sat_fixed_yearlyMean_zones[,2:8]
Dif_fixed_cno_sat_zonas <- as.data.frame(Dif_fixed_cno_sat_zonas)
Dif_fixed_cno_sat_zonas$zonas <- 1:9

save(Dif_fixed_caer_sat_zonas, file='Dif_fixed_caer_sat_zonas.Rdata')
save(Dif_fixed_cno_sat_zonas, file='Dif_fixed_cno_sat_zonas.Rdata')
 
## ONE ##

oneY <- stack("../proj12abr/oneAxis_caer_yearlyProd_temp_20032009.grd") ## C-AER
oneYno <- stack("../proj12abr/oneAxis_cno_yearlyProd_temp_20032009.grd")
oneYsat <- stack("../proj12abr/oneAxis_sat_yearlyProd_temp_20032009.grd")

oneYsat <- projectRaster(oneYsat, newproj)

## yearly sat

sat_one_yearlyMean_zones <- zonal(oneYsat, zonas, fun='mean')
save(sat_one_yearlyMean_zones, file='sat_one_yearlyMean_zones.Rdata') 

## ciclo anual

oneYCiclosat <- stack("../proj12abr/oneAxis_sat_monthlyProd_temp_20032009.grd")
oneYCiclosat <- projectRaster(oneYCiclosat, newproj)

oneYCiclosat <- setZ(oneYCiclosat,idx)

month <- function(x) as.numeric(format(x, '%m'))
oneYCiclosat <-  zApply(oneYCiclosat, by=month, 'mean')

sat_one_cicloMean_zones <- zonal(oneYCiclosat, zonas, fun='mean')
save(sat_one_cicloMean_zones, file='sat_one_cicloMean_zones.Rdata')

## CAER

projection(oneY) <- projection(prsdslonlat)
extent(oneY) <- extent(prsdslonlat)

caer_one_yearlyMean_zones <- zonal(oneY, zonas, fun='mean')
save(caer_one_yearlyMean_zones, file='caer_one_yearlyMean_zones.Rdata')

## ciclo anual
 
oneYCiclocaer <- stack("../proj12abr/oneAxis_caer_monthlyProd_temp_20032009.grd")

projection(oneYCiclocaer) <- projection(prsdslonlat) 
extent(oneYCiclocaer) <- extent(prsdslonlat)

oneYCiclocaer <- setZ(oneYCiclocaer,idx)

month <- function(x) as.numeric(format(x, '%m'))
oneYCiclosat <-  zApply(oneYCiclocaer, by=month, 'mean')

caer_one_cicloMean_zones <- zonal(oneYCiclocaer, zonas, fun='mean')
save(caer_one_cicloMean_zones, file='caer_one_cicloMean_zones.Rdata')

## CNO

projection(oneYno) <- projection(prsdslonlat)
extent(oneYno) <- extent(prsdslonlat)
 
cno_one_yearlyMean_zones <- zonal(oneYno, zonas, fun='mean')
save(cno_one_yearlyMean_zones, file='cno_one_yearlyMean_zones.Rdata')

## ciclo anual
 
oneYCiclocno <- stack("../proj12abr/oneAxis_cno_monthlyProd_temp_20032009.grd")

projection(oneYCiclocno) <- projection(prsdslonlat) 
extent(oneYCiclocno) <- extent(prsdslonlat)

oneYCiclocno <- setZ(oneYCiclocno,idx)

month <- function(x) as.numeric(format(x, '%m'))
oneYCiclocno <-  zApply(oneYCiclocno, by=month, 'mean')

cno_one_cicloMean_zones <- zonal(oneYCiclocno, zonas, fun='mean')
save(cno_one_cicloMean_zones, file='cno_one_cicloMean_zones.Rdata')

##

Dif_one_caer_sat_zonas <- caer_one_yearlyMean_zones[,2:8]- sat_one_yearlyMean_zones[,2:8]
Dif_one_caer_sat_zonas <- as.data.frame(Dif_one_caer_sat_zonas)
Dif_one_caer_sat_zonas$zonas <- 1:9
 
Dif_one_cno_sat_zonas <- cno_one_yearlyMean_zones[,2:8]- sat_one_yearlyMean_zones[,2:8]
Dif_one_cno_sat_zonas <- as.data.frame(Dif_one_cno_sat_zonas)
Dif_one_cno_sat_zonas$zonas <- 1:9
 
save(Dif_one_caer_sat_zonas, file='Dif_one_caer_sat_zonas.Rdata')
save(Dif_one_cno_sat_zonas, file='Dif_one_cno_sat_zonas.Rdata')

## TWO ##

twoY <- stack("../proj12abr/twoAxes_caer_yearlyProd_temp_20032009.grd") ## C-AER
twoYno <- stack("../proj12abr/twoAxes_cno_yearlyProd_temp_20032009.grd")
twoYsat <- stack("../proj12abr/twoAxes_sat_yearlyProd_temp_20032009.grd")

twoYsat <- projectRaster(twoYsat, newproj)

## yearly sat

sat_two_yearlyMean_zones <- zonal(twoYsat, zonas, fun='mean')
save(sat_two_yearlyMean_zones, file='sat_two_yearlyMean_zones.Rdata') 

##ciclo anual
 
twoYCiclosat <- stack("../proj12abr/twoAxes_sat_monthlyProd_temp_20032009.grd")

twoYCiclosat <- projectRaster(twoYCiclosat, newproj)

twoYCiclosat <- setZ(twoYCiclosat,idx)

month <- function(x) as.numeric(format(x, '%m'))
twoYCiclosat <-  zApply(twoYCiclosat, by=month, 'mean')

sat_two_cicloMean_zones <- zonal(twoYCiclosat, zonas, fun='mean')
save(sat_two_cicloMean_zones, file='sat_two_cicloMean_zones.Rdata')

## CAER

projection(twoY) <- projection(prsdslonlat)
extent(twoY) <- extent(prsdslonlat)

caer_two_yearlyMean_zones <- zonal(twoY, zonas, fun='mean')
save(caer_two_yearlyMean_zones, file='caer_two_yearlyMean_zones.Rdata')

## ciclo anual

twoYCiclocaer <- stack("../proj12abr/twoAxes_caer_monthlyProd_temp_20032009.grd")

projection(twoYCiclocaer) <- projection(prsdslonlat) 
extent(twoYCiclocaer) <- extent(prsdslonlat)

twoYCiclocaer <- setZ(twoYCiclocaer,idx)

month <- function(x) as.numeric(format(x, '%m'))
twoYCiclosat <-  zApply(twoYCiclocaer, by=month, 'mean')

caer_two_cicloMean_zones <- zonal(twoYCiclocaer, zonas, fun='mean')
save(caer_two_cicloMean_zones, file='caer_two_cicloMean_zones.Rdata')

## CNO

projection(twoYno) <- projection(prsdslonlat)
extent(twoYno) <- extent(prsdslonlat)

cno_two_yearlyMean_zones <- zonal(twoYno, zonas, fun='mean')
save(cno_two_yearlyMean_zones, file='cno_two_yearlyMean_zones.Rdata')

## ciclo anual

twoYCiclocno <- stack("../proj12abr/twoAxes_cno_monthlyProd_temp_20032009.grd")

projection(twoYCiclocno) <- projection(prsdslonlat) 
extent(twoYCiclocno) <- extent(prsdslonlat)

twoYCiclocno <- setZ(twoYCiclocno,idx)

month <- function(x) as.numeric(format(x, '%m'))
twoYCiclocno <-  zApply(twoYCiclocno, by=month, 'mean')

cno_two_cicloMean_zones <- zonal(twoYCiclocno, zonas, fun='mean')
save(cno_two_cicloMean_zones, file='cno_two_cicloMean_zones.Rdata')

##

Dif_two_caer_sat_zonas <- caer_two_yearlyMean_zones[,2:8]- sat_two_yearlyMean_zones[,2:8]
Dif_two_caer_sat_zonas <- as.data.frame(Dif_two_caer_sat_zonas)
Dif_two_caer_sat_zonas$zonas <- 1:9
 
Dif_two_cno_sat_zonas <- cno_two_yearlyMean_zones[,2:8]- sat_two_yearlyMean_zones[,2:8]
Dif_two_cno_sat_zonas <- as.data.frame(Dif_two_cno_sat_zonas)
Dif_two_cno_sat_zonas$zonas <- 1:9
 
save(Dif_two_caer_sat_zonas, file='Dif_two_caer_sat_zonas.Rdata')
save(Dif_two_cno_sat_zonas, file='Dif_two_cno_sat_zonas.Rdata')

#### Diferencias relativas por zonas ####

Dif_rel_fixed_caer_sat_zonas<- (caer_fixed_yearlyMean_zones[,2:8]- sat_fixed_yearlyMean_zones[,2:8])/sat_fixed_yearlyMean_zones[,2:8]
Dif_rel_fixed_caer_sat_zonas<- as.data.frame(Dif_rel_fixed_caer_sat_zonas)
Dif_rel_fixed_caer_sat_zonas$zonas <- 1:9

save(Dif_rel_fixed_caer_sat_zonas, file='Dif_rel_fixed_caer_sat_zonas.Rdata')

Dif_rel_fixed_cno_sat_zonas<- (cno_fixed_yearlyMean_zones[,2:8]- sat_fixed_yearlyMean_zones[,2:8])/sat_fixed_yearlyMean_zones[,2:8]
Dif_rel_fixed_cno_sat_zonas<- as.data.frame(Dif_rel_fixed_cno_sat_zonas)
Dif_rel_fixed_cno_sat_zonas$zonas <- 1:9

save(Dif_rel_fixed_cno_sat_zonas, file='Dif_rel_fixed_cno_sat_zonas.Rdata')


## one

Dif_rel_one_caer_sat_zonas <- (caer_one_yearlyMean_zones[,2:8]- sat_one_yearlyMean_zones[,2:8])/sat_one_yearlyMean_zones[,2:8]
Dif_rel_one_caer_sat_zonas<- as.data.frame(Dif_rel_one_caer_sat_zonas)
Dif_rel_one_caer_sat_zonas$zonas <- 1:9

save(Dif_rel_one_caer_sat_zonas, file='Dif_rel_one_caer_sat_zonas.Rdata')

Dif_rel_one_cno_sat_zonas<- (cno_one_yearlyMean_zones[,2:8]- sat_one_yearlyMean_zones[,2:8])/sat_one_yearlyMean_zones[,2:8]
Dif_rel_one_cno_sat_zonas<- as.data.frame(Dif_rel_one_cno_sat_zonas)
Dif_rel_one_cno_sat_zonas$zonas <- 1:9

save(Dif_rel_one_cno_sat_zonas, file='Dif_rel_one_cno_sat_zonas.Rdata')

## two

Dif_rel_two_caer_sat_zonas <- (caer_two_yearlyMean_zones[,2:8]- sat_two_yearlyMean_zones[,2:8])/sat_two_yearlyMean_zones[,2:8]
Dif_rel_two_caer_sat_zonas<- as.data.frame(Dif_rel_two_caer_sat_zonas)
Dif_rel_two_caer_sat_zonas$zonas <- 1:9
 
save(Dif_rel_two_caer_sat_zonas, file='Dif_rel_two_caer_sat_zonas.Rdata')

Dif_rel_two_cno_sat_zonas<- (cno_two_yearlyMean_zones[,2:8]- sat_two_yearlyMean_zones[,2:8])/sat_two_yearlyMean_zones[,2:8]
Dif_rel_two_cno_sat_zonas<- as.data.frame(Dif_rel_two_cno_sat_zonas)
Dif_rel_two_cno_sat_zonas$zonas <- 1:9

save(Dif_rel_two_cno_sat_zonas, file='Dif_rel_two_cno_sat_zonas.Rdata')
