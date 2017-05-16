## calculo de la radiación por regiones para ver las diferencias entre modelos y modelos y satelite

library(raster)

## MASCARA

zonas <- raster("zonas.grd")

#### DATOS SAT  ####

SIS <- stack("../../data/SAT/SISdm20032009eur.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
SIS <- setZ(SIS, idx)

## mascara para asignar esa proyeccion al satelite:

mascara <- raster("../../figs/masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../../figs/masque_terre_mer.nc", varname='lat')
maslon <- raster("../../figs/masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])

# Specify the lonlat as spatial points with projection as long/lat

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")
maslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)


## Una vez que tenemos el raster de la máscara con la extensión y la proyección bien definida, proyectamos el raster dl satélite en lat lon a la nueva proyección.

newproj <- projectExtent(mascara, mycrs)
SISproy <- projectRaster(SIS, newproj)
SISproy <- setZ(SISproy, idx)

############################

year <- function(x) as.numeric(format(x, '%y'))
SISy <- zApply(SISproy, by=year, fun='mean')

## series anuales por zonas:
 
sat_rsds_yearlyMean_zones <- zonal(SISy, zonas, fun='mean')
save(sat_rsds_yearlyMean_zones, file='sat_rsds_yearlyMean_zones.Rdata')

## ciclo anual

month <- function(x) as.numeric(format(x, '%m'))
SIS_ciclo <- zApply(SISproy, by=month, fun='mean')

sat_rsds_cicloMean_zones <- zonal(SIS_ciclo, zonas, fun='mean')
save(sat_rsds_cicloMean_zones, file='sat_rsds_cicloMean_zones.Rdata')

##################################

## CAER ##

## asigno la proyeccion al raster correctamente:

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
rsdslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)
extent(rsds) <- extent(prsdslonlat)

## Hago las medias anuales de la simulación C-AER

rsdsy <- zApply(rsds, by=year, fun='mean')

## series anuales por zonas

caer_rsds_yearlyMean_zones <- zonal(rsdsy, zonas, fun='mean')
save(caer_rsds_yearlyMean_zones, file='caer_rsds_yearlyMean_zones.Rdata')

## ciclo anual

month <- function(x) as.numeric(format(x, '%m'))
caer_ciclo <- zApply(rsds, by=month, fun='mean')

caer_rsds_cicloMean_zones <- zonal(caer_ciclo, zonas, fun='mean')
save(caer_rsds_cicloMean_zones, file='caer_rsds_cicloMean_zones.Rdata')

## CNO ##

rsdsno <- stack("../../data/C-NO/rsds_no_day_20032009.nc")
rsdsno <- setZ(rsdsno, idx) 

## defino el raster del modelo bien:

rsdslat <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lat')
rsdslon <- raster("../../data/C-NO/rsds_no_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdslat)
prsdslon <- rasterToPoints(rsdslon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)
extent(rsdsno) <- extent(prsdslonlat)

## Hago las medias anuales de la simulación C-AER

rsdsyno <- zApply(rsdsno, by=year, fun='mean')

## series anuales por zonas

cno_rsds_yearlyMean_zones <- zonal(rsdsyno, zonas, fun='mean')
save(cno_rsds_yearlyMean_zones, file='cno_rsds_yearlyMean_zones.Rdata')

## ciclo anual

cno_ciclo <- zApply(rsdsno, by=month, fun='mean')

cno_rsds_cicloMean_zones <- zonal(cno_ciclo, zonas, fun='mean')
save(cno_rsds_cicloMean_zones, file='cno_rsds_cicloMean_zones.Rdata')

##################################
## DIFERENCIAS ##

Dif_caer_sat_zonas <- caer_rsds_yearlyMean_zones[,2:8]- sat_rsds_yearlyMean_zones[,2:8]
Dif_caer_sat_zonas <- as.data.frame(Dif_caer_sat_zonas)
Dif_caer_sat_zonas$zonas <- 1:9

Dif_cno_sat_zonas <- cno_rsds_yearlyMean_zones[,2:8]- sat_rsds_yearlyMean_zones[,2:8]
Dif_cno_sat_zonas <- as.data.frame(Dif_cno_sat_zonas)
Dif_cno_sat_zonas$zonas <- 1:9

save(Dif_caer_sat_zonas, file='Dif_caer_sat_zonas.Rdata')
save(Dif_cno_sat_zonas, file='Dif_cno_sat_zonas.Rdata')

Dif_rel_caer_sat_zonas <- Dif_caer_sat_zonas[,1:7]/sat_rsds_yearlyMean_zones[,2:8]
Dif_rel_caer_sat_zonas$zonas <- 1:9
Dif_rel_cno_sat_zonas <- Dif_cno_sat_zonas[,1:7]/sat_rsds_yearlyMean_zones[,2:8]
Dif_rel_cno_sat_zonas$zonas <- 1:9

save(Dif_rel_caer_sat_zonas, file='Dif_rel_caer_sat_zonas.Rdata')
save(Dif_rel_cno_sat_zonas, file='Dif_rel_cno_sat_zonas.Rdata')
