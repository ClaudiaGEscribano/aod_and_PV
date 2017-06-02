library(raster)
library(rasterVis)
library(zoo)

## This script will compare real PV data with the assessment with satelite data.

## ## load the real data ## ##
##############################

load("../carmonaYf.Rdata")
load("carmonaSimYf_month.Rdata")

## los datos son la energía diaria acumulada. Calculo la media mensual de eergía diaria acumulada.

carmonaMon <- aggregate(Yf, by=as.yearmon, 'mean')
p <-aggregate(Yf, by=as.yearmon, FUN=function(x) length(x)) # para comprobar si faltan días
carmonaMon <- zoo(rowMeans(carmonaMon), index(carmonaMon))

## extract at the point in the stack of models en sat ##
########################################################

## 1. Hay que asignar al raster la proyección que le corresponde. Para CAER es LCC, para el satélite es long/lat.

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

#######
## CAER
#######

two <- stack("../../proj12abr/twoAxes_caer_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
two <- setZ(two, idx)
 
## defino el raster del modelo bien:

projection(two) <- projection(mascara)
extent(two) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER

twoMeses <- zApply(two, by=as.yearmon, fun='mean')

## EXTRAER AQUI

## Para extraer los puntos del modelo necesito un Spatialdata con la lat y la lon primero.
## Ahora proyecto los puntos que quiero extraer a LCC

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

twoMeses_caer <- extract(twoMeses, bsrnlonlat, method="simple")

carmona_twoMeses_caer <- as.zoo(t(twoMeses_caer), as.yearmon(idx))

#######
## SAT
#######

twosat <- stack("../../proj12abr/twoAxes_sat_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twosat <- setZ(twosat, idx) 

twoMesessat <- zApply(twosat, by=as.yearmon, fun='mean')

lat <- c(37.4)
lon <- c(-5.66)

carmonalonlat <- data.frame(lon, lat)

carmona_twoMeses_sat <- extract(twoMesessat, SpatialPoints(cbind(lon, lat)))

carmona_twoMeses_sat <- as.zoo(t(carmona_twoMeses_sat), as.yearmon(idx))
## 2. después extraer el punto en la latitud que buscamos.

## comparacion
  
c <- merge(carmonaMon, carmona_twoMeses_sat, carmona_twoMeses_caer, all=FALSE)
names(c) <- c("REAL", "SAT","CAER")

## elimino el ultimo mes porque los datos son de solo 14 dias.
c2 <- c[-18,]
 
pdf("seriesCarmona.pdf")
xyplot(c2, scales = list(y = list(relation = "same", alternating = FALSE)), superpose=TRUE, type='b')
dev.off()


pdf("seriesCarmona2.pdf")
xyplot(c2,scales = list(x = list(at = index(c2), rot=45)), superpose=TRUE, type='b')
dev.off()

## añadir cno

twocno <- stack("../../proj12abr/twoAxes_cno_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twocno <- setZ(twocno, idx)
 
## defino el raster del modelo bien:

projection(twocno) <- projection(mascara)
extent(twocno) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER

twoMesescno <- zApply(twocno, by=as.yearmon, fun='mean')

## EXTRAER AQUI

## Para extraer los puntos del modelo necesito un Spatialdata con la lat y la lon primero.
## Ahora proyecto los puntos que quiero extraer a LCC

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

twoMeses_cno <- extract(twoMesescno, bsrnlonlat, method="simple")

carmona_twoMeses_cno <- as.zoo(t(twoMeses_cno), as.yearmon(idx))

c <- merge(carmonaMon, carmona_twoMeses_sat, carmona_twoMeses_caer, carmona_twoMeses_cno, all=FALSE)
names(c) <- c("REAL", "SAT","CAER", "CNO")

## elimino el ultimo mes porque los datos son de solo 14 dias.
c2 <- c[-18,]

pdf("seriesCarmona3.pdf")
xyplot(c2,scales = list(x = list(at = index(c2), rot=45)), superpose=TRUE, type='b')
dev.off()

######## AOD #########
######################

aod <- stack("../../AOD_total_monthly20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
aod <- setZ(aod, idx)
 
## defino el raster de aod bien:

projection(aod) <- projection(mascara)
aod <- crop(aod, mascara)
extent(aod) <- extent(mascara)

aod <- extract(aod, bsrnlonlat, method="simple")
carmona_aod <- as.zoo(t(aod), as.yearmon(idx))
 
c <- merge(carmonaMon, carmona_twoMeses_sat, carmona_twoMeses_caer, carmona_twoMeses_cno, carmona_aod, all=FALSE)
names(c) <- c("REAL", "SAT","CAER", "CNO", "AOD")

c2 <- c[-18,]
 
pdf("seriesCarmonaAOD2.pdf")
xyplot(c2,screens=c(1,1,1,1,2),scales = list(x = list(at = index(c2), rot=45)), type='b')
dev.off()


## pruebas para que el panel dos sea mas pequeño o sean barras etc.

    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
 
########################################
##comparación con una simulacion de CAER con los datos del sistema corregidos################

tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
xProd <- zoo(xProd, order.by=as.yearmon(tt))
c <- merge(carmonaMon, xProd, carmona_twoMeses_caer, carmona_aod, all=FALSE)
names(c) <- c("REAL", "CAER_SIS", "CAER_GEN", "AOD")


pdf("seriesCarmonaCAER.pdf")
xyplot(c,screens=c(1,1,1,2),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
dev.off()


