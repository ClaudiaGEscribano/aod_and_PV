library(raster)
library(rasterVis)
library(zoo)

## This script will compare real PV data with the assessment with satelite data.

## ## load the real data ## ##
##############################

load("../photocampaYf.Rdata")
load("photocampaSimYf_month.Rdata")

lat <- 41.1
lon <- 1.19

## los datos son la energía diaria acumulada. Calculo la media mensual de energía diaria acumulada.

## _______ ESTO NO SALE.
## ¿Debería eliminar los nas antes de hacer operaciones?

##Yf[is.na(Yf)] <- 0 ## Los NA los sustituyo por O. Cuando los 3 inversores den 0, hay que eliminar ese día para hacer la media.
##Yf2 <- apply(Yf, 1, 'sum') ## sumo los 3 inversores para quitar los días que los tres dan 0
##Yf <- Yf[which(Yf2 != 0),] ## Yf tiene solo los días en los que alguno de los 3 inversores es distinto de cero
## _________


photocampaMon <- aggregate(Yf, by=as.yearmon, 'mean', na.rm=TRUE)
## para comprobar si faltan días 
p <-aggregate(Yf, by=as.yearmon, FUN=function(x) length(x))
## Para los meses en los que hay Nan en algún inversor, no haygo la media, sino que tomo el único valor que hay.
photocampaMon[1,1] <- sum(photocampaMon[1,], na.rm=TRUE)
photocampaMon[1,3] <- sum(photocampaMon[1,], na.rm=TRUE)
photocampaMon <- zoo(rowMeans(photocampaMon, na.rm=TRUE), index(photocampaMon))

## extract at the point in the stack ##
#######################################

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

fixed <- stack("../../proj12abr/fixed_caer_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixed <- setZ(fixed, idx)
 
## defino el raster del modelo bien:

projection(fixed) <- projection(mascara)
extent(fixed) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER

fixedMeses <- zApply(fixed, by=as.yearmon, fun='mean')

## EXTRAER AQUI

## Para extraer los puntos del modelo necesito un Spatialdata con la lat y la lon primero.
## Ahora proyecto los puntos que quiero extraer a LCC

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

fixedMeses_caer <- extract(fixedMeses, bsrnlonlat, method="simple")

photocampa_fixedMeses_caer <- as.zoo(t(fixedMeses_caer), as.yearmon(idx))
#######
## SAT
#######

fixedsat <- stack("../../proj12abr/fixed_sat_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixedsat <- setZ(fixedsat, idx) 

fixedMesessat <- zApply(fixedsat, by=as.yearmon, fun='mean')

photocampalonlat <- data.frame(lon, lat)

photocampa_fixedMeses_sat <- extract(fixedMesessat, SpatialPoints(cbind(lon, lat)))

photocampa_fixedMeses_sat <- as.zoo(t(photocampa_fixedMeses_sat), as.yearmon(idx))
## 2. después extraer el punto en la latitud que buscamos.

## comparacion
 
## añadir cno

fixedcno <- stack("../../proj12abr/fixed_cno_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixedcno <- setZ(fixedcno, idx)
 
## defino el raster del modelo bien:

projection(fixedcno) <- projection(mascara)
extent(fixedcno) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER

fixedMesescno <- zApply(fixedcno, by=as.yearmon, fun='mean')

## EXTRAER AQUI
 
## Para extraer los puntos del modelo necesito un Spatialdata con la lat y la lon primero.
## Ahora proyecto los puntos que quiero extraer a LCC

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

fixedMeses_cno <- extract(fixedMesescno, bsrnlonlat, method="simple")

photocampa_fixedMeses_cno <- as.zoo(t(fixedMeses_cno), as.yearmon(idx))

c <- merge(photocampaMon, photocampa_fixedMeses_sat, photocampa_fixedMeses_caer, photocampa_fixedMeses_cno, all=FALSE)
names(c) <- c("REAL", "SAT","CAER", "CNO")

## elimino el ultimo mes porque los datos son de solo 14 dias.
c2 <- c[-18,]
  
pdf("seriesPhotocampa.pdf")
xyplot(c,scales = list(x = list(at = index(c), rot=45)), superpose=TRUE, type='b')
dev.off()

######### AOD ##########
#######################

aod <- stack("../../AOD_total_monthly20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
aod <- setZ(aod, idx)

## defino el raster de aod bien:

projection(aod) <- projection(mascara)
aod <- crop(aod, mascara)
extent(aod) <- extent(mascara)

aod <- extract(aod, bsrnlonlat, method="simple")
photocampa_aod <- as.zoo(t(aod), as.yearmon(idx))

c <- merge(photocampaMon, photocampa_fixedMeses_sat, photocampa_fixedMeses_caer, photocampa_fixedMeses_cno, photocampa_aod, all=FALSE) 
names(c) <- c("REAL", "SAT","CAER", "CNO", "AOD")

c2 <- c[-18,]
  
pdf("seriesPhotocampaAOD.pdf")
xyplot(c,screens=c(1,1,1,1,2),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
dev.off()

##  con los dias que hay por mes:
 
dias <- p[,1]

c <- merge(photocampaMon, photocampa_fixedMeses_sat, photocampa_fixedMeses_caer, photocampa_fixedMeses_cno, photocampa_aod, dias, all=FALSE) 
names(c) <- c("REAL", "SAT","CAER", "CNO", "AOD", "Days")

pdf("seriesPhotocampaAOD3.pdf")
xyplot(c,screens=c(1,1,1,1,2,3),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
dev.off()

############################################################
## comparación con una simulacion de CAER con los datos del sistema corregidos################

tt <- seq(as.Date('2003-01-01'), as.Date('2009-12-31'), 'month')

xProd <- as.zoo(xProd, order.by=as.yearmon(tt))
photocampa_fixedMeses_caer <- xProd
photocampa_fixedMeses_no <- xProd
photocampa_fixed_sat <- xProd
c <- merge(photocampaMon, photocampa_fixedMeses_caer, photocampa_fixedMeses_no, photocampa_fixed_sat)dias, photocampa_aod, all=FALSE)
c <- c[which(index(c) >= "ene 2003" & index(c) <= "dic 2003")]
names(c) <- c("REAL", "CAER", "CNO", "SAT")
names(c) <- c("REAL", "CAER_SIS", "CAER_GEN", "DAYS", "AOD") 

pdf("seriesPhotocampaaerno.pdf")
xyplot(c,screens=c(1,1,1,1,3),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
dev.off()
