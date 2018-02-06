## Datos de radiacion de satelite y modelos para ver las diferencias por zonas

library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(reshape2)

## raster de la máscara de regiones. Asigno proyección.

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

zonas <- raster("zones_ald_MAD50.nc", varname='zon_new')
zonaslat <- raster("zones_ald_MAD50.nc", varname='lat')
zonaslon <- raster("zones_ald_MAD50.nc", varname='lon')

pzonaslat <- rasterToPoints(zonaslat)
pzonaslon <- rasterToPoints(zonaslon)
zonaslonlat <- cbind(pzonaslon[,3], pzonaslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
zonaslonlat <- SpatialPoints(zonaslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

zonaslonlat
extent(zonaslonlat)
 
pzonaslonlat <- spTransform(zonaslonlat, CRSobj = mycrs)
                                        # Take a look
pzonaslonlat
extent(pzonaslonlat)

projection(zonas) <- mycrs
extent(zonas) <- extent(pzonaslonlat)

## mascara land-sea para filtrar:

mascara <- raster("../../figs/masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../../figs/masque_terre_mer.nc", varname='lat')
maslon <- raster("../../figs/masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])

mascaralonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

pmascaralonlat <- spTransform(mascaralonlat, CRSobj = mycrs)
                                        # Take a look
projection(mascara) <- mycrs
extent(mascara) <- extent(pmascaralonlat)

## filtro las zonas con la tierra mar

zonas_land <- mask(zonas, mascara, maskvalue=0)

writeRaster(zonas_land, filename='zonas_land.grd')

## el problema de esta mascara, es que los valores que tiene cada una de las zonas no es un valor entero. Se lo asignamos para que sea más facil hacer los cálculos por zonas

zonasdf <- as.data.frame(zonas_land)

table(zonasdf)
dftable <- as.data.frame(table(zonasdf))
dftable$id <-c(1,2,3,4,5,6,7,8,9)

## tendre que cambiar en el dataframe los valores actuales por una seq del 1 al 9. Una vez hecho, creo un raster a partir del anterior y le doy estos valores.

table(signif(zonasdf))

zonas1 <- signif(zonasdf)
zonas1[zonas1[] == 3.99997] <- 4 
zonas1[zonas1[] == 4] <- 1
zonas1[zonas1[] == 4.3] <- 2
zonas1[zonas1[] == 4.7] <- 3
zonas1[zonas1[] == 5.3] <- 4
zonas1[zonas1[] == 5] <- 5
zonas1[zonas1[] == 6] <- 6
zonas1[zonas1[] == 6.3] <- 7
zonas1[zonas1[] == 6.6] <- 8

zonasv <- as.vector(zonas1)
zonas <- raster(zonas_land)
zonas <- setValues(zonas, values=zonas1[,1])

writeRaster(zonas, filename='zonas.grd', overwrite=TRUE)
##represento el mapa de zonas

my.at <- seq(from=0, to=8, by=1)
myPal <- brewer.pal(8, 'Set2')
myTheme <- rasterTheme(region = myPal, alpha=0.7)
 
data('worldMapEnv')

## si cargo border es la definición antigua.

load("/home/claudia/aod_and_PV/sim20032009/calc/border_aod.Rdata")

pdf("zonasNew5.pdf", height=4, width=5)
levelplot(zonas,margin=FALSE, scales=list(draw=FALSE), par.settings=myTheme, at=my.at)+ layer(sp.lines(border_aod, lwd=0.5))
dev.off()

## insert points:

## tengo que crear un objeto spatialpoints con las coordenadas:

lat <- c(44.08, 46.81, 30.86)
lon <- c(5.06, 6.94, 34.78)

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

pdf("zonasPuntos2.pdf", height=4, width=5)
levelplot(zonas,margin=FALSE, scales=list(draw=FALSE), par.settings=myTheme, at=my.at)+ layer(sp.lines(border_aod, lwd=0.5))+ layer(sp.points(bsrnlonlat, cex=0.5, pch=19, col='red'))
dev.off()

## inserto también la localización de las plantas.

lat <- c(37.7,41.1)
lon <- c(-5.7,1.19)

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
plantslonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
plantslonlat <- spTransform(plantslonlat, mycrs)

pdf("zonasPuntos3.pdf", height=4, width=5)
levelplot(zonas,margin=FALSE, scales=list(draw=FALSE), par.settings=myTheme, at=my.at)+ layer(sp.lines(border_aod, lwd=0.5))+ layer(sp.points(bsrnlonlat, cex=0.5, pch=19, col='red'))+layer(sp.points(plantslonlat, cex=0.5, lwd=1, col='red'))
dev.off()

## categorical data

z <- ratify(zonas)

rat <- levels(z)[[1]]
rat$zonas <- c('AFRW', 'AFRE', 'EMED', 'EURS', 'EURW','EURC','EURNE','BISL') 
levels(z) <- rat

pdf("zonasPuntos4.pdf", height=3, width=5)
levelplot(z ,margin=FALSE, scales=list(draw=FALSE), par.settings=myTheme, at=my.at)+
    layer(sp.lines(border_aod, lwd=0.5))+
    layer(sp.points(bsrnlonlat, cex=0.5, pch=19, col='blue'))+ layer(sp.points(plantslonlat, cex=0.5, lwd=1, col='blue'))
dev.off()

pdf("zonasPuntosLabel.pdf", height=3, width=5)
levelplot(z ,margin=FALSE, scales=list(draw=FALSE), par.settings=myTheme, at=my.at)+
    layer(sp.lines(border, lwd=0.3))+
    layer(sp.points(bsrnlonlat, cex=0.5, pch=19, col='brown1'))+ layer(sp.pointLabel(bsrnlonlat, labels=c("Carpentras", "Payerne","Sde Boker"), cex=0.5, pos='1')) + layer(sp.points(plantslonlat, cex=0.5, lwd=1, col='brown1'))+ layer(sp.pointLabel(plantslonlat, labels=c("Seville", "Tarragona"), cex=0.5, pos='1'))
dev.off()




####
b <- seq(min(getValues(zonas), na.rm=TRUE), max(getValues(zonas), na.rm=TRUE),1)
 
