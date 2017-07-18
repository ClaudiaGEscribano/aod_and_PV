## Datos de radiacion de satelite y modelos para ver las diferencias por zonas

library(raster)

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

my.at <- seq(1:8)
myPal <- brewer.pal(9, 'Set1')
myTheme <- rasterTheme(region = myPal)
 
pdf("zonasmejoradas2.pdf", height=4, width=5)
levelplot(zonas,margin=FALSE, scales=list(draw=FALSE), par.settings=myTheme)
dev.off()
