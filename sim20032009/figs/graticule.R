library(rasterVis)
library(graticule)
library(rgdal)


## Only needed for downloading a raster example
##library(meteoForecast)

##today <- Sys.Date()
##testDay <- today - 7
 
## Retrieve raster data
##r <- getRaster('temp', day = testDay, frames = 1)

################################################################
## I need projected data for the example. Not lat lon

## datos del modelo original:

sismod <- stack("../data/C-AER/rsds_day_20032009.nc")
mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
projection(sismod) <- mycrs

## desde los datos originales de latitud y longitud, los asigno proyección lat lon y luego defino la otra proyeccion:
lat <- raster("../data/C-AER/rsds_day_20032009.nc", varname="lat")
lon <- raster("../data/C-AER/rsds_day_20032009.nc", varname="lon")

# Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

plonlat <- spTransform(lonlat, CRSobj = mycrs)
# Take a look
plonlat
extent(plonlat)

projection(sismod) <- mycrs
extent(sismod) <- extent(plonlat)

## comprobar la extension de sismod

projectExtent(sismod, crs.lonlat)


## Here is where the graticule routine starts
#crs.longlat <- CRS("+init=epsg:4326")
#prj <- CRS(projection(r))

prj <- mycrs
extLL <- projectExtent(SIS, mycrs) ## no sirve para un raster que ya tiene una proyección LCC porque su extensión no contiene la latitud y la longitud.

##lons <- pretty(c(xmin(extLL), xmax(extLL)))
##lats <- pretty(c(ymin(extLL), ymax(extLL)))

## creo las longitudes y las latitudes pero no a partir del raster:

lons <- seq(-15, 45, by=10)
lats <- seq(30, 50, by=5)

## optionally, specify the extents of the meridians and parallels
## here we push them out a little on each side
xl <-  range(lons) + c(-0.4, 0.4)
yl <- range(lats) + c(-0.4, 0.4)
## build the lines with our precise locations and ranges
grat <- graticule(lons, lats, proj = prj,
                  xlim = xl, ylim = yl)
## Labels
labs <- graticule_labels(lons, lats,
                            xline = lons[2],
                            yline = lats[2],
                            proj = prj)

labsLon <- labs[labs$islon,]
labsLat <- labs[!labs$islon,]


## Display the raster
levelplot(sismod, layers=1) +
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))

## haciendo la transformación de lat/lon primero para obtener la extensión y después asignar la proyección al raster consigo que las dos capas, malla y raster estén proyectadas igual.

## pruebo con la mascara:

mascara <- raster("masque_terre_mer.nc", varname=zon_new)
maslat <- raster("masque_terre_mer.nc", varname='lat')
maslon <- raster("masque_terre_mer.nc", varname='lon')

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
extent(plonlat)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)


## hago la mascara de los datos. Primero necesito recortar a las dimensiones de la malla

## ? sirve con extent. diria que sí pq ya están en la misma proyeccion.

extent(sismod) <- extent(mascara)
sis <- mask(sismod, mascara, maskvalue=0)

levelplot(sis, layers=1) +
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))

## ahora falta que sea un poco mejor la malla que se pone encima.

lons <- seq(-15, 45, by=10)
lats <- seq(30, 55, by=5)

## optionally, specify the extents of the meridians and parallels
## here we push them out a little on each side
xl <-  range(lons) + c(-0.5, 0.5)
yl <- range(lats) + c(-0.5, 0.5)
## build the lines with our precise locations and ranges
grat <- graticule(lons, lats, proj = prj,
                  xlim = xl, ylim = yl)
## Labels
labs <- graticule_labels(lons, lats,
                            xline = lons[2],
                            yline = lats[2],
                            proj = prj)

labsLon <- labs[labs$islon,]
labsLat <- labs[!labs$islon,]

## los valores de x e y no son necesarios


## datos de satélite interpolados a la malla del modelo

sat  <- stack("../data/SAT/remap_SISdm20032009_med44.nc", varname='SIS')
satlat <- raster("../data/SAT/remap_SISdm20032009_med44.nc", varname='lat')
satlon <- raster("../data/SAT/remap_SISdm20032009_med44.nc", varname='lon')
mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
projection(sat) <- mycrs
 
crs.lonlat <- CRS("+proj=longlat +datum=WGS84")


psatlat <- rasterToPoints(satlat)
psatlon <- rasterToPoints(satlon)
satlonlat <- cbind(plon[,3], plat[,3])

lonlat <- SpatialPoints(satlonlat, proj4string = crs.lonlat)
plonlat <- spTransform(lonlat, CRSobj = mycrs)
                                        # Take a look
plonlat
extent(plonlat)

projection(sat) <- mycrs
extent(sismod) <- extent(plonlat)
