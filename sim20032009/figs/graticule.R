library(rasterVis)
library(graticule)


## Only needed for downloading a raster example
library(meteoForecast)

today <- Sys.Date()
testDay <- today - 7

## Retrieve raster data
r <- getRaster('temp', day = testDay, frames = 1)

## I need projected data for the example. Not lat lon

SIS <- stack("../data/SAT/remap_SISdm20032009_med44.nc", varname='SIS') 
mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
projection(SIS) <- mycrs

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

## grat está bien creado, y puedo visualizarlos con plo(grat).

## el problema es que no consigo representarlo con levelplot. 

## Display the raster
levelplot(SIS, layers=1) +
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

## solo veo la capa principal.
