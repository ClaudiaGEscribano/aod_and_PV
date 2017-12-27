## Este script prepara los datos para poder ser representados de manera espacial y para las series temporales.

## Es necesario extraer la información de latitud/longitud y asignar una proyección espacial.

## La proyección de las simulaciones es la misma que la de la máscara tierra-mar. En este script estraigo los datos de este nc y los asigno al .nc o raster de la simulación de producción PV.

library(raster)
library(ncdf4)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)

data('worldMapEnv') ## Incluye fronteras para los países posteriores 1991

#########################################################
##   ASIGNO LA PROYECCION A LA MASCARA
#########################################################

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

mascara <- raster("../masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../masque_terre_mer.nc", varname='lat')
maslon <- raster("../masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
maslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)

########################################################

## función que aplicaré al output de producción de los modelos

setProj <- function(data){
    projection(data) <- projection(mascara)
    extent(data) <- extent(mascara)
    data <- mask(data, mascara, maskvalue=0)
    return(data)
    }

## El satélite hay que proyectarlo porque originalmente está en latlon

setProjsat <- function(data){
    newproj <- projectExtent(mascara, mycrs)
    data <- projectRaster(data, newproj)
    data <- mask(data, mascara, maskvalue=0)
    return(data)
    }
