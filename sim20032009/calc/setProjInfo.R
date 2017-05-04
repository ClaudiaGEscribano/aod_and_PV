## This script put the projection info into the nc file in order to use the raster library.

inputfile <- "/home/claudia/data/"

# Grab the lat and lon from the data
lat <- raster(inputfile, varname="lat")
lon <- raster(inputfile, varname="lon")

# Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])

## Define the LCC projection
mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

## declare that projection into the latlon data
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

## Need the rgdal package to project it to the original coordinate system
library("rgdal")

plonlat <- spTransform(lonlat, CRSobj = mycrs)
# Take a look
plonlat
extent(plonlat)

# Now we can properly set the coordinate information for the raster
pr <- raster(inputfile, varname="aero")
# Fix the projection and extent
projection(pr) <- mycrs
extent(pr) <- extent(plonlat) 
# Take a look
pr
plot(pr)

#####################################################################
## He representado los datos en LCC. Si quiero representar en lat/lon tengo q darle a los puntos lat lon esa proyeccion:

lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
# Now we can properly set the coordinate information for the raster
pr <- raster(inputfile, varname="aero")

r <- projectRaster(pr, crs=CRS("+proj=longlat +datum=WGS84"))
