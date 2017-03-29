library(raster)
library(rasterVis)
library(rgdal)

load("/home/claudia/variabilidad/linea.Rdata")

########################################################
## 3. TWO
######################################################

fixedMonthly <- "../calc&fixed_monthlyProd_temp_20032009.grd"
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
lonlat <- SpatialPoints(lonlat, proj4string = mycrs) 
plonlat <- spTransform(lonlat, CRSobj = mycrs)
extent(plonlat)

pr <- stack(inputfile, varname="rsds")
# Fix the projection and extent
projection(pr) <- mycrs
extent(pr) <- extent(plonlat) 
# Take a look
pr
plot(pr)




 

