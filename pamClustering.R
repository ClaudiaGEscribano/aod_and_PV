## This script do the clustering with the pam algorithm.

library(raster)
library(rasterVis)

wheredata <- "/home/claudia/GITHUB/aod_and_PV/data/"
wherecalc <- "/home/claudia/GITHUB/aod_and_PV"

setwd(wheredata)
load("or_features.Rdata")
load("ss_features.Rdata")
load("bc_features.Rdata")
load("su_features.Rdata")
load("sd_features.Rdata")

setwd(wherecalc)

## trying the kmeans algorithm first:
 
set.seed(10)

## hc clustering para inicializar el kmeans




kmeansexp <- function(x, n, k){
    km <- lapply(seq(1:k),
                 FUN=function(i) kmeans(x, i, nstart=n, iter.max=1000))
    return(km)
}

a <- kmeansexp(su_features, 100, 50)


## SELECT SOME PARTITIONS TO PUT THEM ON A RASTER

## Supongo la particion optima la de 20 rasters

mync <- raster("/home/claudia/aerosoles_DATA/climatologia_AOD/AOD/bc/macc_regcm_2003_01_bc.nc")

cluster20 <- raster(mync)
cluster20 <- setValues(cluster20, a[[50]]$cluster)

pdf("clusters20.pdf")
levelplot(cluster20)
dev.off()

## trying to project it with lat lon sense
 
inputfile <- "/home/claudia/aerosoles_DATA/climatologia_AOD/AOD/bc/macc_regcm_2003_01_bc.nc"

## Grab the lat and lon from the data
lat <- raster(inputfile, varname="lat")
lon <- raster(inputfile, varname="lon")

## Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])

## Specify the lonlat as spatial points with projection as long/lat
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

## Need the rgdal package to project it to the original coordinate system
library("rgdal")

## My best guess at the proj4 string from the information given
mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")
plonlat <- spTransform(lonlat, CRSobj = mycrs)
## Take a look
plonlat
extent(plonlat)

projection(cluster20) <- mycrs
extent(cluster20) <- extent(plonlat)
## Take a look

su <- projectRaster(cluster20, crs=CRS("+proj=longlat +datum=WGS84"))

pdf("clusters50LCC.pdf")
levelplot(su)
dev.off()




## la informacion de los clusters hay que llevarla a un raster para representarla espacialmente.
library(raster)
library(rasterVis)

setwd(wheredata)
r <- raster("macc_regcm_2003_01_bc.nc")
setwd(wherecalc)
P <- setValues(r, claraClusters)

pdf("clustersprueba.pdf")
levelplot(P)
dev.off()



## library of clustering

library(cluster)

pamx <- pam(or_features, k=20, diss=FALSE)

features <- cbind(or_features, ss_features)

clarax <- clara(or_features, 20)

claraClusters <- clarax$clustering

