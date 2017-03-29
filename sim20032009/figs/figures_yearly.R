## figuras para mostrar la productividad y la radiación en el plano del generador.

library(raster)
library(rasterVis)
library(rgdal)

########################################################################################
## 1. FIXED 
####################################################################################

#load("/home/claudia/variabilidad/linea.Rdata")
## the lon lat data has to be retrieved from the older files

inputfile <- "/home/claudia/MeteoFrance/aod_and_PV/sim20032009/data/fixed_yearlyProd_temp_20032009.grd"
inputfile2 <- "/home/claudia/MeteoFrance/aod_clusters/macc_regcm_2003_01_bc.nc"

# Grab the lat and lon from the data
lat <- raster(inputfile2, varname="lat")
lon <- raster(inputfile2, varname="lon")

# Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])

## Define the LCC projection
mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

## declare that projection into the latlon data
lonlat <- SpatialPoints(lonlat, proj4string = mycrs) 
plonlat <- spTransform(lonlat, CRSobj = mycrs)

#take a look
plonlat
extent(plonlat)

# Now we can properly set the coordinate information for the raster
pr <- stack(inputfile)
# Fix the projection and extent
projection(pr) <- mycrs
extent(pr) <- extent(plonlat) 
# Take a look
pr
plot(pr)












idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), "year")
fixed30mean <- setZ(fixed30mean, idx)

## xyplot de la anomalía de produccion con el tiempo

mediaDomain30 <- cellStats(fixed30mean, mean)

anomaliesFixed <- mediaDomain30-mean(mediaDomain30)
anomaliesFixed <- as.zoo(anomaliesFixed)

pdf("anomaliesFixed.pdf")
xyplot(anomaliesFixed)
dev.off()

anomaliesFixed_rel <- anomaliesFixed/mean(mediaDomain30)
anomaliesFixed_rel <- anomaliesFixed_rel*100
anomaliesFixed_rel <- as.zoo(anomaliesFixed_rel)

pdf("anomaliesFixed_rel.pdf")
xyplot(anomaliesFixed_rel)
dev.off()
 
## mapas de produccion anual media

fixedMean <- mean(fixed30mean)

pdf("fixed_30y_mean.pdf")
levelplot(fixedMean, margin=FALSE) + layer(sp.lines(linea))
dev.off()

pdf("fixed_30y_mean_contour.pdf")
levelplot(fixedMean, margin=FALSE, contour=TRUE) + layer(sp.lines(linea))
dev.off()


##################################
## 2. 1-AXIS
#######################################################

oneAxis30mean <- stack("/home/claudia/productividad/conTemperatura/YearlyProductivity30_horiz_temp2.gri")

idx <- seq(as.Date("1983-01-01"), as.Date("2013-12-31"), "year")
oneAxis30mean <- setZ(oneAxis30mean, idx)

## xyplot de la anomalía de produccion con el tiempo

mediaDomain30oneAxis <- cellStats(oneAxis30mean, mean)

anomaliesOneAxis <- mediaDomain30oneAxis-mean(mediaDomain30oneAxis)
anomaliesOneAxis <- as.zoo(anomaliesOneAxis)

pdf("anomaliesOneAxis.pdf")
xyplot(anomaliesOneAxis)
dev.off()

anomaliesOneAxis_rel <- anomaliesOneAxis/mean(mediaDomain30oneAxis)
anomaliesOneAxis_rel <- anomaliesOneAxis_rel*100
anomaliesOneAxis_rel <- as.zoo(anomaliesOneAxis_rel)

pdf("anomaliesOneAxis_rel.pdf")
xyplot(anomaliesOneAxis_rel)
dev.off()
 
## mapas de produccion anual media

OneAxisMean <- mean(oneAxis30mean)

pdf("oneAxis_30y_mean.pdf")
levelplot(OneAxisMean, margin=FALSE) + layer(sp.lines(linea))
dev.off()

pdf("oneAxis_30y_mean_contour.pdf")
levelplot(OneAxisMean, margin=FALSE, contour=TRUE) + layer(sp.lines(linea))
dev.off()

##################################
## 3. 2-AXES
#######################################################

twoAxes30mean <- stack("/home/claudia/productividad/conTemperatura/YearlyProductivity30_two_temp2.gri")

idx <- seq(as.Date("1983-01-01"), as.Date("2013-12-31"), "year")
twoAxes30mean <- setZ(twoAxes30mean, idx)

## xyplot de la anomalía de produccion con el tiempo

mediaDomain30twoAxes <- cellStats(twoAxes30mean, mean)

anomaliesTwoAxes <- mediaDomain30twoAxes-mean(mediaDomain30twoAxes)
anomaliesTwoAxes <- as.zoo(anomaliesTwoAxes)

pdf("anomaliesTwoAxes.pdf")
xyplot(anomaliesTwoAxes)
dev.off()

anomaliesTwoAxes_rel <- anomaliesTwoAxes/mean(mediaDomain30twoAxes)
anomaliesTwoAxes_rel <- anomaliesTwoAxes_rel*100
anomaliesTwoAxes_rel <- as.zoo(anomaliesTwoAxes_rel)

pdf("anomaliesTwoAxes_rel.pdf")
xyplot(anomaliesTwoAxes_rel)
dev.off()

## mapas de produccion anual media

TwoAxesMean <- mean(twoAxes30mean)

pdf("twoAxes_30y_mean.pdf")
levelplot(TwoAxesMean, margin=FALSE) + layer(sp.lines(linea))
dev.off()

pdf("twoAxes_30y_mean_contour.pdf")
levelplot(TwoAxesMean, margin=FALSE, contour=TRUE) + layer(sp.lines(linea))
dev.off()

################################################
## diferencias entre seguidores
################################################

pdf("fixed_one.pdf")
levelplot(fixedMean-OneAxisMean, margin=FALSE, contour=TRUE)+ layer(sp.lines(linea))
dev.off()

pdf("fixed_two.pdf")
levelplot(fixedMean-TwoAxesMean, margin=FALSE, contour=TRUE)+ layer(sp.lines(linea))
dev.off()

pdf("one_two.pdf")
levelplot(OneAxisMean-TwoAxesMean, margin=FALSE, contour=TRUE)+ layer(sp.lines(linea))
dev.off()

#################################################
## IRRADIATION
#################################################

G030mean <- stack("/home/claudia/variabilidad/media_by_year_resolucion.gri")

idx <- seq(as.Date("1983-01-01"), as.Date("2013-12-31"), "year")
G030mean <- setZ(G030mean, idx)

## xyplot de la anomalía de produccion con el tiempo

mediaDomain30G0 <- cellStats(G030mean, mean)

anomaliesG0 <- mediaDomain30G0-mean(mediaDomain30G0)
anomaliesG0 <- as.zoo(anomaliesG0)

pdf("anomaliesG0.pdf")
xyplot(anomaliesG0)
dev.off()

anomaliesG0_rel <- anomaliesG0/mean(mediaDomain30G0)
anomaliesG0_rel <- anomaliesG0_rel*100
anomaliesG0_rel <- as.zoo(anomaliesG0_rel)

pdf("anomaliesG0_rel.pdf")
xyplot(anomaliesG0_rel)
dev.off()

## mapas de produccion anual media

G0Mean <- mean(G030mean) ## son W/M2

pdf("G0Mean_30y_mean_WM2.pdf")
levelplot(G0Mean, margin=FALSE) + layer(sp.lines(linea))
dev.off()

pdf("G0Mean_30y_mean_contourWM2.pdf")
levelplot(G0Mean, margin=FALSE, contour=TRUE) + layer(sp.lines(linea))
dev.off()

## Give the maps in energy:

## Yearly irradiation:

G0Mean_yearlyIrradiation <- G0Mean*8760/1000

pdf("G0Mean_30y_kWhm2.pdf")
levelplot(G0Mean_yearlyIrradiation, margin=FALSE) + layer(sp.lines(linea))
dev.off()

###################################################
## diferencia con la produccion de G0
########################################

pdf("G0_fixed.pdf")
levelplot(G0Mean_yearlyIrradiation-fixedMean, margin=FALSE) + layer(sp.lines(linea))
dev.off()

pdf("G0_OneAxis.pdf")
levelplot(G0Mean_yearlyIrradiation-OneAxisMean, margin=FALSE) + layer(sp.lines(linea))
dev.off()

pdf("G0_TwoAxis.pdf")
levelplot(G0Mean_yearlyIrradiation-TwoAxesMean, margin=FALSE) + layer(sp.lines(linea))
dev.off()
