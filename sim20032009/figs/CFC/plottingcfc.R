library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)

## load the cmsaf cfc monthly data.

## datos del satélite en lat/lon

CFC <- stack("/home/claudia/aod_and_PV/sim20032009/data/CFC/SAT/CFC_monthly20032009.grd")
idx<- seq(as.Date("2004-02-01"), as.Date("2009-12-31"), 'month')
CFC <- setZ(CFC, idx)

latcfc <- raster("/home/claudia/aod_and_PV/sim20032009/data/CFC/SAT/CFC_monthly20032009.grd", varname='lat')
loncfc <- raster("/home/claudia/aod_and_PV/sim20032009/data/CFC/SAT/CFC_monthly20032009.grd", varname='lon')

## raster de la máscara tierra/mar. La proyección de esta máscara es LCC.

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

mascara <- raster("../masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../masque_terre_mer.nc", varname='lat')
maslon <- raster("../masque_terre_mer.nc", varname='lon')

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
extent(pmaslonlat)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)


## Una vez que tenemos el raster de la máscara con la extensión y la proyección bien definida, proyectamos el raster dl satélite en lat lon a la nueva proyección.

newproj <- projectExtent(mascara, mycrs)

CFCproy <- projectRaster(CFC, newproj)
CFCproy <- setZ(CFCproy, idx)

## hago la media por años para representar.

year <- function(x) as.numeric(format(x, '%y'))

CFCy <- zApply(CFCproy, by=year, fun='mean')

## Media del satelite:

CFCym_20042009<- mean(CFCy)

## para representar con graticule.

library(graticule)

lons <- seq(-20, 50, by=10)
lats <- seq(25, 55, by=5)

## optionally, specify the extents of the meridians and parallels
## here we push them out a little on each side
xl <-  range(lons) + c(-0.4, 0.4)
yl <- range(lats) + c(-0.4, 0.4)
## build the lines with our precise locations and ranges
grat <- graticule(lons, lats, proj = mycrs,
                  xlim = xl, ylim = yl)
## Labels
labs <- graticule_labels(lons, lats,
                            xline = lons[2],
                            yline = lats[2],
                            proj = mycrs)

labsLon <- labs[labs$islon,]
labsLat <- labs[!labs$islon,]

## superponer mapa

crs.lonlat <- CRS("+proj=longlat +datum=WGS84")

ext <- as.vector(extent(projectExtent(CFCy, crs.lonlat)))
boundaries <- map('worldHires', fill=TRUE, exact=FALSE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)

IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=crs.lonlat)

boundaries_lcc <- spTransform(boundaries_sp, mycrs)

border <- as(boundaries_lcc, 'SpatialLines')

pdf("CFC_anual.pdf")
## Display the raster
levelplot(CFCy, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

pdf("CFC_anual_mean.pdf")
## Display the raster
levelplot(CFCym, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

## LOW CLOUD COVER

CFC_low <- stack("/home/claudia/aod_and_PV/sim20032009/data/CFC/SAT/CFC_low_monthly20042009.grd")
latcfc<- raster("/home/claudia/aod_and_PV/sim20032009/data/CFC/SAT/CFC_low_monthly20032009.grd", varname='lat')
loncfc <- raster("/home/claudia/aod_and_PV/sim20032009/data/CFC/SAT/CFC_low_monthly20032009.grd", varname='lon')

CFClowproy <- projectRaster(CFC_low, newproj)
CFClowproy <- setZ(CFClowproy, idx)

## hago la media por años para representar.

year <- function(x) as.numeric(format(x, '%y'))

CFClowy <- zApply(CFClowproy, by=year, fun='mean')
CFClowym_20042009<- mean(CFClowy)

pdf("CFC_low_anual.pdf")
## Display the raster
levelplot(CFClowy, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()


pdf("CFC_low_anual_mean.pdf")
## Display the raster
levelplot(CFClowym, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

## MODELO CAER
 
cfc_caer<- stack("../../data/CFC/C-AER/CAER_CFC_day20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
cfc_caer<- setZ(cfc_caer, idx)

## defino el raster del modelo bien:
 
CFCcaerlat <- raster("../../data/CFC/C-AER/CAER_CFC_day20032009.nc", varname='lat')
CFCcaerlon <- raster("../../data/CFC/C-AER/CAER_CFC_day20032009.nc", varname='lon')

plat <- rasterToPoints(CFCcaerlat)
plon <- rasterToPoints(CFCcaerlon)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
cfclonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

cfclonlat
extent(cfclonlat)

pcfclonlat <- spTransform(cfclonlat, CRSobj = mycrs)

# Take a look
pcfclonlat
extent(pcfclonlat)
 
extent(cfc_caer) <- extent(pcfclonlat)

## Representación del modelo CAER

CFC_CAERy <- zApply(cfc_caer, by=year, fun='mean')
CFC_CAERy_20042009 <- subset(CFC_CAERy, 2:7)
CFC_CAERym_20042009<- mean(CFC_CAERy_20042009)

pdf("CFC_caer_anual.pdf")
## Display the raster

levelplot(CFC_CAERy, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

pdf("CFC_caer_anual_mean.pdf")
## Display the raster
levelplot(CFC_CAERym, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

## Diferencia satelite caer

Dif_sat_caer <- CFCym - CFC_CAERy
Dif_caer_sat <- CFC_CAERym_20042009 - CFCym_20042009

## paleta
div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_caer_sat[], na.rm=TRUE)
nInt <- 9

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_caer_sat[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_caer_sat[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
pal[7] <- "#FFFFFF"
    
pdf("CFC_caer_sat_anual_mean.pdf")
## Display the raster
levelplot(Dif_caer_sat, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal), at=breaks, margin=FALSE,) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))
dev.off()

## LOW CFC

CFC_low_caer <- stack("/home/claudia/aod_and_PV/sim20032009/data/CFClow/C-AER/CAER_CFClow_20032009.nc")
latcfc<- raster("/home/claudia/aod_and_PV/sim20032009/data/CFClow/C-AER/CAER_CFClow_20032009.nc", varname='lat') 
loncfc <- raster("/home/claudia/aod_and_PV/sim20032009/data/CFClow/C-AER/CAER_CFClow_20032009.nc", varname='lon') 

plat <- rasterToPoints(latcfc)
plon <- rasterToPoints(loncfc)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
cfclonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
extent(cfclonlat)

pcfclonlat <- spTransform(cfclonlat, CRSobj = mycrs)
extent(CFC_low_caer) <- extent(pcfclonlat)

idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
CFC_low_caer <- setZ(CFC_low_caer, idx)

## hago la media por años para representar.

year <- function(x) as.numeric(format(x, '%y'))

CFClowy_caer <- zApply(CFC_low_caer, by=year, fun='mean')
CFClowy_caer20042009 <- subset(CFClowy_caer, 2:7)
CFClowym_caer20042009 <- mean(CFClowy_caer20042009)

pdf("CFC_low_caer_anual_mean.pdf")
## Display the raster
levelplot(CFClowym, scales=list(draw=FALSE)) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))
dev.off()

## Diferencias con el satélite
 
Dif_low_cfc_caer_sat <- CFClowym_caer20042009-CFClowym_20042009

## paleta
div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_low_cfc_caer_sat[], na.rm=TRUE)
nInt <- 9

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_low_cfc_caer_sat[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_low_cfc_caer_sat[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("CFC_low_cfc_caer_sat_anual_mean2.pdf")
## Display the raster
levelplot(Dif_low_cfc_caer_sat, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal),at=breaks, margin=FALSE) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))
dev.off()

## MODELO CNO
 
cfc_cno<- stack("../../data/CFC/C-NO/CNO_CFC_day20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
cfc_cno<- setZ(cfc_cno, idx)

## defino el raster del modelo bien:

CFCcnolat <- raster("../../data/CFC/C-NO/CNO_CFC_day20032009.nc", varname='lat')
CFCcnolon <- raster("../../data/CFC/C-NO/CNO_CFC_day20032009.nc", varname='lon')

plat <- rasterToPoints(CFCcnolat)
plonlon <- rasterToPoints(CFCcnolon)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
cfclonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

cfclonlat
extent(cfclonlat)

pcfclonlat <- spTransform(cfclonlat, CRSobj = mycrs)

# Take a look
pcfclonlat
extent(pcfclonlat)
 
extent(cfc_cno) <- extent(pcfclonlat)

## Representación del modelo CNO

CFC_CNOy <- zApply(cfc_cno, by=year, fun='mean')
CFC_CNOym <- mean(CFC_CNOy)

pdf("CFC_cno_anual.pdf")
## Display the raster
levelplot(CFC_CNOy, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

pdf("CFC_cno_anual_mean.pdf")
## Display the raster
levelplot(CFC_CNOym, scales=list(draw=FALSE)) + layer(sp.lines(border))+
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
dev.off()

## Diferencias entre sat y cno

Dif_sat_cno <- CFCym-CFC_CNOym

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_sat_cno[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_sat_cno[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_sat_cno[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("CFC_sat_cno_anual_mean.pdf")
## Display the raster
levelplot(Dif_sat_cno, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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
dev.off()

## represento las dos diferencias con el satelite juntas:

s <- stack(Dif_sat_caer, Dif_sat_cno)
names(s) <- c("SAT.CAER", "SAT.CNO")


div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(s[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(s[], breaks, rightmost.closed=TRUE)
mids <-tapply(s[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("CFC_sat_caer_cno_anual_mean.pdf")
## Display the raster
levelplot(s, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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
dev.off()

## Diferencias entre simulaciones

Dif_sims <- CFC_CAERym -CFC_CNOym

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_sims[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_sims[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_sims[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("CFC_caer_cno_anual_mean_2.pdf")
## Display the raster
levelplot(Dif_sims, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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
dev.off()

## CICLO ANUAL

library(zoo)
month <- function(x) as.numeric(format(x, '%m'))

CFCm <- zApply(CFCproy, by=month, fun='mean')
names(CFCm) <- month.abb

CFCm_CAER <- zApply(cfc_caer, by=month, fun='mean')
names(CFCm_CAER) <- month.abb

CFCm_CNO <- zApply(cfc_cno, by=month, fun='mean')
names(CFCm_CAER) <- month.abb

CFCm_low<- zApply(CFClowproy, by=month, fun='mean')
names(CFCm_low) <- month.abb

CFCm_low_CAER <- zApply(CFC_low_caer, by=month, fun='mean')
names(CFCm_low_CAER) <- month.abb


## Diferencia SAT CAER

dif_CFC_caer_sat_Mon<- CFCm_CAER-CFCm

div.pal <- brewer.pal(n=13, 'RdBu')

rng <- range(dif_CFC_caer_sat_Mon[], na.rm=TRUE)


inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(dif_CFC_caer_sat_Mon[], breaks, rightmost.closed=TRUE)
mids <-tapply(dif_CFC_caer_sat_Mon[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
pal[5] <- "#FFFFFF"

pdf("CFC_caer_sat_cicloanual.pdf")
## Display the raster
levelplot(dif_CFC_caer_sat_Mon, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal), at=breaks) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))
dev.off()

## caer-sat low ciclo anual

dif_CFC_low_caer_sat_Mon<- CFCm_low_CAER-CFCm_low

div.pal <- brewer.pal(n=9, 'RdBu')

rng <- range(dif_CFC_low_caer_sat_Mon[], na.rm=TRUE)


inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(dif_CFC_low_caer_sat_Mon[], breaks, rightmost.closed=TRUE)
mids <-tapply(dif_CFC_low_caer_sat_Mon[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
pal[4] <- "#FFFFFF"

pdf("CFC_low_caer_sat_cicloanual.pdf")
## Display the raster
levelplot(dif_CFC_low_caer_sat_Mon, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal), at=breaks) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))
dev.off()



## Diferencia SAT CNO

dif_CFC_cno_Mon<- CFCm-CFCm_CNO

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(dif_CFC_cno_Mon[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(dif_CFC_cno_Mon[], breaks, rightmost.closed=TRUE)
mids <-tapply(dif_CFC_cno_Mon[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("CFC_cno_cicloanual.pdf")
## Display the raster
levelplot(dif_CFC_cno_Mon, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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
dev.off()


## DIF C-AER-SAT/DIF C-NO-SAT

## represento la diferencia relativa entre las simulaciones y el satélite tomando de referencia el satélite:

rel_dif_cicloAnual_sat_caer<- (SISm - rsdsm)/SISm

## paleta
div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(rel_dif_cicloAnual_sat_caer[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(rel_dif_cicloAnual_sat_caer[], breaks, rightmost.closed=TRUE)
mids <-tapply(rel_dif_cicloAnual_sat_caer[], idxx,median)
mx <- max(abs(breaks))
 
break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)


pdf("rel_dif_cicloAnual_sat_caer.pdf")
levelplot(rel_dif_cicloAnual_sat_caer, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## Diferencia relativa con la simulacion cno

rel_dif_cicloAnual_sat_cno<- (SISm - rsdsnom)/SISm

rng <- range(rel_dif_cicloAnual_sat_cno[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(rel_dif_cicloAnual_sat_cno[], breaks, rightmost.closed=TRUE)
mids <-tapply(rel_dif_cicloAnual_sat_cno[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)


pdf("rel_dif_cicloAnual_sat_cno.pdf")
levelplot(rel_dif_cicloAnual_sat_cno, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## los los ciclos anuales a la vez:

s <- stack(rel_dif_cicloAnual_sat_caer, rel_dif_cicloAnual_sat_cno)

rng <- range(s[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(s[], breaks, rightmost.closed=TRUE)
mids <-tapply(s[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("rel_dif_cicloAnual_sat_caer_cno.pdf")
levelplot(s, par.settings=rasterTheme(region=pal)) +
    layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## elimino valores por debajo de -1

s[s[] < -0.8] <- -0.8
pdf("rel_dif_cicloAnual_sat_caer_cnoFiltered.pdf")
levelplot(s, par.settings=rasterTheme(region=pal)) +
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## FILTRO TAMBIÉN LAS COMPARACIONES POR SEPARADO:

rel_dif_cicloAnual_sat_cno[rel_dif_cicloAnual_sat_cno[] < -1] <- -1 
pdf("rel_dif_cicloAnual_sat_cnoFiltered.pdf")
levelplot(rel_dif_cicloAnual_sat_cno, par.settings=rasterTheme(region=pal)) +
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

rel_dif_cicloAnual_sat_caer[rel_dif_cicloAnual_sat_caer[] < -1] <- -1 
pdf("rel_dif_cicloAnual_sat_caerFiltered.pdf")
levelplot(rel_dif_cicloAnual_sat_caer, par.settings=rasterTheme(region=pal)) +
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## Voy a calcular el bias de cada una de las simulaciones en el ciclo anual:

biasRsds <- SISm- rsdsm
biasRsdsno <- SISm - rsdsnom

desviacion <- biasRsds -biasRsdsno

rng <- range(desviacion[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(desviacion[], breaks, rightmost.closed=TRUE)
mids <-tapply(desviacion[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("desviacion_CicloAnual_sat_caer_cno_20032009.pdf")
levelplot(desviacion, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

pdf("desviacion_CicloAnual_sat_caer_cno_20032009default.pdf")
levelplot(desviacion) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


desviacionrel <- (biasRsds -biasRsdsno)/SISm

rng <- range(desviacionrel[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(desviacionrel[], breaks, rightmost.closed=TRUE)
mids <-tapply(desviacionrel[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("desviacionrel_CicloAnual_sat_caer_cno_20032009default.pdf")
levelplot(desviacionrel) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


## TODOS LOS MESES PARA HACER LA MEDIA ESTACIONAL:

## satelite

month <- function(x) as.numeric(format(x, '%m'))

SISmm <- zApply(SISproy, by=as.yearmon, fun='mean') #84 valores
SISmm <- mask(SISmm, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
SISmm <- setZ(SISmm, idx)
 
monthIndex <- function(k){
    c <- c(1:7)
    j <- k
    for (i in 1:7){
        c[i] <- j
        j <- j+12}
    return(c)
}

lista <- list()
for (i in 1:12) lista[[i]] <- monthIndex(i)

## lista con un rasterBrick por mes
Meses <- lapply(lista, FUN=function(x) subset(SISmm, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJF <- stack(enero, febrero, diciembre)
DJF <- mean(DJF)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]

MAM <- stack(marzo, abril, mayo)
MAM <- mean(MAM)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJA <- stack(junio, julio, agosto)
JJA <- mean(JJA)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SON <- stack(septiembre, octubre, diciembre)
SON <- mean(SON)

## meses modelo

month <- function(x) as.numeric(format(x, '%m'))

rsdsmm <- zApply(rsds, by=as.yearmon, fun='mean') #84 valores
rsdsmm <- mask(rsdsmm, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
rsdsmm <- setZ(rsdsmm, idx)

Mesesrsds <- lapply(lista, FUN=function(x) subset(rsdsmm, x))

eneroCAER <- Mesesrsds[[1]]
febreroCAER <- Mesesrsds[[2]]
diciembreCAER <- Mesesrsds[[12]]

DJFCAER <- stack(eneroCAER, febreroCAER, diciembreCAER)
DJFCAER <- mean(DJFCAER)

marzoCAER <- Mesesrsds[[3]]
abrilCAER <- Mesesrsds[[4]]
mayoCAER <-Mesesrsds[[5]]

MAMCAER <- stack(marzoCAER, abrilCAER, mayoCAER)
MAMCAER <- mean(MAMCAER)

junioCAER <- Mesesrsds[[6]]
julioCAER <- Mesesrsds[[7]]
agostoCAER <- Mesesrsds[[8]]

JJACAER <- stack(junioCAER, julioCAER, agostoCAER)
JJACAER <- mean(JJACAER)

septiembreCAER <- Mesesrsds[[9]]
octubreCAER <- Mesesrsds[[10]]
noviembreCAER <- Mesesrsds[[11]]

SONCAER <- stack(septiembreCAER, octubreCAER, diciembreCAER)
SONCAER <- mean(SONCAER)

## meses modelo c-no

month <- function(x) as.numeric(format(x, '%m'))

rsdsmmno <- zApply(rsdsno, by=as.yearmon, fun='mean') #84 valores
rsdsmmno <- mask(rsdsmmno, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
rsdsmmno <- setZ(rsdsmmno, idx)

Mesesrsdsno <- lapply(lista, FUN=function(x) subset(rsdsmmno, x))

eneroCNO <- Mesesrsdsno[[1]]
febreroCNO <- Mesesrsdsno[[2]]
diciembreCNO <- Mesesrsdsno[[12]]

DJFCNO <- stack(eneroCNO, febreroCNO, diciembreCNO)
DJFCNO <- mean(DJFCNO)

marzoCNO <- Mesesrsdsno[[3]]
abrilCNO <- Mesesrsdsno[[4]]
mayoCNO <-Mesesrsdsno[[5]]

MAMCNO <- stack(marzoCNO, abrilCNO, mayoCNO)
MAMCNO <- mean(MAMCNO)

junioCNO <- Mesesrsdsno[[6]]
julioCNO <- Mesesrsdsno[[7]]
agostoCNO <- Mesesrsdsno[[8]]

JJACNO <- stack(junioCNO, julioCNO, agostoCNO)
JJACNO <- mean(JJACNO)

septiembreCNO <- Mesesrsdsno[[9]]
octubreCNO <- Mesesrsdsno[[10]]
noviembreCNO <- Mesesrsdsno[[11]]

SONCNO <- stack(septiembreCNO, octubreCNO, diciembreCNO)
SONCNO <- mean(SONCNO)

## meses de verano JJA diferencia SAT-CAER 

JJAdif_sat_caer <- JJA-JJACAER
JJAdif_rel_sat_caer <- JJAdif_sat_caer/JJA

## JJA SAT/C-NO

JJAdif_sat_cno <- JJA-JJACNO
JJAdif_rel_sat_cno <- JJAdif_sat_cno/JJA

## Paleta sat/cno

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(JJAdif_rel_sat_cno[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(JJAdif_rel_sat_cno[], breaks, rightmost.closed=TRUE)
mids <-tapply(JJAdif_rel_sat_cno[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("JJAdif_rel_sat_cno.pdf")
levelplot(JJAdif_rel_sat_cno, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


## invierno DFJ:

DJFdif_sat_caer <- DJF-DJFCAER
DJFdif_rel_sat_caer <- DJFdif_sat_caer/DJF

## Paleta

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(DJFdif_rel_sat_caer[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(DJFdif_rel_sat_caer[], breaks, rightmost.closed=TRUE)
mids <-tapply(DJFdif_rel_sat_caer[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("DJFdif_rel_sat_caer.pdf")
levelplot(DJFdif_rel_sat_caer, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## invierno DFJ sat/cno:

DJFdif_sat_cno <- DJF-DJFCNO
DJFdif_rel_sat_cno <- DJFdif_sat_cno/DJF

## Paleta

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(DJFdif_rel_sat_cno[], na.rm=TRUE)
nInt <- 13
 
inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(DJFdif_rel_sat_cno[], breaks, rightmost.closed=TRUE)
mids <-tapply(DJFdif_rel_sat_cno[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("DJFdif_rel_sat_cno.pdf")
levelplot(DJFdif_rel_sat_cno, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## meses de MAM diferencia SAT-CAER 

MAMdif_sat_caer <- MAM-MAMCAER
MAMdif_rel_sat_caer <- MAMdif_sat_caer/MAM

## Paleta

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(MAMdif_rel_sat_caer[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(MAMdif_rel_sat_caer[], breaks, rightmost.closed=TRUE)
mids <-tapply(MAMdif_rel_sat_caer[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("MAMdif_rel_sat_caer.pdf")
levelplot(MAMdif_rel_sat_caer, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## meses de MAM diferencia SAT-CNO 

MAMdif_sat_cno <- MAM-MAMCNO
MAMdif_rel_sat_cno <- MAMdif_sat_cno/MAM

## Paleta

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(MAMdif_rel_sat_cno[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(MAMdif_rel_sat_cno[], breaks, rightmost.closed=TRUE)
mids <-tapply(MAMdif_rel_sat_cno[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("MAMdif_rel_sat_cno.pdf")
levelplot(MAMdif_rel_sat_cno, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


## OTOÑO SON SAT-CAER:

SONdif_sat_caer <- SON-SONCAER
SONdif_rel_sat_caer <- SONdif_sat_caer/SON

## Paleta

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(SONdif_rel_sat_caer[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(SONdif_rel_sat_caer[], breaks, rightmost.closed=TRUE)
mids <-tapply(SONdif_rel_sat_caer[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
 
pdf("SONdif_rel_sat_caer.pdf")
levelplot(SONdif_rel_sat_caer, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


## OTOÑO SON SAT-CNO:

SONdif_sat_cno <- SON-SONCNO
SONdif_rel_sat_cno <- SONdif_sat_cno/SON

## Paleta

div.pal <- brewer.pal(n=11, 'RdBu')
rng <- range(SONdif_rel_sat_cno[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(SONdif_rel_sat_cno[], breaks, rightmost.closed=TRUE)
mids <-tapply(SONdif_rel_sat_cno[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)
 
pdf("SONdif_rel_sat_cno.pdf")
levelplot(SONdif_rel_sat_cno, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


## ciclo anual de C-AER

rsdsm <- zApply(rsds, by=month, fun='mean')
names(rsdsm) <- month.abb
rsdsm <- mask(rsdsm, mascara, maskvalue=0)

## ciclo anual de C-NO

rsdsnom <- zApply(rsdsno, by=month, fun='mean')
names(rsdsnom) <- month.abb
rsdsnom <- mask(rsdsnom, mascara, maskvalue=0)
 
