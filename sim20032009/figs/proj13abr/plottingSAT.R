library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)

## load the cmsaf daily data.

## datos del satélite en lat/lon
SIS <- stack("../data/SAT/SISdm20032009_med44.nc", varname='SIS')
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
SIS <- setZ(SIS, idx)

latsis <- init(SIS, v='y')
lonsis <- init(SIS, v='x')

## raster de la máscara tierra/mar. La proyección de esta máscara es LCC.

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

mascara <- raster("masque_terre_mer.nc", varname='zon_new')
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
extent(pmaslonlat)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)


## Una vez que tenemos el raster de la máscara con la extensión y la proyección bien definida, proyectamos el raster dl satélite en lat lon a la nueva proyección.

newproj <- projectExtent(mascara, mycrs)

SISproy <- projectRaster(SIS, newproj)

SISproy <- setZ(SISproy, idx)

## hago la media por años para representar.

year <- function(x) as.numeric(format(x, '%y'))

SISy <- zApply(SISproy, by=year, fun='mean')

## Media del satelite:

SISym <- mean(SISy)
SISym <- mask(SISym, mascara, maskvalue=0)

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

ext <- as.vector(extent(projectExtent(SISym, crs.lonlat)))
#boundaries <- map('worldHires', fill=TRUE, exact=FALSE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)
                                        #boundaries$names
boundaries <- map('worldHires', fill=TRUE, exact=FALSE, plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=mycrs) CRS(projection(SISproy)))

border <- as(SpatialLines, boundaries_sp) ## no funciona

pdf("media_sat_anual.pdf")
## Display the raster
levelplot(SISym) +
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

## MODELO
 
rsds <- stack("../data/C-AER/rsds_day_20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
rsds <- setZ(rsds, idx)

## defino el raster del modelo bien:

rsdslat <- raster("../data/C-AER/rsds_day_20032009.nc", varname='lat')
rsdslon <- raster("../data/C-AER/rsds_day_20032009.nc", varname='lon')

prsdslat <- rasterToPoints(rsdslat)
prsdslon <- rasterToPoints(rsdslon)
rsdslonlat <- cbind(prsdslon[,3], prsdslat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

rsdslonlat
extent(rsdslonlat)

prsdslonlat <- spTransform(rsdslonlat, CRSobj = mycrs)

# Take a look
prsdslonlat
extent(prsdslonlat)
 
extent(rsds) <- extent(prsdslonlat)
## Hago las medias anuales de la simulación C-AER

rsdsy <- zApply(rsds, by=year, fun='mean')
rsdsYm <- mean(rsdsy)

rsdsYm <- mask(rsdsYm, mascara, maskvalue=0)
 
pdf("rsds_caer_yearlyMean_20032009.pdf")
levelplot(rsdsYm) +
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

## DIFERENCIA ANUAL ENTRE EL MODELO Y EL SATÉLITE:

diferencia_caer_sat <- rsdsYm-SISym
diferencia_sat_caer <- SISym -rsdsYm

pdf("diferencia_rsds_caer_sat_yearlyMean_20032009.pdf")
levelplot(diferencia_caer_sat, par.settings=RdBuTheme) +
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

dif_rel_caer_sat <- diferencia_caer_sat/rsdsYm
dif_rel_sat_caer <- diferencia_sat_caer/SISym

pdf("dif_rel_rsds_caer_sat_yearlyMean_20032009.pdf")
levelplot(dif_rel_caer_sat, par.settings=RdBuTheme) +
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

## DIFERENCIA ENTRE SAT Y SIMULACIÓN C-NO

rsdsno <- stack("../data/C-NO/rsds_no_day_20032009.nc")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
rsdsno <- setZ(rsdsno, idx)

## defino el raster del modelo bien:

rsdsnolat <- raster("../data/C-NO/rsds_no_day_20032009.nc", varname='lat')
rsdsnolon <- raster("../data/C-NO/rsds_no_day_20032009.nc", varname='lon')

prsdsnolat <- rasterToPoints(rsdsnolat)
prsdsnolon <- rasterToPoints(rsdsnolon)
rsdsnolonlat <- cbind(prsdsnolon[,3], prsdsnolat[,3])

# Specify the lonlat as spatial points with projection as long/lat
rsdsnolonlat <- SpatialPoints(rsdsnolonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

rsdsnolonlat
extent(rsdsnolonlat)
 
prsdsnolonlat <- spTransform(rsdsnolonlat, CRSobj = mycrs)
 
# Take a look
prsdsnolonlat
extent(prsdsnolonlat)
 
extent(rsdsno) <- extent(prsdsnolonlat)

## Hago las medias anuales de la simulación C-NO

rsdsyno <- zApply(rsdsno, by=year, fun='mean')
rsdsYmno <- mean(rsdsyno)

rsdsYmno <- mask(rsdsYmno, mascara, maskvalue=0)

diferencia_cno_sat <- rsdsYmno-SISym
diferencia_sat_cno <- SISym - rsdsYmno

pdf("diferencia_rsds_cno_sat_yearlyMean_20032009.pdf")
levelplot(diferencia_cno_sat, par.settings=RdBuTheme) +
    ## and the graticule
    + layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.6)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.6))
dev.off()

dif_rel_cno_sat <- diferencia_cno_sat/rsdsYmno
dif_rel_sat_cno <- diferencia_sat_cno/SISym

pdf("dif_rel_rsds_cno_sat_yearlyMean_20032009.pdf")
levelplot(dif_rel_cno_sat, par.settings=RdBuTheme) +
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

## DIFERENCIAS RELATIVAS EN UN MISMO GRÁFICO

s <- stack(diferencia_caer_sat, diferencia_cno_sat)
names(s) <- c("CAER-SAT","CNO-SAT")
s1 <- stack(diferencia_sat_caer, diferencia_sat_cno)
names(s1) <- c("SAT-CAER","SAT-CNO")


## paleta
div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(s1[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(s1[], breaks, rightmost.closed=TRUE)
mids <-tapply(s1[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)


pdf("dif_rel_caer_cno_sat20032009.pdf")
levelplot(s, par.settings=rasterTheme(region=pal)) +
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

pdf("dif_rel_sat_caer_cno_20032009.pdf")
levelplot(s1, par.settings=rasterTheme(region=pal)) +
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

## Las tres medias anuales juntas

yearlyMean <- stack(SISym, rsdsYm, rsdsYmno)
names(yearlyMean) <- c("SAT","C-AER","C-NO")

pdf("rsds_yearly_mean20032009.pdf")
levelplot(yearlyMean) +
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

SISm <- zApply(SISproy, by=month, fun='mean')
names(SISm) <- month.abb
SISm <- mask(SISm, mascara, maskvalue=0)

## ciclo anual de C-AER

rsdsm <- zApply(rsds, by=month, fun='mean')
names(rsdsm) <- month.abb
rsdsm <- mask(rsdsm, mascara, maskvalue=0)

## ciclo anual de C-NO

rsdsnom <- zApply(rsdsno, by=month, fun='mean')
names(rsdsnom) <- month.abb
rsdsnom <- mask(rsdsnom, mascara, maskvalue=0)

## represento la diferencia entre las dos simulaciones y la diferencias entre cada una de ellas y el satélite.

pdf("ciclo_anual_rsds_caer_20032009.pdf")
levelplot(rsdsm) +
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

pdf("ciclo_anual_rsds_cno_20032009.pdf")
levelplot(rsdsnom) +
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

pdf("ciclo_anual_rsds_sat_20032009.pdf")
levelplot(SISm) +
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

## Diferencias de las dos simulaciones con el sat y entre ellas:

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
levelplot(desviacion, par.settings=rasterTheme(region=pal)) +
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
levelplot(desviacion) +
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
levelplot(desviacionrel) +
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
