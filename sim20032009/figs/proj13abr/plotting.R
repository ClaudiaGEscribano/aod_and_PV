library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)

data(worldMapEnv)
## FIXED YEARLY##

## load PVoutput files 

fixedY <- stack("../../calc/proj12abr/fixed_caer_yearlyProd_temp_20032009.grd") ## C-AER
fixedYno <- stack("../../calc/proj12abr/fixed_cno_yearlyProd_temp_20032009.grd")
fixedYsat <- stack("../../calc/proj12abr/fixed_sat_yearlyProd_temp_20032009.grd")

## mask

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

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

##
projection(fixedY) <- projection(mascara)
extent(fixedY) <- extent(mascara)
fixedY <-  mask(fixedY, mascara, maskvalue=0) ## hace la mascara sobre fixedY filtrando los valores con 0.

projection(fixedYno) <- projection(mascara)
extent(fixedYno) <- extent(mascara)
fixedYno <- mask(fixedYno, mascara, maskvalue=0)

## EL SATÉLITE HAY QUE PROYECTARLO:

newproj <- projectExtent(mascara, mycrs)
fixedYsat <- projectRaster(fixedYsat, newproj)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
fixedYsat <- setZ(fixedYsat, idx)

## superponer mapa

crs.lonlat <- CRS("+proj=longlat +datum=WGS84")

ext <- as.vector(extent(projectExtent(fixedYsat, crs.lonlat)))
boundaries <- map('world', fill=TRUE, exact=FALSE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)

IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=crs.lonlat)

boundaries_lcc <- spTransform(boundaries_sp, mycrs)

border <- as(boundaries_lcc, 'SpatialLines') 

## lat lon lines
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

## media anual:

fixedYmean <- mean(fixedY)
fixedYnomean <- mean(fixedYno)
fixedYsatmean <- mean(fixedYsat)

fixedYsatmean <- mask(fixedYsatmean, mask=mascara, maskvalue=0)

s <- stack(fixedYmean, fixedYnomean, fixedYsatmean)
names(s) <- c("FIXED.CAER", "FIXED.CNO", "FIXED.SAT")
 
pdf("caer_cno_sat_fixed_yearlyProd_20032009.pdf")
levelplot(s, scales=list(draw=FALSE))+ layer(sp.lines(border, lwd=0.5))+
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

## Diferencia relativa entre las dos simulaciones:

Dif_fixedY <- (fixedYmean-fixedYnomean)/fixedYnomean

## paleta de diferencias relativas:

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_fixedY[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_fixedY[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_fixedY[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("Dif_rel_caer_cno_fixed_yearlyProd_20032009.pdf")
levelplot(Dif_fixedY)+ layer(sp.lines(border))+
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

## ONE AXIS YEARLY ##

oneY <- stack("../../calc/proj12abr/oneAxis_caer_yearlyProd_temp_20032009.grd") ## C-AER
oneYno <- stack("../../calc/proj12abr/oneAxis_cno_yearlyProd_temp_20032009.grd")
oneYsat <- stack("../../calc/proj12abr/oneAxis_sat_yearlyProd_temp_20032009.grd")

projection(oneY) <- projection(mascara)
extent(oneY) <- extent(mascara)
oneY <-  mask(oneY, mascara, maskvalue=0) ## hace la mascara sobre fixedY filtrando los valores con 0.

projection(oneYno) <- projection(mascara)
extent(oneYno) <- extent(mascara)
oneYno <- mask(oneYno, mascara, maskvalue=0)
 
## SAT
oneYsat <- projectRaster(oneYsat, newproj)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
oneYsat <- setZ(oneYsat, idx)
oneYsat <- mask(oneYsat, mascara, maskvalue=0)

oneYmean <- mean(oneY)
oneYnomean <- mean(oneYno)
oneYsatmean <- mean(oneYsat)

s <- stack(oneYmean, oneYnomean, oneYsatmean)
names(s) <- c("ONE.CAER", "ONE.CNO", "ONE.SAT")

pdf("caer_cno_sat_one_yearlyProd_20032009.pdf")
levelplot(s, scales=list(draw=FALSE))+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.3)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## Diferencia relativa entre las dos simulaciones:

Dif_oneY <- (oneYmean-oneYnomean)/oneYnomean

pdf("Dif_rel_caer_cno_one_yearlyProd_20032009.pdf")
levelplot(Dif_oneY)+ layer(sp.lines(border))+
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

## TWO AXES YEARLY ##

twoY <- stack("../../calc/proj12abr/twoAxes_caer_yearlyProd_temp_20032009.grd") ## C-AER
twoYno <- stack("../../calc/proj12abr/twoAxes_cno_yearlyProd_temp_20032009.grd")
twoYsat <- stack("../../calc/proj12abr/twoAxes_sat_yearlyProd_temp_20032009.grd")

projection(twoY) <- projection(mascara)
extent(twoY) <- extent(mascara)
twoY <-  mask(twoY, mascara, maskvalue=0) ## hace la mascara sobre fixedY filtrando los valores con 0.

projection(twoYno) <- projection(mascara)
extent(twoYno) <- extent(mascara)
twoYno <- mask(twoYno, mascara, maskvalue=0)

## sat
twoYsat <- projectRaster(twoYsat, newproj)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'year')
twoYsat <- setZ(twoYsat, idx)
twoYsat <- mask(twoYsat, mascara, maskvalue=0)

twoYmean <- mean(twoY)
twoYnomean <- mean(twoYno)
twoYsatmean <- mean(twoYsat)

s <- stack(twoYmean, twoYnomean, twoYsatmean) 
names(s) <- c("TWO.CAER", "TWO.CNO", "TWO.SAT")

pdf("caer_cno_sat_two_yearlyProd_20032009.pdf")
levelplot(s, scales=list(draw=FALSE))+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.3)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## Diferencia relativa entre las dos simulaciones:

Dif_twoY <- (twoYmean-twoYnomean)/twoYnomean

pdf("Dif_rel_caer_cno_two_yearlyProd_20032009.pdf")
levelplot(Dif_twoY)+ layer(sp.lines(border))+
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

## Diferencias relativas los 3 seguidores:

s <-  stack(Dif_fixedY, Dif_oneY, Dif_twoY)
names(s) <- c("FIXED.CAER.CNO", "ONE.CAER.CNO", "TWO.CAER.CNO")

pdf("Dif_rel_caer_cno_all_yearlyProd_20032009.pdf")
levelplot(s, scales=list(draw=FALSE))+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## Diferencias entre seguidores:

dif_fixed <- fixedYmean-fixedYnomean
dif_one <- oneYmean-oneYnomean
dif_two <- twoYmean-twoYnomean

s <- stack(dif_fixed, dif_one, dif_two)
names(s) <- c("FIXED.CAER.CNO", "ONE.CAER.CNO","TWO.CAER.CNO")

pdf("Dif_caer_cno_all_yearlyProd_20032009.pdf")
levelplot(s)+ layer(sp.lines(border))+
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


## CICLO ANUAL.

## Represento la diferencia en el ciclo anual entre las dos simulaciones para seguidor fijo.

fixedM <- stack("../../calc/proj12abr/fixed_caer_monthlyProd_temp_20032009.grd") ## C-AER
fixedMno <- stack("../../calc/proj12abr/fixed_cno_monthlyProd_temp_20032009.grd")

projection(fixedM) <- projection(mascara)
extent(fixedM) <- extent(mascara)
fixedM <-  mask(fixedM, mascara, maskvalue=0) ## hace la mascara sobre fixedY filtrando los valores con 0.

projection(fixedMno) <- projection(mascara)
extent(fixedMno) <- extent(mascara)
fixedMno <-  mask(fixedMno, mascara, maskvalue=0) ## hace la masc


idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixedM <- setZ(fixedM, idx)
fixedMno <- setZ(fixedMno, idx)

library(zoo)
month <- function(x) as.numeric(format(x, '%m'))

fixedM <- zApply(fixedM, by=month, fun='mean')
names(fixedM) <- month.abb
fixedM <- mask(fixedM, mascara, maskvalue=0)

fixedMno <- zApply(fixedMno, by=month, fun='mean')
names(fixedMno) <- month.abb
fixedMno <- mask(fixedMno, mascara, maskvalue=0)

pdf("ciclo_anual_fixed_caer_20032009.pdf")
levelplot(fixedM) + layer(sp.lines(border))+
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

pdf("ciclo_anual_fixed_cno_20032009.pdf")
levelplot(fixedMno) + layer(sp.lines(border))+
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

## diferencia relativa ciclo anual entre las dos sim:

Dif_rel <- (fixedM-fixedMno)/fixedMno

## paleta

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_rel[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_rel[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_rel[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("dif_rel_ciclo_anual_fixed_caer_cno_20032009.pdf")
levelplot(Dif_rel, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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

## diferencias en el ciclo anual para los demas seguidores:

oneM <- stack("../../calc/proj12abr/oneAxis_caer_monthlyProd_temp_20032009.grd") ## C-AER
oneMno <- stack("../../calc/proj12abr/oneAxis_cno_monthlyProd_temp_20032009.grd")

projection(oneM) <- projection(mascara)
extent(oneM) <- extent(mascara)
oneM <-  mask(oneM, mascara, maskvalue=0) ## hace la mascara sobre fixedY filtrando los valores con 0.

projection(oneMno) <- projection(mascara)
extent(oneMno) <- extent(mascara)
oneMno <-  mask(oneMno, mascara, maskvalue=0) ## hace la masc

idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
oneM <- setZ(oneM, idx)
oneMno <- setZ(oneMno, idx)

library(zoo)
month <- function(x) as.numeric(format(x, '%m'))

oneM <- zApply(oneM, by=month, fun='mean')
names(oneM) <- month.abb
oneM <- mask(oneM, mascara, maskvalue=0)

oneMno <- zApply(oneMno, by=month, fun='mean')
names(oneMno) <- month.abb
oneMno <- mask(oneMno, mascara, maskvalue=0)

## diferencia relativa ciclo anual entre las dos sim:

Dif_rel_one<- (oneM-oneMno)/oneMno

## paleta

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_rel_one[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_rel_one[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_rel_one[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("dif_rel_ciclo_anual_oneAxis_caer_cno_20032009.pdf")
levelplot(Dif_rel_one, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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

## diferencias en el ciclo anual para los demas seguidores:

twoM <- stack("../../calc/proj12abr/twoAxes_caer_monthlyProd_temp_20032009.grd") ## C-AER
twoMno <- stack("../../calc/proj12abr/twoAxes_cno_monthlyProd_temp_20032009.grd")
 
projection(twoM) <- projection(mascara)
extent(twoM) <- extent(mascara)
twoM <-  mask(twoM, mascara, maskvalue=0) ## hace la mascara sobre fixedY filtrando los valores con 0.

projection(twoMno) <- projection(mascara)
extent(twoMno) <- extent(mascara)
twoMno <-  mask(twoMno, mascara, maskvalue=0) ## hace la masc

idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twoM <- setZ(twoM, idx)
twoMno <- setZ(twoMno, idx)

library(zoo)
month <- function(x) as.numeric(format(x, '%m'))

twoM <- zApply(twoM, by=month, fun='mean')
names(twoM) <- month.abb
twoM <- mask(twoM, mascara, maskvalue=0)

twoMno <- zApply(twoMno, by=month, fun='mean')
names(oneMno) <- month.abb
twoMno <- mask(twoMno, mascara, maskvalue=0)

## diferencia relativa ciclo anual entre las dos sim:

Dif_rel_two<- (twoM-twoMno)/twoMno

## paleta

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(Dif_rel_two[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(Dif_rel_two[], breaks, rightmost.closed=TRUE)
mids <-tapply(Dif_rel_two[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("dif_rel_ciclo_anual_twoAxes_caer_cno_20032009.pdf")
levelplot(Dif_rel_one, par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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

## SEASONAL DIFFERENCES:

## FIXED ##

month <- function(x) as.numeric(format(x, '%m'))

fixedmm <- zApply(fixedM, by=as.yearmon, fun='mean') #84 valores
fixedmm <- mask(fixedmm, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixedmm <- setZ(fixedmm, idx)
 
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
Meses <- lapply(lista, FUN=function(x) subset(fixedmm, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJFfixedcaer <- stack(enero, febrero, diciembre)
DJFfixedcaer <- mean(DJFfixedcaer)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]
 
MAMfixedcaer <- stack(marzo, abril, mayo)
MAMfixedcaer <- mean(MAMfixedcaer)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJAfixedcaer <- stack(junio, julio, agosto)
JJAfixedcaer <- mean(JJAfixedcaer)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SONfixedcaer <- stack(septiembre, octubre, diciembre)
SONfixedcaer <- mean(SONfixedcaer)

## cno

fixedmmNo <- zApply(fixedMno, by=as.yearmon, fun='mean') #84 valores
fixedmmNo <- mask(fixedmmNo, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixedmmNo <- setZ(fixedmmNo, idx)
 
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
Meses <- lapply(lista, FUN=function(x) subset(fixedmmNo, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJFfixedcno <- stack(enero, febrero, diciembre)
DJFfixedcno <- mean(DJFfixedcno)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]

MAMfixedcno <- stack(marzo, abril, mayo)
MAMfixedcno <- mean(MAMfixedcno)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJAfixedcno <- stack(junio, julio, agosto)
JJAfixedcno <- mean(JJAfixedcno)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SONfixedcno <- stack(septiembre, octubre, diciembre)
SONfixedcno <- mean(SONfixedcno)

## Diifferences in productivity FIXED

## DJF

DJF_fixed_rel_dif <- (DJFfixedcaer-DJFfixedcno)/DJFfixedcno

## paleta

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(DJF_fixed_rel_dif[], na.rm=TRUE)
nInt <- 9

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(DJF_fixed_rel_dif[], breaks, rightmost.closed=TRUE)
mids <-tapply(DJF_fixed_rel_dif[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("dif_rel_DJF_fixed_caer_cno_20032009.pdf")
levelplot(DJF_fixed_rel_dif) + layer(sp.lines(border))+
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

## JJA

JJA_fixed_rel_dif <- (JJAfixedcaer-JJAfixedcno)/JJAfixedcno

pdf("dif_rel_JJA_fixed_caer_cno_20032009.pdf")
levelplot(JJA_fixed_rel_dif) + layer(sp.lines(border))+
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

## MAM

MAM_fixed_rel_dif <- (MAMfixedcaer-MAMfixedcno)/MAMfixedcno

pdf("dif_rel_MAM_fixed_caer_cno_20032009.pdf")
levelplot(MAM_fixed_rel_dif) + layer(sp.lines(border))+
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

## SON

SON_fixed_rel_dif <- (SONfixedcaer-SONfixedcno)/SONfixedcno

pdf("dif_rel_SON_fixed_caer_cno_20032009.pdf")
levelplot(SON_fixed_rel_dif) + layer(sp.lines(border))+
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

## figura con las 4 estaciones para fixed.

s <- stack(DJF_fixed_rel_dif, MAM_fixed_rel_dif, JJA_fixed_rel_dif, SON_fixed_rel_dif)
names(s) <- c("DFJ", "MAM", "JJA","SON")

my.at <- seq(0, -0.35, by=-0.05)

pdf("dif_rel_SEASON_fixed_caer_cno_20032009.pdf")
levelplot(s, layout=c(2,2), at=my.at, scales=list(draw=FALSE)) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## ONE

month <- function(x) as.numeric(format(x, '%m'))

onemm <- zApply(oneM, by=as.yearmon, fun='mean') #84 valores
onemm <- mask(onemm, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
onemm <- setZ(onemm, idx)
 
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
Meses <- lapply(lista, FUN=function(x) subset(onemm, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJFonecaer <- stack(enero, febrero, diciembre)
DJFonecaer <- mean(DJFonecaer)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]
 
MAMonecaer <- stack(marzo, abril, mayo)
MAMonecaer <- mean(MAMonecaer)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJAonecaer <- stack(junio, julio, agosto)
JJAonecaer <- mean(JJAonecaer)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SONonecaer <- stack(septiembre, octubre, diciembre)
SONonecaer <- mean(SONonecaer)

## cno one

onemmno <- zApply(oneMno, by=as.yearmon, fun='mean') #84 valores
onemmno <- mask(onemmno, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
onemmno <- setZ(onemmno, idx)
 
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
Meses <- lapply(lista, FUN=function(x) subset(onemmno, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJFonecno <- stack(enero, febrero, diciembre)
DJFonecno <- mean(DJFonecno)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]
 
MAMonecno<- stack(marzo, abril, mayo)
MAMonecno <- mean(MAMonecno)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJAonecno <- stack(junio, julio, agosto)
JJAonecno <- mean(JJAonecno)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SONonecno <- stack(septiembre, octubre, diciembre)
SONonecno <- mean(SONonecno)

## ONE axis todas las estaciones

s <- stack(DJF_one_rel_dif, MAM_one_rel_dif, JJA_one_rel_dif, SON_one_rel_dif)
names(s) <- c("DJF", "MAM", "JJA","SON")

my.at <- seq(0, -0.35, by=-0.05)

pdf("dif_rel_SEASON_one_caer_cno_20032009.pdf")
levelplot(s, layout=c(2,2), at=my.at, scales=list(draw=FALSE)) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

## DJF

DJF_one_rel_dif <- (DJFonecaer-DJFonecno)/DJFonecno

pdf("dif_rel_DJF_one_caer_cno_20032009.pdf")
levelplot(DJF_one_rel_dif) + layer(sp.lines(border))+
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

## JJA

JJA_one_rel_dif <- (JJAonecaer-JJAonecno)/JJAonecno

pdf("dif_rel_JJA_one_caer_cno_20032009.pdf")
levelplot(JJA_one_rel_dif) + layer(sp.lines(border))+
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

## MAM

MAM_one_rel_dif <- (MAMonecaer-MAMonecno)/MAMonecno

pdf("dif_rel_MAM_one_caer_cno_20032009.pdf")
levelplot(MAM_one_rel_dif) + layer(sp.lines(border))+
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

## SON

SON_one_rel_dif <- (SONonecaer-SONonecno)/SONonecno

pdf("dif_rel_SON_one_caer_cno_20032009.pdf")
levelplot(SON_one_rel_dif) + layer(sp.lines(border))+
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

## TWO ##

##caer

month <- function(x) as.numeric(format(x, '%m'))

twomm <- zApply(twoM, by=as.yearmon, fun='mean') #84 valores
twomm <- mask(twomm, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twomm <- setZ(twomm, idx)
 
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
Meses <- lapply(lista, FUN=function(x) subset(twomm, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJFtwocaer <- stack(enero, febrero, diciembre)
DJFtwocaer <- mean(DJFtwocaer)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]
 
MAMtwocaer <- stack(marzo, abril, mayo)
MAMtwocaer <- mean(MAMtwocaer)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJAtwocaer <- stack(junio, julio, agosto)
JJAtwocaer <- mean(JJAtwocaer)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SONtwocaer <- stack(septiembre, octubre, diciembre)
SONtwocaer <- mean(SONtwocaer)

## cno

twommno <- zApply(twoMno, by=as.yearmon, fun='mean') #84 valores
twommno <- mask(twommno, mascara, maskvalue=0)
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
twommno <- setZ(twommno, idx)
 
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
Meses <- lapply(lista, FUN=function(x) subset(twommno, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJFtwocno <- stack(enero, febrero, diciembre)
DJFtwocno <- mean(DJFtwocno)

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]
 
MAMtwocno <- stack(marzo, abril, mayo)
MAMtwocno <- mean(MAMtwocno)

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJAtwocno <- stack(junio, julio, agosto)
JJAtwocno <- mean(JJAtwocno)

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SONtwocno <- stack(septiembre, octubre, diciembre)
SONtwocno <- mean(SONtwocno)

## DJF

DJF_two_rel_dif <- (DJFtwocaer-DJFtwocno)/DJFtwocno
 
pdf("dif_rel_DJF_two_caer_cno_20032009.pdf")
levelplot(DJF_two_rel_dif) + layer(sp.lines(border))+
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

## JJA

JJA_two_rel_dif <- (JJAtwocaer-JJAtwocno)/JJAtwocno
 
pdf("dif_rel_JJA_two_caer_cno_20032009.pdf")
levelplot(JJA_two_rel_dif) + layer(sp.lines(border))+
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

## MAM

MAM_two_rel_dif <- (MAMtwocaer-MAMtwocno)/MAMtwocno
 
pdf("dif_rel_MAM_two_caer_cno_20032009.pdf")
levelplot(MAM_two_rel_dif) + layer(sp.lines(border))+
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

## SON

SON_two_rel_dif <- (SONtwocaer-SONtwocno)/SONtwocno
 
pdf("dif_rel_SON_two_caer_cno_20032009.pdf")
levelplot(SON_two_rel_dif) + layer(sp.lines(border))+
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

## TWO todas las estaciones juntas

s <- stack(DJF_two_rel_dif, MAM_two_rel_dif, JJA_two_rel_dif, SON_two_rel_dif)
names(s) <- c("DJF", "MAM", "JJA","SON")

my.at <- seq(0, -0.35, by=-0.05)

pdf("dif_rel_SEASON_two_caer_cno_20032009.pdf")
levelplot(s, layout=c(2,2), at=my.at, scales=list(draw=FALSE)) + layer(sp.lines(border, lwd=0.5))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()

