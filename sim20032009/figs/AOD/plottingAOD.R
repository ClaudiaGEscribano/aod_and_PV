library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(mapdata)
library(rgdal)

data(worldMapEnv)
## load the AOD monthly data.

## datos del satélite en lat/lon

AOD <- stack("../../calc/aod/AOD_total_monthly20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
AOD <- setZ(AOD, idx)
AOD_ciclo <- stack("../../calc/aod/AOD_total_ciclo20032009.grd")
AOD_anual <- stack("../../calc/aod/AOD_total_yearly20032009.grd")

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

pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)

## para representar con graticule.

library(graticule)

lons <- seq(-30, 70, by=10)
lats <- seq(20, 80, by=5)

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

ext <- as.vector(extent(projectExtent(AOD, crs.lonlat)))
boundaries <- map('world', fill=TRUE, exact=FALSE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)

IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=crs.lonlat)

boundaries_lcc <- spTransform(boundaries_sp, mycrs)

border <- as(boundaries_lcc, 'SpatialLines') ## no funciona

## MEDIA ANUAL AOD
 
load("../../calc/border_aod.Rdata")
extent(AOD_anual) <- extent(mascara)

my.at <- seq(0, 0.6, 0.05)
div.pal <- brewer.pal(n=9, 'RdYlGn')
pal <- rev(div.pal)
 
pdf("media_AOD_anualPRUEBA.pdf")
## Display the raster
levelplot(AOD_anual, at=my.at, scales=list(draw=FALSE), par.settings=rasterTheme(region=pal)) + layer(sp.lines(border))+
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

## CICLO ANUAL AOD

extent(AOD_ciclo) <- extent(mascara)

my.at <- seq(0, 0.6, 0.05)
div.pal <- brewer.pal(n=9, 'YlGnBu')
pal <- rev(div.pal)

pdf("media_AOD_cicloanual3.pdf")
## Display the raster
levelplot(AOD_ciclo, at=my.at, scales=list(draw=FALSE), par.settings=rasterTheme(region=div.pal)) + layer(sp.lines(border))+
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

## ESTACIONAL AOD

DJF <- stack("../../calc/aod/DJF_aod.grd")
MAM <- stack("../../calc/aod/MAM_aod.grd")
JJA <- stack("../../calc/aod/JJA_aod.grd")
SON <- stack("../../calc/aod/SON_aod.grd")

seasonal <- stack(DJF, MAM, JJA, SON)
names(seasonal) <- c("DJF", "MAM", "JJA", "SON")
  
my.at <- seq(0, 0.6, 0.05)
div.pal <- brewer.pal(n=7, 'YlGnBu')
#pal <- rev(div.pal)
#pal[1] <- "#FFFFFF"

seasonal[seasonal[]>=0.6] <-0.6

pdf("seasonalAOD.pdf", width=7, height=5)

levelplot(seasonal, at=my.at,scales=list(draw=FALSE), par.settings=rasterTheme(region=div.pal)) + layer(sp.lines(border, lwd=0.2))+
    ## and the graticule
    layer(sp.lines(grat, lwd=0.2)) +
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
levelplot(rsdsYm) + layer(sp.lines(border))+
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
    layer(sp.lines(border))+
    ## and the graticule
    layer(sp.lines(grat)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.4)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.4))
dev.off()

dif_rel_caer_sat <- diferencia_caer_sat/rsdsYm
dif_rel_sat_caer <- diferencia_sat_caer/SISym

pdf("dif_rel_rsds_caer_sat_yearlyMean_20032009.pdf")
levelplot(dif_rel_caer_sat, par.settings=RdBuTheme) +
    layer(sp.lines(border))+
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
    layer(sp.lines(border))
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
    layer(sp.lines(border))+
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
s1 <- stack(dif_rel_sat_caer, dif_rel_sat_cno)
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


pdf("dif_rel_sat_caer_cno_20032009.pdf")
levelplot(s1, par.settings=rasterTheme(region=pal)) +
    layer(sp.lines(border))+
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

## diferencia anual entre las dos simulaciones

rel_dif_sims <- (rsdsYm-rsdsYmno)/rsdsYm

## paleta centrada en 0

div.pal <- brewer.pal(n=11, 'RdBu')

rng <- range(rel_dif_sims[], na.rm=TRUE)
nInt <- 13

inc0 <- diff(rng)/nInt
n0 <- floor(abs(rng[1])/inc0)
inc <- abs(rng[1])/(n0+1/2)
n1 <- ceiling((rng[2]/inc-1/2)+1)
breaks <- seq(rng[1],by=inc,length=n0+1+n1)

idxx <- findInterval(rel_dif_sims[], breaks, rightmost.closed=TRUE)
mids <-tapply(rel_dif_sims[], idxx,median)
mx <- max(abs(breaks))

break2pal <- function(x,mx,pal){
    y <- 1/2*(x/mx+1)
    rgb(pal(y), maxColorValue=255)
}

divRamp <-colorRamp(div.pal)
pal <- break2pal(mids, mx, divRamp)

pdf("rel_dif_caer_cno_yearly_mean200320092.pdf")
levelplot(rel_dif_sims) + layer(sp.lines(border))+
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

## diferencia simulaciones W/m2

dif_sims <- (rsdsYm-rsdsYmno)

pdf("dif_caer_cno_yearly_mean200320092.pdf")
levelplot(dif_sims) + layer(sp.lines(border)) +
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
levelplot(yearlyMean) + layer(sp.lines(border))+
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
levelplot(rsdsm) + layer(sp.lines(border))+
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
levelplot(rsdsnom) + layer(sp.lines(border))+
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
levelplot(SISm) + layer(sp.lines(border))+
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

dif_simulaciones <- rsdsm-rsdsnom
 
pdf("ciclo_anual_dif__simulaciones_20032009.pdf")
levelplot(dif_simulaciones) + layer(sp.lines(border))+
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
