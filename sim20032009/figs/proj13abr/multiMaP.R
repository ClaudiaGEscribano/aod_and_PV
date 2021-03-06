## This script create a multivariate choropleth map with a quantitative variable, the difference between 2 simulations and a qualitative variable: the significance of that difference.

source('projectInfo.R')
source('graticule.R')
source('border.R')

## Data use for the map:

## 1. FIXED PANELS SIMULATIONS.

fixedAER <- stack("../../calc/proj12abr/outputTciclo/fixed_caer_yearlyProd_tday_20032009.grd")
fixedNO <- stack("../../calc/proj12abr/outputTciclo/fixed_cno_yearlyProd_tday_20032009.grd")

## set the projection to the raster

fixedAER <- setProj(fixedAER)
fixedNO <- setProj(fixedNO)

## 2. significance test code:

stackWilc <- function(s1, s2) { 
  mycor <- function(v) { 
    x <- v[1:split] 
    y <- v[(split+1):(2*split)]
    if (is.na(v) == TRUE) { v <-  NA}
    else {
    t.test(x, y, exact=TRUE)$p.value
  }}
  s <- stack(s1, s2) 
  split <- nlayers(s)/2
  calc(s, fun=mycor) 
}

pvalue <- stackWilc(fixedAER, fixedNO)
 
## 3. rastersatck of the differences (quantitative variable)

fixedAERm <- mean(fixedAER, na.rm=TRUE)
fixedNOm <- mean(fixedNO, na.tm=TRUE)

diffixed <- fixedAERm-fixedNOm

reldiffixed <-(fixedAERm-fixedNOm)/fixedNOm
reldiffixed <- reldiffixed*100

## 4. Defining classes of the qualitative variable
 
a <- which(pvalue[] >= 0.05)
 
classes <- levels(factor(pvalue[] >= 0.05))
nClases <- length(classes)

breaks <- c(0, 0.05, max(pvalue[],na.rm=TRUE))
pvalueclass <- cut(pvalue, breaks)
pvalueclass <- ratify(pvalueclass)

classes <- levels(pvalueclass)[[1]]
classes$type <- c("SIG", "NO-SIG") 
levels(pvalueclass) <- classes

## 5. Create a stack with the 2 rasters

s <- stack(diffixed, pvalueclass)
names(s) <- c('dif', 'sig')
    
library(colorspace)

classes <- classes$type
nClasses <- length(classes)

pList <- lapply(1:nClasses, function(i){
    sig <- pvalueclass
    sig[!(pvalueclass ==i)] <- NA

    dif <- mask(diffixed, sig)
    
    pClass <- levelplot(dif, contour=TRUE, scales=list(draw=FALSE), margin=FALSE, alpha.regions=(if (i ==1) TRUE else 0.3))+layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.3)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
})

p <- Reduce('+', pList)
 
## 6. Creo una malla con los datos de significancia y superpongo.

sig <- pvalueclass
sig[sig[]==1] <- NA ## me quedo con las celdas no significativas

a <- SpatialPoints(coordinates(sig)[!is.na(values(sig)),])
 
levelplot(reldiffixed, alpha.regions=0.8)+layer(sp.points(a, pch=3, cex=0.2, alpha=1))
 
## ejemplo: esto es para añadir una capa solo sobre uno de los rasters del stack.

levelplot(s, margin=FALSE, at=seq(0, 1, 0.05)) + 
  layer(sp.points(xy2, pch=3, cex=2, col=1), columns=1) +
    layer(sp.points(xy2, pch=2, cex=2, col=1), columns=2)

https://stackoverflow.com/questions/28597149/add-xy-points-to-raster-map-generated-by-levelplot

#########################################

## Try to plot for the 3 types in the same graph.

## 2.1 cargo los datos de los tres seguidores
 
fixedAER <- stack("../../calc/proj12abr/outputTciclo/fixed_caer_yearlyProd_tday_20032009.grd")
fixedNO <- stack("../../calc/proj12abr/outputTciclo/fixed_cno_yearlyProd_tday_20032009.grd")
oneAER <- stack("../../calc/proj12abr/outputTciclo/oneAxis_caer_yearlyProd_tday_20032009.grd")
oneNO <- stack("../../calc/proj12abr/outputTciclo/oneAxis_cno_yearlyProd_tday_20032009.grd")
twoAER <- stack("../../calc/proj12abr/outputTciclo/twoAxes_caer_yearlyProd_tday_20032009.grd")
twoNO <- stack("../../calc/proj12abr/outputTciclo/twoAxes_cno_yearlyProd_tday_20032009.grd")

fixedAER <- setProj(fixedAER)
fixedNO <- setProj(fixedNO)
oneAER <- setProj(oneAER)
oneNO <- setProj(oneNO)
twoAER <- setProj(twoAER)
twoNO <- setProj(twoNO)

fixedAERm <- mean(fixedAER)
fixedNOm <- mean(fixedNO)
Dfixed <- fixedAERm-fixedNOm
oneAERm <- mean(oneAER)
oneNOm <- mean(oneNO)
Done <- oneAERm-oneNOm
twoAERm <- mean(twoAER)
twoNOm <- mean(twoNO)
Dtwo <- twoAERm-twoNOm
 
S <- stack(Dfixed, Done, Dtwo) ## stack de diferencias.
names(S) <- c("Fixed", "One", "Two")

## 2.2 Creo un stack de pvalues

pvalueF <- stackWilc(fixedAER, fixedNO)
pvalueO <- stackWilc(oneAER, oneNO)
pvalueT <- stackWilc(twoAER, twoNO)

PV <- stack(pvalueF, pvalueO, pvalueT)

pv <- lapply(1:nlayers(PV), FUN=function(i){
    breaks <- c(0, 0.05, max(PV[[i]][],na.rm=TRUE))
    pvalueclass <- cut(PV[[i]], breaks)
    pvalueclass <- ratify(pvalueclass)

    classes <- levels(pvalueclass)[[1]]
    classes$type <- c("SIG", "NO-SIG") 
    levels(pvalueclass) <- classes
    return(pvalueclass)}
    )

spv <- stack(pv)

## Creo los spatial points para los 3 raster:

pvpoints <- lapply(pv, FUN=function(i){
    nosig <- i
    nosig[nosig[] == 1] <- NA

    pts <- SpatialPoints(coordinates(nosig)[!is.na(values(nosig)),])
})

                   
pdf("useTday/dif_aer_no_all_Ym20032009SIGt.pdf", height=3, width=7)
levelplot(S, scales=list(draw=FALSE), colorkey=list(space='bottom'),layout=c(3,1), alpha.regions=1)+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))+
    layer(sp.points(pvpoints[[1]], pch=20, cex=0.2, alpha=1, lwd=0.1), columns=1)+
    layer(sp.points(pvpoints[[2]], pch=20, cex=0.2, alpha=1, lwd=0.1), columns=2)+
    layer(sp.points(pvpoints[[3]], pch=20, cex=0.2, alpha=1, lwd=0.1), columns=3)   
dev.off()

layer(sp.points(a, pch=3, cex=0.2, alpha=1))

## Estacional:

## Primero creo un Stack con las diferencias o diferencias relativas que quiera representar.

## DJF: ##

fixedDJFaer <- stack("../../calc/proj12abr/outputTciclo/DJF_all_fixed_caer.grd")
fixedDJFno <- stack("../../calc/proj12abr/outputTciclo/DJF_all_fixed_cno.grd")
 
fixedAER <- setProj(fixedDJFaer)
fixedNO <- setProj(fixedDJFno)

fixedAERm <- mean(fixedAER)
fixedNOm <- mean(fixedNO)

fixedDJF <- (fixedAERm-fixedNOm)
fixedDJF <- fixedDJF/fixedNOm

##

oneDJFaer <- stack("../../calc/proj12abr/outputTciclo/DJF_all_one_caer.grd")
oneDJFno <- stack("../../calc/proj12abr/outputTciclo/DJF_all_one_cno.grd")
 
oneAER <- setProj(oneDJFaer)
oneNO <- setProj(oneDJFno)

oneAERm <- mean(oneAER)
oneNOm <- mean(oneNO)

oneDJF <- (oneAERm-oneNOm)
oneDJF <- oneDJF/oneNOm

##

twoDJFaer <- stack("../../calc/proj12abr/outputTciclo/DJF_all_two_caer.grd")
twoDJFno <- stack("../../calc/proj12abr/outputTciclo/DJF_all_two_cno.grd")
 
twoAER <- setProj(twoDJFaer)
twoNO <- setProj(twoDJFno)

twoAERm <- mean(twoAER)
twoNOm <- mean(twoNO)

twoDJF <- (twoAERm-twoNOm)
twoDJF <- twoDJF/twoNOm

DJF <- stack(fixedDJF, oneDJF, twoDJF)
names(DJF) <- c("Fixed", "One", "Two") 

## 2.2 Creo un stack de pvalues

pvalueF <- stackWilc(fixedAER, fixedNO)
pvalueO <- stackWilc(oneAER, oneNO)
pvalueT <- stackWilc(twoAER, twoNO)

PV <- stack(pvalueF, pvalueO, pvalueT)

pv <- lapply(1:nlayers(PV), FUN=function(i){
    breaks <- c(0, 0.05, max(PV[[i]][],na.rm=TRUE))
    pvalueclass <- cut(PV[[i]], breaks)
    pvalueclass <- ratify(pvalueclass)

    classes <- levels(pvalueclass)[[1]]
    classes$type <- c("SIG", "NO-SIG") 
    levels(pvalueclass) <- classes
    return(pvalueclass)}
    )

spv <- stack(pv)

## Creo los spatial points para los 3 raster:

pvpoints <- lapply(pv, FUN=function(i){
    nosig <- i
    nosig[nosig[] == 1] <- NA

    pts <- SpatialPoints(coordinates(nosig)[!is.na(values(nosig)),])
})

pvpointsDJF <- pvpoints

########

## MAM ##

fixedMAMaer <- stack("../../calc/proj12abr/outputTciclo/MAM_all_fixed_caer.grd")
fixedMAMno <- stack("../../calc/proj12abr/outputTciclo/MAM_all_fixed_cno.grd")

fixedAER <- setProj(fixedMAMaer)
fixedNO <- setProj(fixedMAMno)

fixedAERm <- mean(fixedAER)
fixedNOm <- mean(fixedNO)

fixedMAM <- (fixedAERm-fixedNOm)
fixedMAM <- fixedMAM/fixedNOm

##

oneMAMaer <- stack("../../calc/proj12abr/outputTciclo/MAM_all_one_caer.grd")
oneMAMno <- stack("../../calc/proj12abr/outputTciclo/MAM_all_one_cno.grd")

oneAER <- setProj(oneMAMaer)
oneNO <- setProj(oneMAMno)

oneAERm <- mean(oneAERm)
oneNOm <- mean(oneNOm)

oneMAM <- (oneAERm-oneNOm)
oneMAM <- oneMAM/oneNOm

##

twoMAMaer <- stack("../../calc/proj12abr/outputTciclo/MAM_all_two_caer.grd")
twoMAMno <- stack("../../calc/proj12abr/outputTciclo/MAM_all_two_cno.grd")

twoAER <- setProj(twoMAMaer)
twoNO <- setProj(twoMAMno)

twoAERm <- mean(twoAER)
twoNOm <- mean(twoNO)
  
twoMAM <- (twoAERm-twoNOm)
twoMAM <- twoMAM/twoNOm

MAM <- stack(fixedMAM, oneMAM, twoMAM)
names(MAM) <- c("Fixed", "One", "Two")

## PVALUES

pvalueF <- stackWilc(fixedAER, fixedNO)
pvalueO <- stackWilc(oneAER, oneNO)
pvalueT <- stackWilc(twoAER, twoNO)

PV <- stack(pvalueF, pvalueO, pvalueT)

pv <- lapply(1:nlayers(PV), FUN=function(i){
    breaks <- c(0, 0.05, max(PV[[i]][],na.rm=TRUE))
    pvalueclass <- cut(PV[[i]], breaks)
    pvalueclass <- ratify(pvalueclass)

    classes <- levels(pvalueclass)[[1]]
    classes$type <- c("SIG", "NO-SIG") 
    levels(pvalueclass) <- classes
    return(pvalueclass)}
    )

spv <- stack(pv)

pvpoints <- lapply(pv, FUN=function(i){
    nosig <- i
    nosig[nosig[] == 1] <- NA

    pts <- SpatialPoints(coordinates(nosig)[!is.na(values(nosig)),])
})

pvpointsMAM <- pvpoints

####

## JJA

fixedJJAaer <- stack("../../calc/proj12abr/outputTciclo/JJA_all_fixed_caer.grd")
fixedJJAno <- stack("../../calc/proj12abr/outputTciclo/JJA_all_fixed_cno.grd")
 
fixedAER <- setProj(fixedJJAaer)
fixedNO <- setProj(fixedJJAno)

fixedAERm <- mean(fixedAER)
fixedNOm <- mean(fixedNO)

fixedJJA <- (fixedAERm-fixedNOm)
fixedJJA <- fixedJJA/fixedNOm

##

oneJJAaer <- stack("../../calc/proj12abr/outputTciclo/JJA_all_one_caer.grd")
oneJJAno <- stack("../../calc/proj12abr/outputTciclo/JJA_all_one_cno.grd")

oneAER <- setProj(oneJJAaer)
oneNO <- setProj(oneJJAno)

oneAERm <- mean(oneAER)
oneNOm <- mean(oneNO)

oneJJA <- (oneAERm-oneNOm)
oneJJA <- oneJJA/oneNOm

##

twoJJAaer <- stack("../../calc/proj12abr/outputTciclo/JJA_all_two_caer.grd")
twoJJAno <- stack("../../calc/proj12abr/outputTciclo/JJA_all_two_cno.grd")

twoAER <- setProj(twoJJAaer)
twoNO <- setProj(twoJJAno)

twoAERm <- mean(twoAER)
twoNOm <- mean(twoNO)

twoJJA <- (twoAERm-twoNOm)
twoJJA <- twoJJA/twoNOm

JJA <- stack(fixedJJA, oneJJA, twoJJA)
names(JJA) <- c("Fixed", "One", "Two")

## PVALUES

pvalueF <- stackWilc(fixedAER, fixedNO)
pvalueO <- stackWilc(oneAER, oneNO)
pvalueT <- stackWilc(twoAER, twoNO)

PV <- stack(pvalueF, pvalueO, pvalueT)

pv <- lapply(1:nlayers(PV), FUN=function(i){
    breaks <- c(0, 0.05, max(PV[[i]][],na.rm=TRUE))
    pvalueclass <- cut(PV[[i]], breaks)
    pvalueclass <- ratify(pvalueclass)

    classes <- levels(pvalueclass)[[1]]
    classes$type <- c("SIG", "NO-SIG") 
    levels(pvalueclass) <- classes
    return(pvalueclass)}
    )

spv <- stack(pv)

pvpoints <- lapply(pv, FUN=function(i){
    nosig <- i
    nosig[nosig[] == 1] <- NA

    pts <- SpatialPoints(coordinates(nosig)[!is.na(values(nosig)),])
})

pvpointsJJA <- pvpoints

## SON

fixedSONaer <- stack("../../calc/proj12abr/outputTciclo/SON_all_fixed_caer.grd")
fixedSONno <- stack("../../calc/proj12abr/outputTciclo/SON_all_fixed_cno.grd")

fixedAER <- setProj(fixedSONaer)
fixedNO <- setProj(fixedSONno)

fixedAERm <- mean(fixedAER)
fixedNOm <- mean(fixedNO)

fixedSON <- (fixedAERm-fixedNOm)
fixedSON <- fixedSON/fixedNOm

##

oneSONaer <- stack("../../calc/proj12abr/outputTciclo/SON_all_one_caer.grd")
oneSONno <- stack("../../calc/proj12abr/outputTciclo/SON_all_one_cno.grd")

oneAER <- setProj(oneSONaer)
oneNO <- setProj(oneSONno)

oneAERm <- mean(oneAER)
oneNOm <- mean(oneNO)

oneSON <- (oneAERm-oneNOm)
oneSON <- oneSON/oneNOm

##

twoSONaer <- stack("../../calc/proj12abr/outputTciclo/SON_all_two_caer.grd")
twoSONno <- stack("../../calc/proj12abr/outputTciclo/SON_all_two_cno.grd")

twoAER <- setProj(twoSONaer)
twoNO <- setProj(twoSONno)

twoAERm <- mean(twoAER)
twoNOm <- mean(twoNO)
  
twoSON <- (twoAERm-twoNOm)
twoSON <- twoSON/twoNOm

SON <- stack(fixedSON, oneSON, twoSON)
names(SON) <- c("Fixed", "One", "Two")

## PVALUES

pvalueF <- stackWilc(fixedAER, fixedNO)
pvalueO <- stackWilc(oneAER, oneNO)
pvalueT <- stackWilc(twoAER, twoNO)

PV <- stack(pvalueF, pvalueO, pvalueT)

pv <- lapply(1:nlayers(PV), FUN=function(i){
    breaks <- c(0, 0.05, max(PV[[i]][],na.rm=TRUE))
    pvalueclass <- cut(PV[[i]], breaks)
    pvalueclass <- ratify(pvalueclass)

    classes <- levels(pvalueclass)[[1]]
    classes$type <- c("SIG", "NO-SIG") 
    levels(pvalueclass) <- classes
    return(pvalueclass)}
    )

spv <- stack(pv)

pvpoints <- lapply(pv, FUN=function(i){
    nosig <- i
    nosig[nosig[] == 1] <- NA

    pts <- SpatialPoints(coordinates(nosig)[!is.na(values(nosig)),])
})

pvpointsSON <- pvpoints

## PLOT ALL THE SEASONS:

s <- stack(fixedDJF, oneDJF, twoDJF, fixedMAM, oneMAM, twoMAM, fixedJJA, oneJJA, twoJJA, fixedSON, oneSON, twoSON)

s1 <- s*100
nl <- nlayers(s1)

my.at <- seq(-30,0,5)

pdf("useTday/RelDif_aer_no_all20032009SIGt.pdf", height=6, width=6)
levelplot(s1, scales=list(draw=FALSE), ylab=list(c("SON", "JJA", "MAM", "DJF"), rot=0, cex=0.7), xlab=list(c("Fixed", "One", "Two"),cex=0.7), names.attr=c(rep('', 12)), at=my.at, layout=c(3,4))+ layer(sp.lines(border, lwd=0.3))+
    layer(sp.lines(grat, lwd=0.1)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.2))+
    layer(sp.points(pvpointsDJF[[1]], pch=20, cex=0.1, alpha=0.5,lwd=0.2, col='black'),columns=1, rows=1)+
    layer(sp.points(pvpointsDJF[[2]], pch=20, cex=0.1, alpha=0.5, lwd=0.2, col='black'), columns=2, rows=1)+
    layer(sp.points(pvpointsDJF[[3]], pch=20, cex=0.1, alpha=0.5, lwd=0.2, col='black'), columns=3, rows=1)+
    layer(sp.points(pvpointsMAM[[1]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.2), columns=1, rows=2)+
    layer(sp.points(pvpointsMAM[[2]], pch=20, cex=0.1, alpha=0.5, col='black', lwd=0.2), columns=2, rows=2)+
    layer(sp.points(pvpointsMAM[[3]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.2), columns=3, rows=2)+
    layer(sp.points(pvpointsJJA[[1]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.2), columns=1, rows=3)+
    layer(sp.points(pvpointsJJA[[2]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.1), columns=2, rows=3)+
    layer(sp.points(pvpointsJJA[[3]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.1), columns=3, rows=3)+
    layer(sp.points(pvpointsSON[[1]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.1), columns=1, rows=4)+
    layer(sp.points(pvpointsSON[[2]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.1), columns=2, rows=4)+
    layer(sp.points(pvpointsSON[[3]], pch=20, cex=0.1, alpha=0.5, col='black',lwd=0.1), columns=3, rows=4)
 
dev.off()

