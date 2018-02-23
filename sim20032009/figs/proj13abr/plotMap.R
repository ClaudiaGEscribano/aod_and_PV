## Aquí voy a escribir el código de mapas
 
source('projectInfo.R')
source('graticule.R')

## Leo los datos de salida de las simulaciones que vaya a representar.

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

source('border.R')

#######################################################
## 1. MEAN DIFFERENCE
######################################################

fixedAERm <- mean(fixedAER)
fixedNOm <- mean(fixedNO)
Dfixed <- fixedAERm-fixedNOm
oneAERm <- mean(oneAER)
oneNOm <- mean(oneNO)
Done <- oneAERm-oneNOm
twoAERm <- mean(twoAER)
twoNOm <- mean(twoNO)
Dtwo <- twoAERm-twoNOm
 
S <- stack(Dfixed, Done, Dtwo)
names(S) <- c("Fixed", "One", "Two")
 
pdf("useTday/dif_aer_no_all_Ym20032009.pdf", height=3, width=7)
levelplot(S, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'),layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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
 
##############################################
## 2. RELATIVE MEAN DIFFERENCE
##############################################

reldiffixed <-(fixedAERm-fixedNOm)/fixedNOm
reldifone <- (oneAERm-oneNOm)/oneNOm
reldiftwo <- (twoAERm-twoNOm)/twoNOm
 
my.at <- seq(-0.20, 0, 0.02)

S <- stack(reldiffixed, reldifone, reldiftwo)
names(S) <- c("Fixed", "One", "Two")

pdf("useTday/RelDif_aer_no_all_Ym20032009.pdf", height=3, width=7)
levelplot(S, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

#################################################
## 3. RELATIVE SEASONAL DIFFERENCES
################################################

## DJF ##

fixedDJFaer <- raster("../../calc/proj12abr/outputTciclo/DJF_fixed_caer.grd")
fixedDJFno <- raster("../../calc/proj12abr/outputTciclo/DJF_fixed_cno.grd")

fixedAER <- setProj(fixedDJFaer)
fixedNO <- setProj(fixedDJFno)
fixedDJF <- (fixedAER-fixedNO)/fixedNO
 
oneDJFaer <- raster("../../calc/proj12abr/outputTciclo/DJF_one_caer.grd")
oneDJFno <- raster("../../calc/proj12abr/outputTciclo/DJF_one_cno.grd")

oneAER <- setProj(oneDJFaer)
oneNO <- setProj(oneDJFno)
oneDJF <- (oneAER-oneNO)/oneNO

twoDJFaer <- raster("../../calc/proj12abr/outputTciclo/DJF_two_caer.grd")
twoDJFno <- raster("../../calc/proj12abr/outputTciclo/DJF_two_cno.grd")

twoAER <- setProj(twoDJFaer)
twoNO <- setProj(twoDJFno)
twoDJF <- (twoAER-twoNO)/twoNO

DJF <- stack(fixedDJF, oneDJF, twoDJF)
names(DJF) <- c("Fixed", "One", "Two")


my.at <- seq(-0.35, 0, 0.05)

pdf("useTday/RelDif_aer_no_DJF20032009.pdf", height=3, width=7)
levelplot(DJF, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

## MAM ##

fixedMAMaer <- raster("../../calc/proj12abr/outputTciclo/MAM_fixed_caer.grd")
fixedMAMno <- raster("../../calc/proj12abr/outputTciclo/MAM_fixed_cno.grd")

fixedAER <- setProj(fixedMAMaer)
fixedNO <- setProj(fixedMAMno)
fixedMAM <- (fixedAER-fixedNO)/fixedNO
 
oneMAMaer <- raster("../../calc/proj12abr/outputTciclo/MAM_one_caer.grd")
oneMAMno <- raster("../../calc/proj12abr/outputTciclo/MAM_one_cno.grd")

oneAER <- setProj(oneMAMaer)
oneNO <- setProj(oneMAMno)
oneMAM <- (oneAER-oneNO)/oneNO

twoMAMaer <- raster("../../calc/proj12abr/outputTciclo/MAM_two_caer.grd")
twoMAMno <- raster("../../calc/proj12abr/outputTciclo/MAM_two_cno.grd")

twoAER <- setProj(twoMAMaer)
twoNO <- setProj(twoMAMno)
twoMAM <- (twoAER-twoNO)/twoNO
 
MAM <- stack(fixedMAM, oneMAM, twoMAM)
names(MAM) <- c("Fixed", "One", "Two")

pdf("useTday/RelDif_aer_no_MAM20032009.pdf", height=3, width=7)
levelplot(MAM, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

## JJA

fixedJJAaer <- raster("../../calc/proj12abr/outputTciclo/JJA_fixed_caer.grd")
fixedJJAno <- raster("../../calc/proj12abr/outputTciclo/JJA_fixed_cno.grd")

fixedAER <- setProj(fixedJJAaer)
fixedNO <- setProj(fixedJJAno)
fixedJJA <- (fixedAER-fixedNO)/fixedNO
 
oneJJAaer <- raster("../../calc/proj12abr/outputTciclo/JJA_one_caer.grd")
oneJJAno <- raster("../../calc/proj12abr/outputTciclo/JJA_one_cno.grd")

oneAER <- setProj(oneJJAaer)
oneNO <- setProj(oneJJAno)
oneJJA <- (oneAER-oneNO)/oneNO

twoJJAaer <- raster("../../calc/proj12abr/outputTciclo/JJA_two_caer.grd")
twoJJAno <- raster("../../calc/proj12abr/outputTciclo/JJA_two_cno.grd")

twoAER <- setProj(twoJJAaer)
twoNO <- setProj(twoJJAno)
twoJJA <- (twoAER-twoNO)/twoNO
 
JJA <- stack(fixedJJA, oneJJA, twoJJA)
names(JJA) <- c("Fixed", "One", "Two")
 
pdf("useTday/RelDif_aer_no_JJA20032009.pdf", height=3, width=7)
levelplot(JJA, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

## SON

fixedSONaer <- raster("../../calc/proj12abr/outputTciclo/SON_fixed_caer.grd")
fixedSONno <- raster("../../calc/proj12abr/outputTciclo/SON_fixed_cno.grd")

fixedAER <- setProj(fixedSONaer)
fixedNO <- setProj(fixedSONno)
fixedSON <- (fixedAER-fixedNO)/fixedNO
 
oneSONaer <- raster("../../calc/proj12abr/outputTciclo/SON_one_caer.grd")
oneSONno <- raster("../../calc/proj12abr/outputTciclo/SON_one_cno.grd")

oneAER <- setProj(oneSONaer)
oneNO <- setProj(oneSONno)
oneSON <- (oneAER-oneNO)/oneNO

twoSONaer <- raster("../../calc/proj12abr/outputTciclo/SON_two_caer.grd")
twoSONno <- raster("../../calc/proj12abr/outputTciclo/SON_two_cno.grd")
 
twoAER <- setProj(twoSONaer)
twoNO <- setProj(twoSONno)
twoSON <- (twoAER-twoNO)/twoNO

SON <- stack(fixedSON, oneSON, twoSON)
names(SON) <- c("Fixed", "One", "Two")

pdf("useTday/RelDif_aer_no_SON20032009.pdf", height=3, width=7)
levelplot(SON, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

## TODOS

s <- stack(fixedDJF, oneDJF, twoDJF, fixedMAM, oneMAM, twoMAM, fixedJJA, oneJJA, twoJJA, fixedSON, oneSON, twoSON)

nl <- nlayers(s)

pdf("useTday/RelDif_aer_no_all20032009.pdf", height=5, width=4)
              
levelplot(s, scales=list(draw=FALSE), ylab=list(c("SON", "JJA", "MAM", "DJF"), rot=0), xlab=c("Fixed", "One", "Two"), names.attr=c(rep('', 12)), at=my.at, layout=c(3,4))+ layer(sp.lines(border, lwd=0.5))+
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


