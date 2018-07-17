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
 
pdf("useTday/dif_aer_no_all_Ym20032009SIG.pdf", height=3, width=7)
levelplot(S, scales=list(draw=FALSE), colorkey=list(space='bottom'),layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

## 1. significance test:

stackWilc <- function(s1, s2) { 
  mycor <- function(v) { 
    x <- v[1:split] 
    y <- v[(split+1):(2*split)]
    if (is.na(v) == TRUE) { v <-  NA}
    else {
    wilcox.test(x, y, exact=TRUE)$p.value
  }}
  s <- stack(s1, s2) 
  split <- nlayers(s)/2
  calc(s, fun=mycor) 
} 
 

## 1.1 p-valor del test para FIXED aer/no-aer

pvalue <- stackWilc(fixedAER, fixedNO)

## 1.1.1 Dibujo el p-valor

pdf("pvalueWILCOX.pdf")
levelplot(pvalue, margin=FALSE, scales=list(draw=FALSE), colorkey=list(space='bottom')))+ layer(sp.lines(border, lwd=0.5))+
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

pvalue[pvalue[]>=0.05] <- NA

pdf("pvalueWILCOXsig.pdf")
levelplot(pvalue, margin=FALSE, scales=list(draw=FALSE), colorkey=list(space='bottom', title='kWh/kWp'))+ layer(sp.lines(border, lwd=0.5))+
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

a <- which(pvalue[] >= 0.05)
Dfixed[a] <- NA

pvalue <- stackWilc(oneAER, oneNO)
pvalue[pvalue[]>=0.05] <- NA
a <- which(pvalue[] >= 0.05)
Done[a] <- NA

pvalue <- stackWilc(twoAER, twoNO)
pvalue[pvalue[]>=0.05] <- NA
a <- which(pvalue[] >= 0.05)
Dtwo[a] <- NA

##############################################
## 2. RELATIVE MEAN DIFFERENCE
##############################################

reldiffixed <-(fixedAERm-fixedNOm)/fixedNOm
reldifone <- (oneAERm-oneNOm)/oneNOm
reldiftwo <- (twoAERm-twoNOm)/twoNOm

reldiffixed <-Dfixed/fixedNOm
reldifone <- Done/oneNOm
reldiftwo <- Dtwo/twoNOm


reldiffixed <- reldiffixed*100
reldifone <- reldifone*100
reldiftwo <- reldiftwo*100


my.at <- seq(-20, 0, 2)

S <- stack(reldiffixed, reldifone, reldiftwo)
names(S) <- c("Fixed", "One", "Two")

pdf("useTday/RelDif_aer_no_all_Ym20032009SIG.pdf", height=3, width=7)
levelplot(S, scales=list(draw=FALSE), colorkey=list(space='bottom'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

## con significancia estadística:

## DJF ##

fixedDJFaer <- stack("../../calc/proj12abr/outputTciclo/DJF_all_fixed_caer.grd")
fixedDJFno <- stack("../../calc/proj12abr/outputTciclo/DJF_all_fixed_cno.grd")
 
fixedAER <- setProj(fixedDJFaer)
fixedNO <- setProj(fixedDJFno)

pvalue <- stackWilc(fixedAER, fixedNO)
a <- which(pvalue[] >= 0.05)
 
fixedAER <- mean(fixedAER)
fixedNO <- mean(fixedNO)

fixedDJF <- (fixedAER-fixedNO)
fixedDJF[a] <- NA
fixedDJF <- fixedDJF/fixedNO

oneDJFaer <- stack("../../calc/proj12abr/outputTciclo/DJF_all_one_caer.grd")
oneDJFno <- stack("../../calc/proj12abr/outputTciclo/DJF_all_one_cno.grd")

oneAER <- setProj(oneDJFaer)
oneNO <- setProj(oneDJFno)

pvalue <- stackWilc(oneAER, oneNO)
a <- which(pvalue[] >= 0.05)
 
oneAER <- mean(oneAER)
oneNO <- mean(oneNO)

oneDJF <- (oneAER-oneNO)
oneDJF[a] <- NA
oneDJF <- oneDJF/oneNO

twoDJFaer <- stack("../../calc/proj12abr/outputTciclo/DJF_all_two_caer.grd")
twoDJFno <- stack("../../calc/proj12abr/outputTciclo/DJF_all_two_cno.grd")

twoAER <- setProj(twoDJFaer)
twoNO <- setProj(twoDJFno)

pvalue <- stackWilc(twoAER, twoNO)
a <- which(pvalue[] >= 0.05)

twoAER <- mean(twoAER)
twoNO <- mean(twoNO)

twoDJF <- (twoAER-twoNO)
twoDJF[a] <- NA
twoDJF <- twoDJF/twoNO

DJF <- stack(fixedDJF, oneDJF, twoDJF)
names(DJF) <- c("Fixed", "One", "Two")

my.at <- seq(-0.35, 0, 0.05)

pdf("useTday/RelDif_aer_no_DJF20032009SIG.pdf", height=3, width=7)
levelplot(DJF, scales=list(draw=FALSE), colorkey=list(space='bottom'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

fixedMAMaer <- stack("../../calc/proj12abr/outputTciclo/MAM_all_fixed_caer.grd")
fixedMAMno <- stack("../../calc/proj12abr/outputTciclo/MAM_all_fixed_cno.grd")

fixedAER <- setProj(fixedMAMaer)
fixedNO <- setProj(fixedMAMno)

pvalue <- stackWilc(fixedAER, fixedNO)
a <- which(pvalue[] >= 0.05)

fixedAER <- mean(fixedAER)
fixedNO <- mean(fixedNO)

fixedMAM <- (fixedAER-fixedNO)
fixedMAM[a] <- NA
fixedMAM <- fixedMAM/fixedNO
    
oneMAMaer <- stack("../../calc/proj12abr/outputTciclo/MAM_all_one_caer.grd")
oneMAMno <- stack("../../calc/proj12abr/outputTciclo/MAM_all_one_cno.grd")

oneAER <- setProj(oneMAMaer)
oneNO <- setProj(oneMAMno)

pvalue <- stackWilc(oneAER, oneNO)
a <- which(pvalue[] >= 0.05)

oneAER <- mean(oneAER)
oneNO <- mean(oneNO)

oneMAM <- (oneAER-oneNO)
oneMAM[a] <- NA
oneMAM <- oneMAM/oneNO

twoMAMaer <- stack("../../calc/proj12abr/outputTciclo/MAM_all_two_caer.grd")
twoMAMno <- stack("../../calc/proj12abr/outputTciclo/MAM_all_two_cno.grd")

twoAER <- setProj(twoMAMaer)
twoNO <- setProj(twoMAMno)

pvalue <- stackWilc(twoAER, twoNO)
a <- which(pvalue[] >= 0.05)

twoAER <- mean(twoAER)
twoNO <- mean(twoNO)
  
twoMAM <- (twoAER-twoNO)
twoMAM[a] <- NA
twoMAM <- twoMAM/twoNO
 
MAM <- stack(fixedMAM, oneMAM, twoMAM)
names(MAM) <- c("Fixed", "One", "Two")

pdf("useTday/RelDif_aer_no_MAM20032009SIG.pdf", height=3, width=7)
levelplot(MAM, scales=list(draw=FALSE), colorkey=list(space='bottom'), at=my.at, layout=c(3,1))+ layer(sp.lines(border, lwd=0.5))+
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

fixedJJAaer <- stack("../../calc/proj12abr/outputTciclo/JJA_all_fixed_caer.grd")
fixedJJAno <- stack("../../calc/proj12abr/outputTciclo/JJA_all_fixed_cno.grd")
 
fixedAER <- setProj(fixedJJAaer)
fixedNO <- setProj(fixedJJAno)

pvalue <- stackWilc(fixedAER, fixedNO)
a <- which(pvalue[] >= 0.05)

fixedAER <- mean(fixedAER)
fixedNO <- mean(fixedNO)

fixedJJA <- (fixedAER-fixedNO)
fixedJJA[a] <- NA
fixedJJA <- fixedJJA/fixedNO
 
oneJJAaer <- stack("../../calc/proj12abr/outputTciclo/JJA_all_one_caer.grd")
oneJJAno <- stack("../../calc/proj12abr/outputTciclo/JJA_all_one_cno.grd")

oneAER <- setProj(oneJJAaer)
oneNO <- setProj(oneJJAno)

pvalue <- stackWilc(oneAER, oneNO)
a <- which(pvalue[] >= 0.05)

oneAER <- mean(oneAER)
oneNO <- mean(oneNO)

oneJJA <- (oneAER-oneNO)
oneJJA[a] <- NA
oneJJA <- oneJJA/oneNO

twoJJAaer <- stack("../../calc/proj12abr/outputTciclo/JJA_all_two_caer.grd")
twoJJAno <- stack("../../calc/proj12abr/outputTciclo/JJA_all_two_cno.grd")

twoAER <- setProj(twoJJAaer)
twoNO <- setProj(twoJJAno)

pvalue <- stackWilc(twoAER, twoNO)
a <- which(pvalue[] >= 0.05)

twoAER <- mean(twoAER)
twoNO <- mean(twoNO)
  
twoJJA <- (twoAER-twoNO)
twoJJA[a] <- NA
twoJJA <- twoJJA/twoNO

JJA <- stack(fixedJJA, oneJJA, twoJJA)
names(JJA) <- c("Fixed", "One", "Two")
 
pdf("useTday/RelDif_aer_no_JJA20032009SIG.pdf", height=3, width=7)
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

fixedSONaer <- stack("../../calc/proj12abr/outputTciclo/SON_all_fixed_caer.grd")
fixedSONno <- stack("../../calc/proj12abr/outputTciclo/SON_all_fixed_cno.grd")

fixedAER <- setProj(fixedSONaer)
fixedNO <- setProj(fixedSONno)

pvalue <- stackWilc(fixedAER, fixedNO)
a <- which(pvalue[] >= 0.05)

fixedAER <- mean(fixedAER)
fixedNO <- mean(fixedNO)

fixedSON <- (fixedAER-fixedNO)
fixedSON[a] <- NA
fixedSON <- fixedSON/fixedNO
 
oneSONaer <- stack("../../calc/proj12abr/outputTciclo/SON_all_one_caer.grd")
oneSONno <- stack("../../calc/proj12abr/outputTciclo/SON_all_one_cno.grd")

oneAER <- setProj(oneSONaer)
oneNO <- setProj(oneSONno)

pvalue <- stackWilc(oneAER, oneNO)
a <- which(pvalue[] >= 0.05)

oneAER <- mean(oneAER)
oneNO <- mean(oneNO)

oneSON <- (oneAER-oneNO)
oneSON[a] <- NA
oneSON <- oneSON/oneNO

twoSONaer <- stack("../../calc/proj12abr/outputTciclo/SON_all_two_caer.grd")
twoSONno <- stack("../../calc/proj12abr/outputTciclo/SON_all_two_cno.grd")
 
twoAER <- setProj(twoSONaer)
twoNO <- setProj(twoSONno)

pvalue <- stackWilc(twoAER, twoNO)
a <- which(pvalue[] >= 0.05)

twoAER <- mean(twoAER)
twoNO <- mean(twoNO)
  
twoSON <- (twoAER-twoNO)
twoSON[a] <- NA
twoSON <- twoSON/twoNO

SON <- stack(fixedSON, oneSON, twoSON)
names(SON) <- c("Fixed", "One", "Two")

pdf("useTday/RelDif_aer_no_SON20032009SIG.pdf", height=3, width=7)
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

s1 <- s*100
nl <- nlayers(s1)

my.at <- seq(-30,0,5)

pdf("useTday/RelDif_aer_no_all20032009SIG.pdf", height=6, width=6)
levelplot(s1, scales=list(draw=FALSE), ylab=list(c("SON", "JJA", "MAM", "DJF"), rot=0, cex=0.7), xlab=list(c("Fixed", "One", "Two"),cex=0.7), names.attr=c(rep('', 12)), at=my.at, layout=c(3,4))+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.2)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.2))
dev.off()
 

