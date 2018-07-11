## script para representar el target diagram de las simulaciones de productividad con respecto a los datos reales:

library(tdr)
library(zoo)


######## ejemplos #########
 
z <- zoo(cbind(pvModels, pvObs),
         order =  as.Date(rownames(pvModels)))
plot(z, plot.type  = 'single',
     col = c(rep('gray', 22), 'red'),
     ylab = '', xlab = '')

## la función applyStats te da estadísticas entre modelo y observaciones.

errModel <- applyStats(pvModels, pvObs)

targetDiagram(errModel, groups = model)

########################################

## después de hacer pvpoint y empezar compareReal:

xProd <- zoo(xProdM, order.by=as.yearmon(tt))
xProdno <- zoo(xProdMno, order.by=as.yearmon(tt))
xProdsat <- zoo(xProdMsat, order.by=as.yearmon(tt))  

c <- merge(xProd, xProdno, xProdsat, carmonaMon, all=FALSE)

names(c) <- c("AER", "NO-AER","SAT", "REAL")
pvmodels <- c[,1:3]
pvobs <- c[,4]

names(c) <- c("AER", "NO-AER","SAT", "REAL")
pvmodels <- c[-18,1:3]
pvobs <- c[-18,4]

## calculo estadísticas con tdr:

errModel <- applyStats(pvmodels, pvobs)

##       mo       mm      sdo      sdm       mbe       mae      rmse       nmbe
## 1 5.957916 6.143272 1.416185 1.342903 0.1853560 0.2167638 0.2674931 0.04423678
## 2 5.957916 6.555616 1.416185 1.294880 0.5976998 0.5976998 0.6730644 0.14264611
## 3 5.957916 6.507945 1.416185 1.455766 0.5500290 0.5500290 0.5871861 0.13126906
##        cvmbe      nmae      cvmae      nrmse     cvrmse        r2    tStone
## 1 0.03111088 0.0517325 0.03638248 0.06383948 0.04489709 0.9821239  3.844327
## 2 0.10032029 0.1426461 0.10032029 0.16063251 0.11296978 0.9530980  7.725506
## 3 0.09231902 0.1312691 0.09231902 0.14013693 0.09855562 0.9790931 10.702938
##    model
## 1    AER
## 2 NO-AER
## 3    SAT


myTheme <- custom.theme.2(cex=2)
##myTheme$strip.background$col <- 'transparent'
##myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) 

pdf('targetSeville.pdf')
targetDiagram(errModel, groups = model, par.settings=myTheme)
dev.off()

## TARRAGONA

c <- merge(xProd, xProdno, xProdsat, photocampaMon, all=FALSE)

names(c) <- c("AER", "NO-AER","SAT", "REAL")
pvmodels <- c[,1:3]
pvobs <- c[,4]

errModel <- applyStats(pvmodels, pvobs)

errModel

##         mo       mm       sdo      sdm       mbe       mae      rmse      nmbe
## 1 2.554084 3.165701 0.9125276 1.206845 0.6116167 0.6381562 0.7696081 0.2134457
## 2 2.554084 3.375753 0.9125276 1.295179 0.8216691 0.8216691 0.9568768 0.2867511
## 3 2.554084 3.194906 0.9125276 1.129301 0.6408218 0.6408218 0.7580114 0.2236379
##       cvmbe      nmae     cvmae     nrmse    cvrmse        r2   tStone  model
## 1 0.2394662 0.2227076 0.2498572 0.2685826 0.3013245 0.8686155 4.535457    AER
## 2 0.3217080 0.2867511 0.3217080 0.3339367 0.3746458 0.9057974 5.804362 NO-AER
## 3 0.2509008 0.2236379 0.2509008 0.2645355 0.2967841 0.8772840 5.482774    SAT

pdf('targetTarragona2.pdf')
targetDiagram(errModel, groups = model, par.settings=myTheme)
dev.off()
