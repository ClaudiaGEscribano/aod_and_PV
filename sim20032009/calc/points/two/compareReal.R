library(raster)
library(rasterVis)
library(zoo)

## This script will compare real PV data with the assessment with satelite data.

## ## load the real data ## ##
##############################

load("../carmonaYf.Rdata")
load("carmonaSimYf_month.Rdata")

## los datos son la energía diaria acumulada. Calculo la media mensual de energía diaria acumulada.

carmonaMon <- aggregate(Yf, by=as.yearmon, 'mean')
carmonaSD <- apply(carmonaMon, 1, FUN='sd', na.rm=TRUE)

p <-aggregate(Yf, by=as.yearmon, FUN=function(x) length(x)) # para comprobar si faltan días

carmonaMon <- zoo(rowMeans(carmonaMon, na.rm=TRUE), index(carmonaMon))

## 'a' contiene los valores medios de producción mensuales y la desviación estandar parlos 18 inversores.

a <- cbind(carmonaMon, carmonaSD)
 
########################################
## COMPARE CAER/CNO SIMULATION WITH REAL DATA
########################################

tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')

## xProdM y xProdMno son los datos obtenidos con pvpoint.R

xProd <- zoo(xProdM, order.by=as.yearmon(tt))
xProdno <- zoo(xProdMno, order.by=as.yearmon(tt))

c <- merge(carmonaMon, xProd, xProdno, all=FALSE)#, carmona_twoMeses_cno, carmona_twoMeses_sat, carmona_aod, all=FALSE)
names(c) <- c("REAL", "CAER", "CNO") #, "SAT", "AOD")

d <- as.data.frame(c)
d <- melt(d)

c <- c[-18] ## Este mes tiene muy pocos días.
  
pdf("seriesCarmonamodelosreal2.pdf")
xyplot(c,screens=c(1,1,1),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
dev.off()

rmse <- sqrt( mean( (c$CAER - c$REAL)^2, na.rm = TRUE) )
## 1.041356

rmseNO <- sqrt( mean( (c$CNO - c$REAL)^2, na.rm = TRUE) ) 
## 0.7850891

mae <- mean(c$CAER - c$REAL)
## -0.7962638
maeNO <- mean(c$CNO - c$REAL)
## -0.4136843

#####################################################

## ## creo un objeto que sea la diferencia

## b <-c[,2:4]-c[,1]
## b$AOD <- c[,5]
## z <- c("DIF REAL DATA [kWh/kWp]", "AOD")

## pdf("seriesDifCarmona.pdf")
## xyplot(b,screens=c(1,1,1,2),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE, strip=strip.custom(factor.levels=z),
##        panel=function(...) {
##           panel.xyplot(...)
##           panel.abline(h=c(0, 0.5, 1.0, 1.5), col='grey')})
## dev.off()


## ###

## caer <-as.data.frame( b[,1])
## cno <- as.data.frame(b[,2])
## sat <- as.data.frame(b[,3])
## AOD <- as.data.frame(b[,4])

## caer$model <- "caer"
## names(caer) <- c("value", "model")
## cno$model <- "cno"
## names(cno) <- c("value", "model")
## sat$model <- "sat"
## names(sat) <- c("value", "model")

## data <- rbind(caer, cno, sat)
## data$AOD <- rep(b[,4], 3)

## myTheme <- custom.theme.2(pch = 20, cex = 1.3)

## pdf("scatterALL2.pdf")
## xyplot(value~AOD, group=model, data=data, par.settings =custom.theme(pch=20, cex=1.3), auto.key=TRUE,
##        panel=function(...){
##        panel.xyplot(...)
##        panel.abline(h=c(0.5, 0, 1.0), col='grey')})
## dev.off()
 

## sat <- as.vector(carmona_twoMeses_sat)
## cno <- as.vector(twoMeses_cno)
## aod <- as.vector(aod)
## xProd <- as.vector(xProd)
## carmona <- as.vector(carmonaMon)

## matrix <- cbind( cno, sat, aod, xProd, carmona)
## df <- as.data.frame(matrix)
## names(df) <- c( "cno", "sat", "aod", "xProd")
 
## dfm <- df[,4]-df[1:3]
## ##

## x <- data.frame(b)
