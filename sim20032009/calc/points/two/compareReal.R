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
  
pdf("seriesCarmonamodelosreal3.pdf")
xyplot(c,screens=c(1,1,1),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
dev.off()

rmse <- sqrt( mean( (c$CAER - c$REAL)^2, na.rm = TRUE) )
## 0.2674931

rmseNO <- sqrt( mean( (c$CNO - c$REAL)^2, na.rm = TRUE) )
## 0.6730644
 

mae <- mean(c$CAER - c$REAL)
## 0.185356
maeNO <- mean(c$CNO - c$REAL)
## 0.5976998

## DIFERENCIAS

err <- cbind(c$CAER-c$REAL, c$CNO-c$REAL)
names(err) <- c("CAER", "CNO")

pdf("CarmonaDiferencias2.pdf")
xyplot(err, scales = list(x = list(at = index(c), rot=45)), type='b', ylab='kWh/m²', par.settings=myTheme,superpose=TRUE,
           panel = function(...) {
        panel.grid()#col="grey", lwd=0.1, h=5, v=0)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()

## summary(err)
##      Index           CAER               CNO        
##  Min.   :2008   Min.   :-0.13962   Min.   :0.1568  
##  1st Qu.:2008   1st Qu.: 0.07222   1st Qu.:0.3674  
##  Median :2008   Median : 0.16710   Median :0.5654  
##  Mean   :2008   Mean   : 0.18536   Mean   :0.5977  
##  3rd Qu.:2008   3rd Qu.: 0.22946   3rd Qu.:0.8206  
##  Max.   :2009   Max.   : 0.60867   Max.   :1.2248
 
rerr <- cbind((c$CAER-c$REAL)/c$REAL, (c$CNO-c$REAL)/c$REAL)
names(rerr) <- c("CAER", "CNO")

## summary(rerr)
##      Index           CAER                CNO         
##  Min.   :2008   Min.   :-0.024826   Min.   :0.02788  
##  1st Qu.:2008   1st Qu.: 0.008794   1st Qu.:0.04690  
##  Median :2008   Median : 0.033898   Median :0.11500  
##  Mean   :2008   Mean   : 0.036286   Mean   :0.11145  
##  3rd Qu.:2008   3rd Qu.: 0.050163   3rd Qu.:0.15666  
##  Max.   :2009   Max.   : 0.149535   Max.   :0.24856


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
