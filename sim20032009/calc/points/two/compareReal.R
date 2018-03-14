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
xProdsat <- zoo(xProdMsat, order.by=as.yearmon(tt))  

c <- merge(carmonaMon, xProd, xProdno, xProdsat, all=FALSE)#, carmona_twoMeses_cno, carmona_twoMeses_sat, carmona_aod, all=FALSE)
names(c) <- c("REAL", "CAER", "CNO","SAT") ##"AOD")

d <- as.data.frame(c)
d <- melt(d)

c <- c[-18] ## Este mes tiene muy pocos días.

## Theme for plotting

myTheme <- custom.theme.2()
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) 

names(c) <- c("REAL", "CAER", "CNO", "SAT")

pdf("seriesCarmonaALL.pdf")
xyplot(c,screens=c(1,1,1), scales = list(x = list(at = index(c), rot=45)),
       type='b', par.settings=myTheme, superpose=TRUE, grid=TRUE)
dev.off()

## cálculos de los estimadores estadísticos:

rmse <- sqrt( mean( (c$CAER - c$REAL)^2, na.rm = TRUE) )
## 0.2674931
rmseNO <- sqrt( mean( (c$CNO - c$REAL)^2, na.rm = TRUE) )
## 0.6730644
rmseSAT <- sqrt( mean( (c$SAT - c$REAL)^2, na.rm = TRUE) )
## 0.5871861


mae <- mean(c$CAER - c$REAL)
## 0.185356
maeNO <- mean(c$CNO - c$REAL)
## 0.5976998
maeSAT <- mean(c$SAT - c$REAL)
## 0.550029

## cor

cor1 <- cor(c$CAER,c$REAL)
##0.9910216
cor2 <- cor(c$CNO,c$REAL)
##0.9762674
cor3 <- cor(c$SAT,c$REAL)
##0.9894913

## sdr
sdr1 <- sd(c$CAER)/sd(c$REAL)
## 0.9482534
sdr2 <- sd(c$CNO)/sd(c$REAL)
## 0.9143432
sdr3 <- sd(c$SAT)/sd(c$REAL)
## 1.027948

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

## Errores añadiendo el sat

err2 <- cbind(c$CAER-c$REAL, c$CNO-c$REAL, c$SAT-c$REAL)
names(err2) <- c("AER", "NO-AER", "SAT")

pdf("CarmonaDiferenciasabsolutas.pdf")
xyplot(err2, scales = list(x = list(at = index(c), rot=45)), type='b', ylab='kWh/m²', par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
               panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()

dferr2 <- melt(as.data.frame(err2))

pdf("pruebasvis.pdf")
xyplot(value~variable, groups=variable,data=dferr2, type='p', ylab='Error kWh/kWp', xlab='model', cex=1.3 ,par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
               panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()

pdf("pruebascatter.pdf")
xyplot(CAER+CNO+SAT~REAL, data=c, type='p', ylab='MODEL', xlab='REAL', cex=1.3 ,par.settings=myTheme,superpose=TRUE, grid=TRUE, auto.key=TRUE, xlim=c(4,9.3), ylim=c(4,9.3),
           panel = function(...) {
               panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()

pdf("pruebasviolinTwo.pdf")
bwplot(value~variable, groups=variable,data=dferrCarmona, ylab='Error kWh/kWp', xlab='model', ylim=c(-0.5,2),
       panel = function(..., cex,box.ratio) {
           panel.violin(..., col='transparent',
 varwidth = FALSE, box.ratio = box.ratio)
 panel.bwplot(..., col='black',
 cex=2, pch='|', fill=c(blue,red,green), box.ratio =0.1)
           panel.abline(h=0, col='grey')
       },
 par.settings = list(box.rectangle=list(col='black',lwd=2),
 plot.symbol = list(pch='.', cex=4,lwd= 1)))
dev.off()

 
pdf("pruebasboxTwo.pdf")
bwplot(value~variable, groups=variable,data=dferrCarmona, ylab='Error kWh/kWp', xlab='model',
       panel = function(...,box.ratio) {
       panel.bwplot(..., col='black',
 cex=2, pch='|', fill='gray', box.ratio =1)
 },
 par.settings = list(box.rectangle=list(col='black',lwd=2),
 plot.symbol = list(pch='.', cex=4,lwd= 1)))
dev.off()


## Un color para cada  grupo
## defino los colores:

pal <- brewer.pal(n=7, 'Set1')
colors <- pal[1:3]
key <- list(col=colors)

red <- "#E41A1C"
blue <- "#377EB8"
green <- "#4DAF4A"


pdf("a.pdf")
bwplot(value~variable, groups=variable,data=dferrCarmona, ylab='Error kWh/kWp', xlab='model',
       panel = function(...,box.ratio) {
       panel.bwplot(..., col='black',
 cex=2, pch='|', fill=c(blue,red,green), box.ratio =1)
 },
 par.settings = list(box.rectangle=list(col='black',lwd=2),
 plot.symbol = list(pch='.', cex=4,lwd= 1)))
dev.off()



## summary(err2)
##      Index           CAER               CNO              SAT        
##  Min.   :2008   Min.   :-0.13962   Min.   :0.1568   Min.   :0.1592  
##  1st Qu.:2008   1st Qu.: 0.07222   1st Qu.:0.3674   1st Qu.:0.3940  
##  Median :2008   Median : 0.16710   Median :0.5654   Median :0.5242  
##  Mean   :2008   Mean   : 0.18536   Mean   :0.5977   Mean   :0.5500  
##  3rd Qu.:2008   3rd Qu.: 0.22946   3rd Qu.:0.8206   3rd Qu.:0.7385  
##  Max.   :2009   Max.   : 0.60867   Max.   :1.2248   Max.   :0.8504

## errores relativos:

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

## errores relativos con el sat:

rerr2 <- cbind((c$CAER-c$REAL)/c$REAL, (c$CNO-c$REAL)/c$REAL, (c$SAT-c$REAL)/c$REAL)
names(rerr2) <- c("CAER", "CNO", "SAT")

## summary(rerr2)
##      Index           CAER                CNO               SAT         
##  Min.   :2008   Min.   :-0.024826   Min.   :0.02788   Min.   :0.02726  
##  1st Qu.:2008   1st Qu.: 0.008794   1st Qu.:0.04690   1st Qu.:0.06857  
##  Median :2008   Median : 0.033898   Median :0.11500   Median :0.08941  
##  Mean   :2008   Mean   : 0.036286   Mean   :0.11145   Mean   :0.09704  
##  3rd Qu.:2008   3rd Qu.: 0.050163   3rd Qu.:0.15666   3rd Qu.:0.12474  
##  Max.   :2009   Max.   : 0.149535   Max.   :0.24856   Max.   :0.19109


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
