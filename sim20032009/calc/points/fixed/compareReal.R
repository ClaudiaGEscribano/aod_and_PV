library(raster)
library(rasterVis)
library(zoo)

## This script will compare real PV data with the simulatios of model with/without aerosols

## ## load the real data ## ##
##############################

## datos 2003
##load("../photocampaYf.Rdata")

## datos 2003-2005
load("../photocampa.Rdata")

## simulados
##load("photocampaSimYf_month.Rdata")

lat <- 41.1
lon <- 1.19

## agrego los datos diarios por medias mensuales de energía diaria
photocampaMon <- aggregate(photocampa$Yf, by=as.yearmon, 'mean', na.rm=TRUE) 
## para comprobar si faltan días en el mes
p <-aggregate(photocampa$Yf, by=as.yearmon, FUN=function(x) length(x))
## Para los meses en los que hay Nan en algún inversor, no hago la media, sino que tomo el único valor que hay.

## La energía Yf es la que tengo que comparar con los datos simulados. Algunos de los valores menores de lo esperado debido a paradas por mantenimiento, mejora etc. Por ello, podemos simular con la Gefectiva para compara con la simulada con datos del modelo.


## extract at the point in the stack ##
#######################################

## Las simulaciones generales tienen un sistema que no se corresponde con la planta real, es genérico. Si se quiere, se puede extraer la información de un raster en una latitud concreta con el código a continuación, pero para una comparación hay que simular la generación en un putno con las características del sistema concreto. Los resultados de esto es la salida de pvpoint.R

## 1. Hay que asignar al raster la proyección que le corresponde. Para CAER es LCC, para el satélite es long/lat.
 
mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")

## mascara data to project pvoutput data

mascara <- raster("../../../figs/masque_terre_mer.nc", varname='zon_new')
maslat <- raster("../../../figs/masque_terre_mer.nc", varname='lat')
maslon <- raster("../../../figs/masque_terre_mer.nc", varname='lon')

pmaslat <- rasterToPoints(maslat)
pmaslon <- rasterToPoints(maslon)
maslonlat <- cbind(pmaslon[,3], pmaslat[,3])
 
# Specify the lonlat as spatial points with projection as long/lat
maslonlat <- SpatialPoints(maslonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
pmaslonlat <- spTransform(maslonlat, CRSobj = mycrs)

projection(mascara) <- mycrs
extent(mascara) <- extent(pmaslonlat)

#######
## CAER
#######
 
fixed <- stack("../../proj12abr/fixed_caer_monthlyProd_temp_20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
fixed <- setZ(fixed, idx)
 
## defino el raster del modelo bien:

projection(fixed) <- projection(mascara)
extent(fixed) <- extent(mascara)

## Hago las medias anuales de la simulación C-AER

fixedMeses <- zApply(fixed, by=as.yearmon, fun='mean')

## EXTRAER AQUI

## Para extraer los puntos del modelo necesito un Spatialdata con la lat y la lon primero.
## Ahora proyecto los puntos que quiero extraer a LCC

bsrnlonlat <- SpatialPoints(cbind(lon,lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
bsrnlonlat <- spTransform(bsrnlonlat, mycrs)

fixedMeses_caer <- extract(fixedMeses, bsrnlonlat, method="simple")

photocampa_fixedMeses_caer <- as.zoo(t(fixedMeses_caer), as.yearmon(idx))

##############################################################################################

## Se puede extraer el valor del AOD en ese punto concreto

######### AOD ##########
#######################

aod <- stack("../../AOD_total_monthly20032009.grd")
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'month')
aod <- setZ(aod, idx)

## defino el raster de aod bien:

projection(aod) <- projection(mascara)
aod <- crop(aod, mascara)
extent(aod) <- extent(mascara)

aod <- extract(aod, bsrnlonlat, method="simple")
photocampa_aod <- as.zoo(t(aod), as.yearmon(idx))

##_______________________________________________________________________________________
## ESTE CÓDIGO NO SE USA:
#c <- merge(photocampaMon, photocampa_fixedMeses_sat, photocampa_fixedMeses_caer, photocampa_fixedMeses_cno, photocampa_aod, all=FALSE) 
#names(c) <- c("REAL", "SAT","CAER", "CNO", "AOD")

#c2 <- c[-18,]
  
#pdf("seriesPhotocampaAOD.pdf")
#xyplot(c,screens=c(1,1,1,1,2),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
#dev.off()

##  con los dias que hay por mes:
 
#dias <- p[,1]

#c <- merge(photocampaMon, photocampa_fixedMeses_sat, photocampa_fixedMeses_caer, photocampa_fixedMeses_cno, photocampa_aod, dias, all=FALSE) 
#names(c) <- c("REAL", "SAT","CAER", "CNO", "AOD", "Days")

#pdf("seriesPhotocampaAOD3.pdf")
#xyplot(c,screens=c(1,1,1,1,2,3),scales = list(x = list(at = index(c), rot=45)), type='b', superpose=TRUE)
#dev.off()

##______________________________________________________________________________________-

## 2. COMPARACIÓN CON LA SIMULACIÓN CON DATOS DEL pv system CORRECTOS

## comparación con una simulacion de CAER con los datos del sistema corregidos################

tt <- seq(as.Date('2003-01-01'), as.Date('2009-12-31'), 'month')
 
xProd <- as.zoo(xProd, order.by=as.yearmon(tt))
xProdno <- as.zoo(xProdno, order.by=as.yearmon(tt))
xProdsat <- as.zoo(xProdMsat, order.by=as.yearmon(tt))
 
photocampa_fixedMeses_caer <- xProd
photocampa_fixedMeses_no <- xProdno
photocampa_fixed_sat <- xProdsat
 
c <- merge(photocampaMon, photocampa_fixedMeses_caer, photocampa_fixedMeses_no, photocampa_fixed_sat, all=FALSE)#, photocampa_fixed_sat)dias, photocampa_aod, all=FALSE)
#c <- c[which(index(c) >= "ene 2003" & index(c) <= "dic 2003")]
names(c) <- c("REAL", "CAER", "CNO", "SAT")
#names(c) <- c("REAL", "CAER_SIS", "CAER_GEN", "DAYS", "AOD") 

myTheme <- custom.theme.2()
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) 

pdf("seriesPhotocampaALL.pdf")
xyplot(c,screens=c(1,1,1), scales = list(x = list(at = index(c), rot=45)),
       grid=TRUE,  par.settings=myTheme, type='b', superpose=TRUE)
dev.off()


pdf("pruebascatterFixed.pdf")
xyplot(CAER+CNO+SAT~REAL, data=c2, type='p', ylab='MODEL', xlab='REAL', cex=1.3 ,par.settings=myTheme,superpose=TRUE, grid=TRUE, auto.key=TRUE, xlim=c(1,6), ylim=c(1,6),
           panel = function(...) {
               panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()


## root mean squared error and mean absoulte error

rmse <- sqrt( mean( (c$CAER - c$REAL)^2, na.rm = TRUE) )
## 0.8215362

rmseNO <- sqrt( mean( (c$CNO - c$REAL)^2, na.rm = TRUE) ) 
## 0.9933572

mae <- mean(c$CAER - c$REAL)
## 0.6805526
maeNO <- mean(c$CNO - c$REAL)
## 0.87685

## Elimino los datos de producción reales por debajo de 1

d2 <- d[-15,]
c2 <- c[-15,]

pdf("modelsreal2.pdf")
xyplot(d2$REAL~d2$CAER+d2$CNO, xlab='REAL', ylab='models')
dev.off()

rmse <- sqrt( mean( (c2$CAER - c2$REAL)^2, na.rm = TRUE) )
##  0.7471841

rmseNO <- sqrt( mean( (c2$CNO - c2$REAL)^2, na.rm = TRUE) ) 
## 0.934339

rmseSAT <- sqrt( mean( (c2$SAT - c2$REAL)^2, na.rm = TRUE) )
##  0.7946017


mae <- mean(c2$CAER - c2$REAL)
## 0.6246872
maeNO <- mean(c2$CNO - c2$REAL)
## 0.8278974
maeSAT <- mean(c2$SAT-c2$REAL)
## 0.699568


## DIFERENCIAS ##

err <- cbind(c$CAER-c$REAL, c$CNO-c$REAL)
names(err) <- c("CAER", "CNO")

rerr <- cbind((c$CAER-c$REAL)/c$REAL, (c$CNO-c$REAL)/c$REAL)
names(rerr) <- c("CAER", "CNO")

myTheme <- custom.theme.2()
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) 

pdf("PhotocampaDiferencias2.pdf")
xyplot(err, scales = list(x = list(at = index(c), rot=45)), type='p', ylab='kWh/m²', par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
                panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()


err2 <- cbind(c2$CAER-c2$REAL, c2$CNO-c$REAL)
names(err2) <- c("CAER", "CNO")

rerr2 <- cbind((c2$CAER-c2$REAL)/c2$REAL, (c2$CNO-c2$REAL)/c2$REAL)
names(rerr2) <- c("CAER", "CNO")

pdf("PhotocampaDiferencias3.pdf")
xyplot(err2, scales = list(x = list(at = index(c2), rot=45)), type='p', ylab='kWh/m²', par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
                panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()


err3 <- cbind(c2$CAER-c2$REAL, c2$CNO-c2$REAL, c2$SAT-c2$REAL)
names(err3) <- c("CAER", "CNO", "SAT")

## summary(err3)
##      Index           CAER              CNO              SAT         
##  Min.   :2003   Min.   :-0.1725   Min.   :0.1975   Min.   :0.08237  
##  1st Qu.:2003   1st Qu.: 0.4932   1st Qu.:0.6363   1st Qu.:0.44773  
##  Median :2004   Median : 0.6453   Median :0.7749   Median :0.70663  
##  Mean   :2004   Mean   : 0.6247   Mean   :0.8279   Mean   :0.69957  
##  3rd Qu.:2004   3rd Qu.: 0.7331   3rd Qu.:1.0222   3rd Qu.:0.95380  
## Max.   :2005   Max.   : 1.5825   Max.   :1.7752   Max.   :1.70121

pdf("PhotocampaDiferenciasabsolutas.pdf")
xyplot(err3, scales = list(x = list(at = index(c2), rot=45)), type='b', ylab='kWh/m²', par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
                panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()

dferr2 <- melt(as.data.frame(err3))

pdf("pruebasvisFixed.pdf")
xyplot(value~variable, groups=variable,data=dferr2, type='p', ylab='Error kWh/kWp', xlab='model', cex=1.3 ,par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
               panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()




## summary(err3)
##      Index           CAER              CNO              SAT         
##  Min.   :2003   Min.   :-0.1725   Min.   :0.1975   Min.   :0.08237  
##  1st Qu.:2003   1st Qu.: 0.4932   1st Qu.:0.6363   1st Qu.:0.44773  
##  Median :2004   Median : 0.6453   Median :0.7749   Median :0.70663  
##  Mean   :2004   Mean   : 0.6247   Mean   :0.8279   Mean   :0.69957  
##  3rd Qu.:2004   3rd Qu.: 0.7331   3rd Qu.:1.0222   3rd Qu.:0.95380  
##  Max.   :2005   Max.   : 1.5825   Max.   :1.7752   Max.   :1.70121

rerr3 <- cbind((c2$CAER-c2$REAL)/c2$REAL, (c2$CNO-c2$REAL)/c2$REAL)
names(rerr2) <- c("CAER", "CNO")

rerr4 <- cbind((c2$CAER-c2$REAL)/c2$REAL, (c2$CNO-c2$REAL)/c2$REAL, (c2$SAT-c2$REAL)/c2$REAL)
names(rerr4) <- c("CAER", "CNO", "SAT")

## summary(rerr4)
##      Index           CAER               CNO               SAT         
##  Min.   :2003   Min.   :-0.05591   Min.   :0.08489   Min.   :0.04146  
##  1st Qu.:2003   1st Qu.: 0.17601   1st Qu.:0.21357   1st Qu.:0.14831  
##  Median :2004   Median : 0.19306   Median :0.30280   Median :0.25638  
##  Mean   :2004   Mean   : 0.23887   Mean   :0.31254   Mean   :0.27753  
##  3rd Qu.:2004   3rd Qu.: 0.35160   3rd Qu.:0.40555   3rd Qu.:0.33426  
##  Max.   :2005   Max.   : 0.51921   Max.   :0.58243   Max.   :0.60075


## summary(err2)
##      Index           CAER              CNO        
##  Min.   :2003   Min.   :-0.1725   Min.   :0.1975  
##  1st Qu.:2003   1st Qu.: 0.4932   1st Qu.:0.6363  
##  Median :2004   Median : 0.6453   Median :0.7749  
##  Mean   :2004   Mean   : 0.6247   Mean   :0.8279  
##  3rd Qu.:2004   3rd Qu.: 0.7331   3rd Qu.:1.0222  
##  Max.   :2005   Max.   : 1.5825   Max.   :1.7752

## summary(rerr2)
##      Index           CAER               CNO         
##  Min.   :2003   Min.   :-0.05591   Min.   :0.08489  
##  1st Qu.:2003   1st Qu.: 0.17601   1st Qu.:0.21357  
##  Median :2004   Median : 0.19306   Median :0.30280  
##  Mean   :2004   Mean   : 0.23887   Mean   :0.31254  
##  3rd Qu.:2004   3rd Qu.: 0.35160   3rd Qu.:0.40555  
##  Max.   :2005   Max.   : 0.51921   Max.   :0.58243  


## summary(rerr)
##      Index           CAER                CNO           
##  Min.   :2003   Min.   : -0.05591   Min.   :  0.08489  
##  1st Qu.:2003   1st Qu.:  0.17673   1st Qu.:  0.22019  
##  Median :2004   Median :  0.20478   Median :  0.30306  
##  Mean   :2004   Mean   :  7.48121   Mean   :  7.90140  
##  3rd Qu.:2004   3rd Qu.:  0.37431   3rd Qu.:  0.46659  
##  Max.   :2005   Max.   :130.60100   Max.   :136.91210  

## Nos quedamos con el valor de la mediana. 0.2 y 0.3 en cada caso. (20% y 30%)

## Elimino el valo de diciembre de 2004 porque está claro que lo obtenido es inferior de lo que debería obtenerse teóricamente.
err2 <- c[-15,]

rerr2 <- cbind((err2$CAER-err2$REAL)/err2$REAL, (err2$CNO-err2$REAL)/err2$REAL)
names(rerr2) <- c("CAER", "CNO")

## summary(rerr2)
##      Index           CAER               CNO         
##  Min.   :2003   Min.   :-0.05591   Min.   :0.08489  
##  1st Qu.:2003   1st Qu.: 0.17601   1st Qu.:0.21357  
##  Median :2004   Median : 0.19306   Median :0.30280  
##  Mean   :2004   Mean   : 0.23887   Mean   :0.31254  
##  3rd Qu.:2004   3rd Qu.: 0.35160   3rd Qu.:0.40555  
##  Max.   :2005   Max.   : 0.51921   Max.   :0.58243

## Si elimino los dos otros dos puntos sospechosos: julio y agosto de 2003

err3 <- c[-7,]
err3 <- err3[-7,]
err3 <- err3[-13,]

rerr3 <- cbind((err3$CAER-err3$REAL)/err3$REAL, (err3$CNO-err3$REAL)/err3$REAL)
names(rerr3) <- c("CAER", "CNO")

## summary(rerr3)
##      Index           CAER               CNO         
##  Min.   :2003   Min.   :-0.05591   Min.   :0.08489  
##  1st Qu.:2003   1st Qu.: 0.16977   1st Qu.:0.20707  
##  Median :2004   Median : 0.19026   Median :0.28138  
##  Mean   :2004   Mean   : 0.20944   Mean   :0.28292  
##  3rd Qu.:2004   3rd Qu.: 0.24789   3rd Qu.:0.33559  
##  Max.   :2005   Max.   : 0.46005   Max.   :0.49792

## La mejora es muy notable.
 
rmse <- sqrt( mean( (err3$CAER - err3$REAL)^2, na.rm = TRUE) )
## 0.5764781

rmseNO <- sqrt( mean( (err3$CNO - err3$REAL)^2, na.rm = TRUE) )
## 0.7626082
 

mae <- mean(err3$CAER - err3$REAL)
## 0.5081601
maeNO <- mean(err3$CNO - err3$REAL)
## 0.7051249

err3 <- cbind(err3$CAER-err3$REAL, err3$CNO-err3$REAL)
names(err3) <- c("CAER", "CNO")

myTheme <- custom.theme.2()
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) 

pdf("PhotocampaDiferencias3.pdf")
xyplot(err3, scales = list(x = list(at = index(err3), rot=45)), type='p', ylab='kWh/m^2', par.settings=myTheme,superpose=TRUE, grid=TRUE,
           panel = function(...) {
                panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()

###############################
## agregar por meses

month <- function(x) format(as.Date(x, '%m'))

monPhoto <- format(as.Date(index(photocampa)), '%m')
