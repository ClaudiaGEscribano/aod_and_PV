## Este script sirve para representar el ciclo anual de los distintos puntos en los que están las estaciones bsrn

library(zoo)
library(latticeExtra)
library(reshape2)

## MODELOS

load("../../../calc/points/rsds/bsrn_rsdsCiclo_caer.Rdata")
load("../../../calc/points/rsds/bsrn_rsdsCiclo_cno.Rdata")

load("../../../calc/points/rsds/bsrn_rsdsMon_caer.Rdata")
load("../../../calc/points/rsds/bsrn_rsdsMon_cno.Rdata")

## SATELITE

load("../../../calc/points/rsds/bsrn_rsdsCiclo_sat.Rdata")

load("../../../calc/points/rsds/bsrn_rsdsMon_sat.Rdata")

#########################################
## CARPENTRAS
########################################

## MESES
load("/home/datos/aod/sim20032009/data/estaciones_data/carpentras20032009.Rdata")

foo <- function(data, model1, model2, model3, numstation){
    dat <- data[2:13]
    station <- as.vector(t(dat))

    CAER <- model1[numstation,]
    CNO <- model2[numstation,]
    SAT <- model3[numstation,]
    
    names(station) <- names(CAER)
    M <- cbind(station, CAER, CNO, SAT)
    M <- melt(M)
    return(M)
}

Meses <- foo(carpentras20032009,bsrn_rsdsMon_caer, bsrn_rsdsMon_cno,bsrn_rsdsMon_sat, 3)

## cálculo de correlaciones y errores:

##rmse1 <- sqrt(mean((Meses[Meses$Var2 == 'CAER', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))
rmse1 <- sqrt(sum((Meses[Meses$Var2 == 'CAER',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'CAER', 3])) ## 16.59329

##rmse2 <- sqrt(mean((Meses[Meses$Var2 == 'CNO', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))
rmse2 <- sqrt(sum((Meses[Meses$Var2 == 'CNO',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'CNO', 3])) ## 27.26103

##rmse3 <- sqrt(mean((Meses[Meses$Var2 == 'SAT', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))
rmse3 <- sqrt(sum((Meses[Meses$Var2 == 'SAT',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'SAT', 3])) ## 6.378948

##----------
mae1 <- mean(abs(Meses[Meses$Var2 == 'CAER', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE) ##13.96777

mae2 <- mean(abs(Meses[Meses$Var2 == 'CNO', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE) ##24.49333

mae3 <- mean(abs(Meses[Meses$Var2 == 'SAT', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE) ## 5.154899
##-----------

cor1 <- cor(Meses[Meses$Var2 == 'CAER', 3],Meses[Meses$Var2 == 'station', 3])

cor2 <- cor(Meses[Meses$Var2 == 'CNO', 3],Meses[Meses$Var2 == 'station', 3])

cor3 <- cor(Meses[Meses$Var2 == 'SAT', 3],Meses[Meses$Var2 == 'station', 3])

##------------

sdr1 <- sd(Meses[Meses$Var2 == 'CAER', 3])/sd(Meses[Meses$Var2 == 'station', 3])
 
sdr2 <- sd(Meses[Meses$Var2 == 'CNO', 3])/sd(Meses[Meses$Var2 == 'station', 3])

sdr3 <- sd(Meses[Meses$Var2 == 'SAT', 3])/sd(Meses[Meses$Var2 == 'station', 3])

##----------------
 
#carpentras <- carpentras20032009[,-1]
#carpentras <- carpentras[,-13]
#station <- as.vector(t(carpentras))

#CAER <- bsrn_rsdsMon_caer[3,]
#CNO <- bsrn_rsdsMon_cno[3,]
#SAT <- bsrn_rsdsMon_sat[3,]

#names(station) <- names(CAER)
#Meses <- cbind(station, CAER, CNO, SAT)
#Meses <- melt(Meses)

## CICLO

load("/home/datos/aod/sim20032009/data/estaciones_data/carpentras20032009.Rdata")

fooC <- function(data, model1, model2, model3, numstation){
    dat <- data[2:13]
    dat[dat[] ==-999] <-  NA
    dat <- colMeans(dat, na.rm=TRUE)
    station <- as.vector(t(dat))

    CAER <- model1[numstation,]
    CNO <- model2[numstation,]
    SAT <- model3[numstation,]

    names(CAER) <- names(CNO) <- names(SAT) <- names(station) <- month.abb
    M <- cbind(station, CAER, CNO, SAT)
    M <- melt(M)
    return(M)
}

ciclo <- fooC(carpentras20032009, bsrn_rsdsCiclo_caer, bsrn_rsdsCiclo_cno, bsrn_rsdsCiclo_sat, 3)

#carpentras <- carpentras20032009[,2:13]
#carpentras <- colMeans(carpentras)
#station <- as.vector(t(carpentras))

#CAER <- bsrn_rsdsCiclo_caer[3,]
#CNO <- bsrn_rsdsCiclo_cno[3,]
#SAT <- bsrn_rsdsCiclo_sat[3,]

#names(CAER) <- names(CNO) <- names(SAT) <- names(station) <- month.abb

#ciclo <- cbind(station, CAER, CNO, SAT)
#ciclo <- melt(ciclo)

## PLOT
 
myTheme <- custom.theme.2(pch = 20, cex =0.5)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) #,8,5,10) 

pdf('CarpentrasCicloAnual.pdf')
xyplot(value~Var1, group=Var2, data=ciclo, type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

pdf("CarpentrasMeses.pdf")
xyplot(value~Var1, group=Var2 , data=Meses, scales=list(x=list(rot=90, cex=0.6)),type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()
 
## Represento la DIFERENCIA

## MESES

load("/home/datos/aod/sim20032009/data/estaciones_data/carpentras20032009.Rdata")
carpentras <- carpentras20032009[,-1]
carpentras <- carpentras[,-13]
station <- as.vector(t(carpentras))

aer <- bsrn_rsdsMon_caer[3,]
no_aer <- bsrn_rsdsMon_cno[3,]
sat <- bsrn_rsdsMon_sat[3,]

names(station) <- names(aer)
Meses <- cbind(station, aer, no_aer, sat)

Meses <- Meses-Meses[,1]
Meses <- Meses[, -1]
 

M2 <- zoo(Meses, order.by=index(Meses))
tt <- as.yearmon(seq(as.Date("2003-01-01"), as.Date("2009-12-01"), "month"))
index(M2) <- tt


#Meses2 <- as.zoo(Meses, order.by=Meses$Var1)
#M <- as.zoo(Meses, order.by=rownames(Meses))
#Meses2 <- Meses2[,-1]

    
## pdf("CarpentrasMesesDif.pdf")
## xyplot(value~Var1, group=Var2, data=Meses, scales=list(x=list(labels=zoo(Meses$Var1), cex=0.5, rot=45)),type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE, grid=TRUE)
## dev.off()
 
pdf("CarpentrasMesesDif.pdf", width=7, height=5)
xyplot(M2, superpose=TRUE, aspect=2/3,scales=list(x=list(cex=1), y=list(cex=1)),type=c('o','l'), xlab='date', ylab='SSR difference [W/m^2]', par.settings=myTheme, auto.key=TRUE, grid=TRUE, abline=list(h=0, col='black')
)

dev.off()

## CICLO

load("/home/datos/aod/sim20032009/data/estaciones_data/carpentras20032009.Rdata")
carpentras <- carpentras20032009[,2:13]
carpentras <- colMeans(carpentras)
station <- as.vector(t(carpentras))

CAER <- bsrn_rsdsCiclo_caer[3,]
CNO <- bsrn_rsdsCiclo_cno[3,]
SAT <- bsrn_rsdsCiclo_sat[3,]

names(CAER) <- names(CNO) <- names(SAT) <- names(station) <- month.abb

ciclo <- cbind(station, CAER, CNO, SAT)

ciclo <- ciclo-ciclo[,1]
ciclo <- ciclo[, -1]

ciclo <- melt(ciclo)
 
pdf('CarpentrasCicloAnualDif.pdf')
xyplot(value~Var1, group=Var2, data=ciclo, type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

############################################################
## SEDEBROKER
###########################################################

load("/home/datos/aod/sim20032009/data/estaciones_data/sedebroker20032009.Rdata")

Meses <- foo(sedebroker20032009, bsrn_rsdsMon_caer, bsrn_rsdsMon_cno, bsrn_rsdsMon_sat, 7)
ciclo <- fooC(sedebroker20032009, bsrn_rsdsCiclo_caer, bsrn_rsdsCiclo_cno, bsrn_rsdsCiclo_sat, 7)

Meses[Meses[]== -999] <- NA

pdf("sedebrokerMeses.pdf")
xyplot(value~Var1, group=Var2 , data=Meses, scales=list(x=list(rot=90, cex=0.6)),type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

pdf("sedebrokerCiclo.pdf")
xyplot(value~Var1, group=Var2 , data=ciclo,type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

## Represento la diferencia
 
sedebroker <- sedebroker20032009[,2:13]
sedebroker[sedebroker[] == -999] <- NA
station <- as.vector(t(sedebroker))

CAER <- bsrn_rsdsMon_caer[7,]
CNO <- bsrn_rsdsMon_cno[7,]
SAT <- bsrn_rsdsMon_sat[7,]

names(station) <- names(CAER)
Meses <- cbind(station, CAER, CNO, SAT)

Meses <- Meses-Meses[,1]
Meses <- Meses[, -1]

M2 <- zoo(Meses, order.by=index(Meses))
tt <- as.yearmon(seq(as.Date("2003-01-01"), as.Date("2009-12-01"), "month"))
index(M2) <- tt

names(M2) <- c("aer", "no-aer", "sat")
#Meses <-melt(Meses)

## pdf("SedebokerMesesDif.pdf")
## xyplot(value~Var1, group=Var2 , data=Meses, scales=list(x=list(rot=90, cex=0.6)),type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
## dev.off()

pdf("SedebokerMesesDif.pdf", width=7, height=5)
xyplot(M2, superpose=TRUE, aspect=2/3,scales=list(x=list(cex=1), y=list(cex=1)),type=c('o','l'), xlab='date', ylab='SSR difference [W/m^2]', par.settings=myTheme, auto.key=TRUE, grid=TRUE, abline=list(h=0, col='black')
)

dev.off()

sedeboker <- sedebroker20032009[,2:13]
sedeboker[sedeboker[] == -999] <- NA
sedeboker <- colMeans(sedeboker, na.rm=TRUE)
station <- as.vector(t(sedeboker))
 
CAER <- bsrn_rsdsCiclo_caer[7,]
CNO <- bsrn_rsdsCiclo_cno[7,]
SAT <- bsrn_rsdsCiclo_sat[7,]

names(CAER) <- names(CNO) <- names(SAT) <- names(station) <- month.abb

ciclo <- cbind(station, CAER, CNO, SAT)

ciclo <- ciclo-ciclo[,1]
ciclo <- ciclo[, -1]

ciclo <- melt(ciclo)
 
pdf('sedebokerCicloAnualDif.pdf')
xyplot(value~Var1, group=Var2, data=ciclo, type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()


## Cálculo de los estadísticos 

##rmse1 <- sqrt(mean((Meses[Meses$Var2 == 'CAER', 3]-Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)) ## 12.069
rmse1 <- sqrt(sum((Meses[Meses$Var2 == 'CAER',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'CAER', 3])) ## 423.4792

##rmse2 <- sqrt(mean((Meses[Meses$Var2 == 'CNO', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))## 13.19
rmse2 <- sqrt(sum((Meses[Meses$Var2 == 'CNO',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'CNO', 3])) ## 436.5937

##rmse3 <- sqrt(mean((Meses[Meses$Var2 == 'SAT', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE)) ## 12.24
rmse3 <- sqrt(sum((Meses[Meses$Var2 == 'SAT',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'SAT', 3])) ## 430.156

##----------
 
mae1 <- mean(abs(Meses[Meses$Var2 == 'CAER', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE)## 153.3061
 
mae2 <- mean(abs(Meses[Meses$Var2 == 'CNO', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE)## 174.0799

mae3 <- mean(abs(Meses[Meses$Var2 == 'SAT', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE)## 149.8499

##-----------

cor1 <- cor(Meses[Meses$Var2 == 'CAER', 3],Meses[Meses$Var2 == 'station', 3], use="complete.obs")

cor2 <- cor(Meses[Meses$Var2 == 'CNO', 3],Meses[Meses$Var2 == 'station', 3], use="complete.obs")

cor3 <- cor(Meses[Meses$Var2 == 'SAT', 3],Meses[Meses$Var2 == 'station', 3], use="complete.obs")

##------------

sdr1 <- sd(Meses[Meses$Var2 == 'CAER', 3], na.rm=TRUE)/sd(Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)
 
sdr2 <- sd(Meses[Meses$Var2 == 'CNO', 3], na.rm=TRUE)/sd(Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)

sdr3 <- sd(Meses[Meses$Var2 == 'SAT', 3], na.rm=TRUE)/sd(Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)

############################################################
## PYRENE
###########################################################

load("/home/datos/aod/sim20032009/data/estaciones_data/payerne20032009.Rdata")

Meses <- foo(payerne20032009, bsrn_rsdsMon_caer, bsrn_rsdsMon_cno, bsrn_rsdsMon_sat, 6)
ciclo <- fooC(payerne20032009, bsrn_rsdsCiclo_caer, bsrn_rsdsCiclo_cno, bsrn_rsdsCiclo_sat, 6)

Meses[Meses[]== -999] <- NA

pdf("payerneMeses.pdf")
xyplot(value~Var1, group=Var2 , data=Meses, scales=list(x=list(rot=90, cex=0.6)),type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

pdf("payerneCiclo.pdf")
xyplot(value~Var1, group=Var2 , data=ciclo,type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

## Represento la diferencia

payerne <- payerne20032009[,2:13]
payerne[payerne[] == -999] <- NA
station <- as.vector(t(payerne))

CAER <- bsrn_rsdsMon_caer[6,]
CNO <- bsrn_rsdsMon_cno[6,]
SAT <- bsrn_rsdsMon_sat[6,]

names(station) <- names(CAER)
Meses <- cbind(station, CAER, CNO, SAT)

Meses <- Meses-Meses[,1]
Meses <- Meses[, -1]

M2 <- zoo(Meses, order.by=index(Meses))
tt <- as.yearmon(seq(as.Date("2003-01-01"), as.Date("2009-12-01"), "month"))
index(M2) <- tt

names(M2) <- c("aer", "no-aer", "sat")
#Meses <-melt(Meses)

pdf("PayerneMesesDif.pdf")
xyplot(value~Var1, group=Var2 , data=Meses, scales=list(x=list(rot=90, cex=0.6)),type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

pdf("PayerneMesesDif.pdf", width=7, height=5)
xyplot(M2, superpose=TRUE, aspect=2/3,scales=list(x=list(cex=1), y=list(cex=1)),type=c('o','l'), xlab='date', ylab='SSR difference [W/m^2]', par.settings=myTheme, auto.key=TRUE, grid=TRUE, abline=list(h=0, col='black')
)
dev.off()


payerne <- payerne20032009[,2:13]
payerne[payerne[] == -999] <- NA
payerne <- colMeans(payerne, na.rm=TRUE)
station <- as.vector(t(payerne))

CAER <- bsrn_rsdsCiclo_caer[6,]
CNO <- bsrn_rsdsCiclo_cno[6,]
SAT <- bsrn_rsdsCiclo_sat[6,]

names(CAER) <- names(CNO) <- names(SAT) <- names(station) <- month.abb

ciclo <- cbind(station, CAER, CNO, SAT)

ciclo <- ciclo-ciclo[,1]
ciclo <- ciclo[, -1]

ciclo <- melt(ciclo)
 
pdf('payerneCicloAnualDif.pdf')
xyplot(value~Var1, group=Var2, data=ciclo, type=c('o','l'), xlab='month', ylab='[W/m2]', par.settings=myTheme, auto.key=TRUE)
dev.off()

## cálculo de los estadísticos:

##rmse1 <- sqrt(mean((Meses[Meses$Var2 == 'CAER', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))
rmse1 <- sqrt(sum((Meses[Meses$Var2 == 'CAER',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'CAER', 3])) ## 21.20609

##rmse2 <- sqrt(mean((Meses[Meses$Var2 == 'CNO', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))
rmse2 <- sqrt(sum((Meses[Meses$Var2 == 'CNO',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'CNO', 3])) ## 29.69635

##rmse3 <- sqrt(mean((Meses[Meses$Var2 == 'SAT', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE))
rmse3 <- sqrt(sum((Meses[Meses$Var2 == 'SAT',3] - Meses[Meses$Var2 == 'station', 3])^2)/length(Meses[Meses$Var2 == 'SAT', 3])) ## 7.362819

##----------
mae1 <- mean(abs(Meses[Meses$Var2 == 'CAER', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE) ## 18.79345

mae2 <- mean(abs(Meses[Meses$Var2 == 'CNO', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE) ##27.32191

mae3 <- mean(abs(Meses[Meses$Var2 == 'SAT', 3]-Meses[Meses$Var2 == 'station', 3]), na.rm=TRUE)## 6.425288
##-----------

cor1 <- cor(Meses[Meses$Var2 == 'CAER', 3],Meses[Meses$Var2 == 'station', 3], use="complete.obs")

cor2 <- cor(Meses[Meses$Var2 == 'CNO', 3],Meses[Meses$Var2 == 'station', 3], use="complete.obs")

cor3 <- cor(Meses[Meses$Var2 == 'SAT', 3],Meses[Meses$Var2 == 'station', 3], use="complete.obs")

##------------

sdr1 <- sd(Meses[Meses$Var2 == 'CAER', 3], na.rm=TRUE)/sd(Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)
 
sdr2 <- sd(Meses[Meses$Var2 == 'CNO', 3], na.rm=TRUE)/sd(Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)

sdr3 <- sd(Meses[Meses$Var2 == 'SAT', 3], na.rm=TRUE)/sd(Meses[Meses$Var2 == 'station', 3], na.rm=TRUE)
