## Este script sive para representar las series temporales de la media anual de radiación en los distintos puntos donde están las estaciones de bsrn.

library(zoo)

## MODELOS
 
load("../../../calc/points/fixed/bsrn_fixedY_caer.Rdata")
load("../../../calc/points/fixed/bsrn_fixedY_cno.Rdata")

## SATELITE

load("../../../calc/points/fixed/bsrn_fixedY_sat.Rdata")

#########################################
## CARPENTRAS
########################################

load("../../../data/estaciones_data/carpentras20032009.Rdata")
carpentrasYmean <- carpentras20032009[,14]
carpentrasBSRN <- carpentrasYmean 

## Para los modelos y el satélite son los datos de la fila 3

carpentrasCAER <- bsrn_fixedY_caer[3,]
carpentrasCNO <- bsrn_fixedY_cno[3,]
carpentrasSAT <- bsrn_fixedY_sat[3,]

carpentras <- cbind(carpentrasSAT, carpentrasCAER, carpentrasCNO)
carpentras <- zoo(carpentras)

pdf('carpentraYfixed2.pdf')
xyplot(carpentras, lwd=3,superpose=TRUE, xlab='Years', ylab='kWh/kWp')
dev.off()

## Represento la diferencia

carpentrasDif <- carpentras - carpentrasSAT
carpentrasDif <- carpentrasDif[,2:3]
carpentrasDif <- carpentrasDif/carpentrasSAT

pdf('carpentraYreldifFixed.pdf')
xyplot(carpentrasDif, lwd=2, superpose=TRUE, xlab='Years', ylab='Rel. Dif')
dev.off()

############################################################
## SEDEBROKER
###########################################################

## En este caso el problema es que para algunos años, faltan meses de la serie temporal. Haré la media anual eliminando estos meses, aunque estos años serán poco realistas. 

load("../../../data/estaciones_data/sedebroker20032009.Rdata")

sedebroker20032009[sedebroker20032009[]==-999] <- NA
sedebroker20032009m <- sedebroker20032009[,2:13]
sedebrokerYmean <- rowMeans(sedebroker20032009m, na.rm=TRUE)
sedebrokerBSRN <- sedebrokerYmean
 
sedebrokerCAER <- bsrn_fixedY_caer[7,]
sedebrokerCNO <- bsrn_fixedY_cno[7,]
sedebrokerSAT <- bsrn_fixedY_sat[7,]

sedebroker <- cbind(sedebrokerSAT, sedebrokerCAER, sedebrokerCNO)
sedebroker <- zoo(sedebroker)

pdf('sedebrokerYfixed2.pdf')
xyplot(sedebroker, lwd=3, superpose=TRUE, xlab='Years', ylab='[kWh/kWP]')
dev.off()

sedebrokerDif <- sedebroker - sedebrokerSAT
sedebrokerDif <- sedebrokerDif[,2:3]
sedebrokerDif <- sedebrokerDif/sedebrokerSAT

pdf('sedebrokerYreldifFixed.pdf')
xyplot(sedebrokerDif, lwd=2, superpose=TRUE, xlab='Years', ylab='Rel. Dif')
dev.off()

############################################################
## PAYERNE
###########################################################

load("../../../data/estaciones_data/payerne20032009.Rdata")
payerneYmean <- payerne20032009[,14]
payerneBSRN <- payerneYmean 

payerneCAER <- bsrn_fixedY_caer[6,]
payerneCNO <- bsrn_fixedY_cno[6,]
payerneSAT <- bsrn_fixedY_sat[6,]

payerne <- cbind(payerneSAT, payerneCAER, payerneCNO)
payerne <- zoo(payerne)
names(payerne) <- c("PyreneSAT", "PyreneCAER", "PYRENEcno")
 
pdf('payerneYfixed2.pdf')
xyplot(payerne, lwd=3,superpose=TRUE, xlab='Years', ylab='[kWh/kWp]')
dev.off()

pyreneDif <- payerne - payerneSAT
pyreneDif <- pyreneDif[,2:3]
pyreneDif <- pyreneDif/payerneSAT

pdf('payerneYreldifFixed.pdf')
xyplot(pyreneDif, lwd=2, superpose=TRUE, xlab='Years', ylab='Rel. Dif')
dev.off()
