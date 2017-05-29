## Este script sirve para representar el ciclo anual de los distintos puntos en los que están las estaciones bsrn

library(zoo)
library(lattice)

## MODELOS

load("../../../calc/points/fixed/bsrn_fixedCiclo_caer.Rdata")
load("../../../calc/points/fixed/bsrn_fixedCiclo_cno.Rdata")

## SATELITE

load("../../../calc/points/fixed/bsrn_fixedCiclo_sat.Rdata")

#########################################
## CARPENTRAS
########################################

load("../../../data/estaciones_data/carpentras20032009.Rdata")
carpentrasCiclo <- carpentras20032009[,2:13]
carpentrasBSRNciclo <- colMeans(carpentrasCiclo) 

## Para los modelos y el satélite son los datos de la fila 3

carpentrasCicloCAER <- bsrn_fixedCiclo_caer[3,]
carpentrasCicloCNO <- bsrn_fixedCiclo_cno[3,]
carpentrasCicloSAT <- bsrn_fixedCiclo_sat[3,]

carpentras <- cbind(carpentrasCicloSAT,carpentrasCicloCAER, carpentrasCicloCNO)
carpentras <- zoo(carpentras)

pdf('carpentraCiclofixed2.pdf')
xyplot(carpentras, lwd=3,superpose=TRUE, xlab='months', ylab='[kWh/kWp]')
dev.off()

## Represento la diferencia

carpentrasDif <- carpentras - carpentrasCicloSAT
carpentrasDif <- carpentrasDif[,2:3]
carpentrasDif <- carpentrasDif/carpentrasCicloSAT

pdf('carpentrasCicloreldif.pdf')
xyplot(carpentrasDif, lwd=2,superpose=TRUE, xlab='months')
dev.off()

############################################################
## SEDEBROKER
###########################################################

load("../../../data/estaciones_data/sedebroker20032009.Rdata")
sedebrokerCiclo <- sedebroker20032009[,2:13]
sedebrokerCiclo[sedebrokerCiclo[]==-999] <- NA
sedebrokerBSRNciclo <- colMeans(sedebrokerCiclo, na.rm=TRUE)

sedebrokerCAER <- bsrn_fixedCiclo_caer[7,]
sedebrokerCNO <- bsrn_fixedCiclo_cno[7,]
sedebrokerSAT <- bsrn_fixedCiclo_sat[7,]

sedebroker <- cbind(sedebrokerSAT, sedebrokerCAER, sedebrokerCNO)
sedebroker <- zoo(sedebroker)

pdf('sedebrokerCiclofixed2.pdf')
xyplot(sedebroker, lwd=3, superpose=TRUE, xlab='months', ylab='[kWh/kwp]')
dev.off()

## Represento la diferencia

sedebrokerDif <- sedebroker - sedebrokerSAT
sedebrokerDif <- sedebrokerDif[,2:3]
sedebrokerDif <- sedebrokerDif/sedebrokerSAT
 
pdf('sedebrokerCicloreldifixed.pdf')
xyplot(sedebrokerDif, superpose=TRUE, xlab='months')
dev.off()


############################################################
## PAYERNE
###########################################################

load("../../../data/estaciones_data/payerne20032009.Rdata")
payerneCiclo <- payerne20032009[,2:13]
payerneBSRNciclo <- colMeans(payerneCiclo) 

payerneCicloCAER <- bsrn_fixedCiclo_caer[6,]
payerneCicloCNO <- bsrn_fixedCiclo_cno[6,]
payerneCicloSAT <- bsrn_fixedCiclo_sat[6,]

payerne <- cbind(payerneCicloSAT, payerneCicloCAER, payerneCicloCNO)
payerne <- zoo(payerne)

pdf('payerneCiclofixed2.pdf')
xyplot(payerne, lwd=3, superpose=TRUE, xlab='months', ylab='[kWh/kWp]')
dev.off()

## Represento la diferencia

payerneDif <- payerne - payerneCicloSAT
payerneDif <- payerneDif[,2:3]
payerneDif <- payerneDif/payerneCicloSAT

pdf('payerneCicloreldiffixed.pdf')
xyplot(payerneDif, superpose=TRUE, xlab='month')
dev.off()
