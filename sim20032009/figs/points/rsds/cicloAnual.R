## Este script sirve para representar el ciclo anual de los distintos puntos en los que están las estaciones bsrn

library(zoo)

## MODELOS

load("../../calc/points/bsrn_rsdsCiclo_caer.Rdata")
load("../../calc/points/bsrn_rsdsCiclo_cno.Rdata")

## SATELITE

load("../../calc/points/bsrn_rsdsCiclo_sat.Rdata")

#########################################
## CARPENTRAS
########################################

load("../../data/estaciones_data/carpentras20032009.Rdata")
carpentrasCiclo <- carpentras20032009[,2:13]
carpentrasBSRNciclo <- colMeans(carpentrasCiclo) 

## Para los modelos y el satélite son los datos de la fila 3

carpentrasCicloCAER <- bsrn_rsdsCiclo_caer[3,]
carpentrasCicloCNO <- bsrn_rsdsCiclo_cno[3,]
carpentrasCicloSAT <- bsrn_rsdsCiclo_sat[3,]

carpentras <- cbind(carpentrasBSRNciclo, carpentrasCicloSAT, carpentrasCicloCAER, carpentrasCicloCNO)
carpentras <- zoo(carpentras)

pdf('carpentraCiclo.pdf')
xyplot(carpentras, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

## Represento la diferencia

carpentrasDif <- carpentras - carpentrasBSRNciclo
carpentrasDif <- carpentrasDif[,2:4]

pdf('carpentrasCiclodif.pdf')
xyplot(carpentrasDif, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

############################################################
## SEDEBROKER
###########################################################

load("../../data/estaciones_data/sedebroker20032009.Rdata")
sedebrokerCiclo <- sedebroker20032009[,2:13]
sedebrokerCiclo[sedebrokerCiclo[]==-999] <- NA
sedebrokerBSRNciclo <- colMeans(sedebrokerCiclo, na.rm=TRUE)

sedebrokerCAER <- bsrn_rsdsCiclo_caer[7,]
sedebrokerCNO <- bsrn_rsdsCiclo_cno[7,]
sedebrokerSAT <- bsrn_rsdsCiclo_sat[7,]

sedebroker <- cbind(sedebrokerBSRNciclo, sedebrokerSAT, sedebrokerCAER, sedebrokerCNO)
sedebroker <- zoo(sedebroker)

pdf('sedebrokerCiclo.pdf')
xyplot(sedebroker, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

## Represento la diferencia

sedebrokerDif <- sedebroker - sedebrokerBSRNciclo
sedebrokerDif <- sedebrokerDif[,2:4]

pdf('sedebrokerCiclodif.pdf')
xyplot(sedebrokerDif, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()


############################################################
## PYRENE
###########################################################

load("../../data/estaciones_data/payerne20032009.Rdata")
payerneCiclo <- payerne20032009[,2:13]
payerneBSRNciclo <- colMeans(payerneCiclo) 

payerneCicloCAER <- bsrn_rsdsCiclo_caer[6,]
payerneCicloCNO <- bsrn_rsdsCiclo_cno[6,]
payerneCicloSAT <- bsrn_rsdsCiclo_sat[6,]

payerne <- cbind(payerneBSRNciclo, payerneCicloSAT, payerneCicloCAER, payerneCicloCNO)
payerne <- zoo(payerne)

pdf('payerneCiclo.pdf')
xyplot(payerne, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

## Represento la diferencia

payerneDif <- payerne - payerneBSRNciclo
payerneDif <- payerneDif[,2:4]

pdf('payerneCiclodif.pdf')
xyplot(payerneDif, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()
