## Este script sive para representar las series temporales de la media anual de radiación en los distintos puntos donde están las estaciones de bsrn.

library(zoo)

## ESTACIONES

## MODELOS

load("../../../calc/points/rsds/srn_rsdsY_caer.Rdata")
load("../../../calc/points/rsds/bsrn_rsdsY_cno.Rdata")

## SATELITE

load("../../../calc/points/rsds/bsrn_rsdsY_sat.Rdata")

#########################################
## CARPENTRAS
########################################

load("../../data/estaciones_data/carpentras20032009.Rdata")
carpentrasYmean <- carpentras20032009[,14]
carpentrasBSRN <- carpentrasYmean 

## Para los modelos y el satélite son los datos de la fila 3

carpentrasCAER <- bsrn_rsdsY_caer[3,]
carpentrasCNO <- bsrn_rsdsY_cno[3,]
carpentrasSAT <- bsrn_rsdsY_sat[3,]

carpentras <- cbind(carpentrasBSRN, carpentrasSAT, carpentrasCAER, carpentrasCNO)
carpentras <- zoo(carpentras)

pdf('carpentraY.pdf')
xyplot(carpentras, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

## Represento la diferencia

carpentrasDif <- carpentras - carpentrasBSRN
carpentrasDif <- carpentrasDif[,2:4]

pdf('carpentraYdif.pdf')
xyplot(carpentrasDif, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

############################################################
## SEDEBROKER
###########################################################

## En este caso el problema es que para algunos años, faltan meses de la serie temporal. Haré la media anual eliminando estos meses, aunque estos años serán poco realistas. 

load("../../data/estaciones_data/sedebroker20032009.Rdata")

sedebroker20032009[sedebroker20032009[]==-999] <- NA
sedebroker20032009m <- sedebroker20032009[,2:13]
sedebrokerYmean <- rowMeans(sedebroker20032009m, na.rm=TRUE)
sedebrokerBSRN <- sedebrokerYmean
 
sedebrokerCAER <- bsrn_rsdsY_caer[7,]
sedebrokerCNO <- bsrn_rsdsY_cno[7,]
sedebrokerSAT <- bsrn_rsdsY_sat[7,]

sedebroker <- cbind(sedebrokerBSRN, sedebrokerSAT, sedebrokerCAER, sedebrokerCNO)
sedebroker <- zoo(sedebroker)

pdf('sedebrokerY.pdf')
xyplot(sedebroker, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()

## Como resultado vemos que para los primeros años donde faltan datos, los valores de la radiación son mucho más bajos, puesto que estoy haciendo la media con menos valores y en muchos casos elminando el valor más alto.

############################################################
## PAYERNE
###########################################################

load("../../data/estaciones_data/payerne20032009.Rdata")
payerneYmean <- payerne20032009[,14]
payerneBSRN <- payerneYmean 

payerneCAER <- bsrn_rsdsY_caer[6,]
payerneCNO <- bsrn_rsdsY_cno[6,]
payerneSAT <- bsrn_rsdsY_sat[6,]

payerne <- cbind(payerneBSRN, payerneSAT, payerneCAER, payerneCNO)
payerne <- zoo(payerne)

pdf('payerneY.pdf')
xyplot(payerne, superpose=TRUE, xlab='Years', ylab='[W/m2]')
dev.off()
