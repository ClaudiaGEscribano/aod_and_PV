## Este script sive para representar las series temporales de la media anual de radiación en los distintos puntos donde están las estaciones de bsrn.

library(zoo)
library(lattice)

## DIFERENCIAS RELATIVAS RADIACION ZONAS CON EL SATÉLITE.

load("../../calc/regions/Dif_rel_caer_sat_zonas.Rdata")
load("../../calc/regions/Dif_rel_cno_sat_zonas.Rdata")

library(reshape2)

caer <- melt(Dif_rel_caer_sat_zonas, id.vars='zonas')
caer$model <- rep("caer", length(caer[,1]))

names(caer) <- c("zonas", "year", "rsds", "model")

cno <- melt(Dif_rel_cno_sat_zonas, id.vars='zonas')
cno$model <- rep("cno", length(cno[,1]))

names(cno) <- c("zonas", "year", "rsds", "model")

rsds_dif <- rbind(caer,cno)
  
xyplot(rsds~year|as.factor(zonas), group=model, data=rsds_dif, type='l', lwd=3, auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
 
rsds_dif$zonas <- rep(c("AFRE","AFRW", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),14)

  
xyplot(rsds~year|as.factor(zonas), group=model,data=rsds_dif, type='l', lwd=3, auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

## DIFERENCIAS RELATIVAS EN PRODUCTIVIDAD

load("../../calc/regions/Dif_rel_fixed_caer_sat_zonas.Rdata")
load("../../calc/regions/Dif_rel_fixed_cno_sat_zonas.Rdata")

caerfixed <- melt(Dif_rel_fixed_caer_sat_zonas, id.vars='zonas')
caerfixed$model <- rep("caer", length(caer[,1]))

names(caerfixed) <- c("zonas", "year", "yield", "model")

cnofixed <- melt(Dif_rel_fixed_cno_sat_zonas, id.vars='zonas')
cnofixed$model <- rep("cno", length(cno[,1]))

names(cnofixed) <- c("zonas", "year", "yield", "model")

fixed_dif <- rbind(caerfixed,cnofixed)

fixed_dif$zonas <- rep(c("AFRE","AFRW", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),14)
 
xyplot(yield~year|as.factor(zonas), group=model, data=fixed_dif, type='l', lwd=3, ylab='rel.dif', scales=list(rot=45), auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

## Diferencias relativas en prod. one

load("../../calc/regions/Dif_one_caer_sat_zonas.Rdata")
load("../../calc/regions/Dif_one_cno_sat_zonas.Rdata")
load("../../calc/regions/sat_one_yearlyMean_zones.Rdata")

load("../../calc/regions/Dif_rel_one_caer_sat_zonas.Rdata")
load("../../calc/regions/Dif_rel_one_cno_sat_zonas.Rdata")

caerone <- melt(Dif_rel_one_caer_sat_zonas, id.vars='zonas')
caerone$model <- rep("caer", length(caerone[,1]))

names(caerone) <- c("zonas", "year", "yield", "model")

cnoone <- melt(Dif_rel_one_cno_sat_zonas, id.vars='zonas')
cnoone$model <- rep("cno", length(cnoone[,1]))

names(cnoone) <- c("zonas", "year", "yield", "model")

one_dif <- rbind(caerone,cnoone)

one_dif$zonas <- rep(c("AFRE","AFRW", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),14)

xyplot(yield~year|as.factor(zonas), group=model, data=one_dif, type='l', lwd=3, scales=list(rot=45), auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

## Diferencias relativas en prod two

load("../../calc/regions/Dif_two_caer_sat_zonas.Rdata")
load("../../calc/regions/Dif_two_cno_sat_zonas.Rdata")
load("../../calc/regions/sat_two_yearlyMean_zones.Rdata")

load("../../calc/regions/Dif_rel_two_caer_sat_zonas.Rdata")
load("../../calc/regions/Dif_rel_two_cno_sat_zonas.Rdata")

caertwo <- melt(Dif_rel_two_caer_sat_zonas, id.vars='zonas')
caertwo$model <- rep("caer", length(caertwo[,1]))

names(caertwo) <- c("zonas", "year", "yield", "model")

cnotwo <- melt(Dif_rel_two_cno_sat_zonas, id.vars='zonas')
cnotwo$model <- rep("cno", length(cnotwo[,1]))

names(cnotwo) <- c("zonas", "year", "yield", "model")

two_dif <- rbind(caertwo,cnotwo)

two_dif$zonas <- rep(c("AFRE","AFRW", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),14)

xyplot(yield~year|as.factor(zonas), group=model, data=two_dif, type='l', lwd=3, scales=list(rot=45), auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

## Diferencias relativas de CAER_ rsds y fixed-two-one

names(Dif_rel_caer_sat_zonas) <- names(Dif_rel_fixed_caer_sat_zonas)

 
dif_rsds <- melt(Dif_rel_caer_sat_zonas, id.vars='zonas')
dif_fixed <- melt(Dif_rel_fixed_caer_sat_zonas, id.vars='zonas')
dif_one <- melt(Dif_rel_one_caer_sat_zonas, id.vars='zonas')
dif_two <- melt(Dif_rel_two_caer_sat_zonas, id.vars='zonas')

dif_fixed$var <- rep("fixed", length(dif_rsds[,1]))
dif_rsds$var <- rep("rsds", length(dif_rsds[,1]))
dif_one$var <- rep("one", length(dif_rsds[,1]))
dif_two$var <- rep("two", length(dif_rsds[,1]))

names(dif_fixed) <- c("zonas", "year", "rel.dif", "var")
names(dif_rsds) <- c("zonas", "year", "rel.dif", "var")
names(dif_one) <- c("zonas", "year", "rel.dif", "var")
names(dif_two) <- c("zonas", "year", "rel.dif", "var")

rel_dif <- rbind(dif_rsds, dif_fixed, dif_one, dif_two)

rel_dif$zonas <- rep(c("AFRE","AFRW", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),14)

pdf("rel_dif_rsds_fixed2.pdf") 
xyplot(rel.dif~year|as.factor(zonas), group=var, data=rel_dif, type='l', lwd=1.5, scales=list(rot=45), auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

## DIFERENCIAS ENTRE LAS DOS SIMULACIONES

load("../../calc/regions/Dif_rel_fixed_caer_cno_zonas.Rdata")
load("../../calc/regions/Dif_rel_one_sat_cno_zonas.Rdata")##esta guardado con nombre incorrecto, en realidad es caer_cno
load("../../calc/regions/Dif_rel_two_caer_cno_zonas.Rdata")

dif_fixed <- melt(Dif_rel_fixed_caer_cno_zonas, id.vars='zonas')
dif_one <- melt(Dif_rel_one_caer_cno_zonas, id.vars='zonas')
dif_two <- melt(Dif_rel_two_caer_cno_zonas, id.vars='zonas')

dif_fixed$var <- rep("fixed", length(dif_fixed[,1]))
dif_one$var <- rep("one", length(dif_one[,1]))
dif_two$var <- rep("two", length(dif_two[,1]))

names(dif_fixed) <- c("zonas", "year", "rel.dif", "var")
names(dif_one) <- c("zonas", "year", "rel.dif", "var")
names(dif_two) <- c("zonas", "year", "rel.dif", "var")

rel_dif <- rbind(dif_fixed, dif_one, dif_two)

rel_dif$zonas <- rep(c("AFRE","AFRW", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),21)
 
pdf("rel_dif_types_caer_cno.pdf") 
xyplot(rel.dif~year|as.factor(zonas), group=var, data=rel_dif, type='l', lwd=2, scales=list(rot=45), auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
