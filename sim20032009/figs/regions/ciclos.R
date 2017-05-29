library(zoo)
library(lattice)
library(reshape2)

## DIFERENCIAS RELATIVAS RADIACION ZONAS CON EL SATÉLITE.

load("../../calc/regions/sat_rsds_cicloMean_zones.Rdata")
load("../../calc/regions/caer_rsds_cicloMean_zones.Rdata")
load("../../calc/regions/cno_rsds_cicloMean_zones.Rdata")

load("../../calc/regions/sat_fixed_cicloMean_zones.Rdata")
load("../../calc/regions/caer_fixed_cicloMean_zones.Rdata")
load("../../calc/regions/cno_fixed_cicloMean_zones.Rdata")

load("../../calc/regions/sat_one_cicloMean_zones.Rdata")
load("../../calc/regions/caer_one_cicloMean_zones.Rdata")
load("../../calc/regions/cno_one_cicloMean_zones.Rdata")

load("../../calc/regions/sat_two_cicloMean_zones.Rdata")
load("../../calc/regions/caer_two_cicloMean_zones.Rdata")
load("../../calc/regions/cno_two_cicloMean_zones.Rdata")

## plot rsds for 3 sources of data

sat <- sat_rsds_cicloMean_zones
sat <- as.data.frame(sat)
caer <- caer_rsds_cicloMean_zones
caer <- as.data.frame(caer)
cno <- cno_rsds_cicloMean_zones
cno <- as.data.frame(cno)

names(sat) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(caer) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(cno) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")

sat <- melt(sat, id.vars='zonas')
caer <- melt(caer, id.vars='zonas')
cno <- melt(cno, id.vars='zonas')

sat$data <- "sat"
caer$data <-"caer"
cno$data <- "cno"

rsds <- rbind(sat,caer,cno)
 
rsds$zonas <- rep(c("AFRW","AFRE", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),36)
names(rsds) <- c("zonas", "month", "rsds", "data")

xyplot(rsds~month|as.factor(zonas), group=data, data=rsds, type='l', lwd=2, scales=list(rot=45),auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
               panel.xyplot(...)
       }
)

## FIXED PRODUCTIVITY ##

satF <- sat_fixed_cicloMean_zones
satF <- as.data.frame(satF)
caerF <- caer_fixed_cicloMean_zones
caerF <- as.data.frame(caerF)
cnoF <- cno_fixed_cicloMean_zones
cnoF <- as.data.frame(cnoF)

names(satF) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(caerF) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(cnoF) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")

satF <- melt(satF, id.vars='zonas')
caerF <- melt(caerF, id.vars='zonas')
cnoF <- melt(cnoF, id.vars='zonas')

satF$data <- "sat"
caerF$data <-"caer"
cnoF$data <- "cno"

fixed <- rbind(satF,caerF,cnoF)

fixed$zonas <- rep(c("AFRW","AFRE", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),36)
names(fixed) <- c("zonas", "month", "fixed", "data")

pdf("ciclosAnualsFIXED.pdf")
xyplot(fixed~month|as.factor(zonas), group=data, data=fixed, type='l', lwd=2, scales=list(rot=45),auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
 
## ONE PRODUCTIVITY ##
 
satO <- sat_one_cicloMean_zones
satO <- as.data.frame(satO)
caerO <- caer_one_cicloMean_zones
caerO <- as.data.frame(caerO)
cnoO <- cno_one_cicloMean_zones
cnoO <- as.data.frame(cnoO)

names(satO) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(caerO) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(cnoO) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")

satO <- melt(satO, id.vars='zonas')
caerO <- melt(caerO, id.vars='zonas')
cnoO <- melt(cnoO, id.vars='zonas')

satO$data <- "sat"
caerO$data <-"caer"
cnoO$data <- "cno"

one <- rbind(satO,caerO,cnoO)

one$zonas <- rep(c("AFRW","AFRE", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),36)
names(one) <- c("zonas", "month", "one", "data")

pdf("ciclosAnualsONE.pdf")
xyplot(one~month|as.factor(zonas), group=data, data=one, type='l', lwd=2, scales=list(rot=45),auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
## TWO PRODUCTIVITY ##
  
satT <- sat_two_cicloMean_zones
satT <- as.data.frame(satT)
caerT <- caer_two_cicloMean_zones
caerT <- as.data.frame(caerT)
cnoT <- cno_two_cicloMean_zones
cnoT <- as.data.frame(cnoT)
 
names(satT) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(caerT) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(cnoT) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")

satT <- melt(satT, id.vars='zonas')
caerT <- melt(caerT, id.vars='zonas')
cnoT <- melt(cnoT, id.vars='zonas')

satT$data <- "sat"
caerT$data <-"caer"
cnoT$data <- "cno"

two <- rbind(satT,caerT,cnoT)

two$zonas <- rep(c("AFRW","AFRE", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),36)
names(two) <- c("zonas", "month", "two", "data")

pdf("ciclosAnualsTWO.pdf")
xyplot(two~month|as.factor(zonas), group=data, data=two, type='l', lwd=2, scales=list(rot=45),auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

## DIFERENCIAS ENTRE LAS DOS SIMULACIONES:

caerF$type <- 'fixed'
caerO$type <- 'one'
caerT$type <- 'two'

caerF$value <- (caerF$value - cnoF$value)/cnoF$value
caerO$value <- (caerO$value - cnoO$value)/cnoO$value
caerT$value <- (caerT$value - cnoT$value)/cnoT$value

modelsDif <- rbind(caerF, caerO, caerT)

modelsDif$zonas <- rep(c("AFRW","AFRE", "EMED", "EURS", "EURW","CNEUR","NEEUR","BISL"),36)
names(modelsDif) <- c("zonas", "month", "rel.dif", "model", "type")

pdf("ciclosAnualsMODELS.pdf")
xyplot(rel.dif~month|as.factor(zonas), group=type, data=modelsDif, type='l', lwd=2, scales=list(rot=45),auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

