library(zoo)
library(latticeExtra)
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
aer <- caer_rsds_cicloMean_zones
aer <- as.data.frame(aer)
no_aer<- cno_rsds_cicloMean_zones
no_aer<- as.data.frame(no_aer)

names(sat) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(aer) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
names(no_aer) <- c("zonas","Jan","Feb", "Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")

## si quiero representar diferencias utilizo este código, si no, salto hasta después de ##

aer_sat <- aer-sat
aer_sat$zonas <- sat$zonas
no_sat <- no_aer-sat
no_sat$zonas <- sat$zonas

aer <- melt(aer_sat, id.vars='zonas')
noaer <- melt(no_sat, id.vars='zonas')

aer$data <- "aer"
noaer$data <-"no-aer"

ssr <- rbind(aer, noaer)

ssr$zonas <- rep(c("AFRE","AFRW", "MEDE", "EURS", "EURW","EURC","EURNE","BISL"),24)
#rep(c("1.AFRW","2.AFRE", "3.MIDE", "5.EURS", "6.EURW","7.EURC","4.EURE","8.BISL"),36)
names(ssr) <- c("zonas", "month", "ssr", "data")

pdf("diferencia_mesesSSR.pdf", width=7, height=5)
xyplot(ssr~month|as.factor(zonas), group=data, data=ssr, type=c('o','l'), lwd=2, scales=list(x=list(rot=45, cex=0.7)),ylab=list(label='SSR difference [W/m^2]', cex=0.75), par.settings=myTheme, grid=TRUE, layout=c(4,2),auto.key=TRUE, aspect=2/3)
dev.off()

###########################################



sat <- melt(sat, id.vars='zonas')
aer <- melt(aer, id.vars='zonas')
no_aer<- melt(no_aer, id.vars='zonas')

sat$data <- "sat"
aer$data <-"aer"
no_aer$data <- "no-aer"

rsds <- rbind(sat,aer,no_aer)
 
rsds$zonas <-  rep(c("AFRE","AFRW", "MEDE", "EURS", "EURW","EURC","EURNE","BISL"),36) 
names(rsds) <- c("zonas", "month", "SSR", "data")

myTheme <- custom.theme.2(cex=0.5, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20) #,8,5) 

pdf("dif_model_sat_cicloAnualzonas.pdf", width=7, height=4)  
xyplot(SSR~month|as.factor(zonas), group=data,data=rsds, type=c('o','l'), scales=list(x=list(rot=45, cex=0.5)),lwd=1.5, auto.key=TRUE, grid=TRUE, layout=c(4,2), aspect=2/3, par.settings=myTheme,ylab='SSR[W/m^2]',
    panel = function(...) {
      #  panel.grid()#col="grey", lwd=0.1, h=5, v=0)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)
dev.off()



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

## Si quiero representar diferencias utilizo este trozo de codifo, si no, salto al siguiente

caer_sat <- caerF-satF
caer_sat$zonas <- satF$zonas
cno_sat <- cnoF-satF
cno_sat$zonas <- satF$zonas

aer <- melt(caer_sat, id.vars='zonas')
noaer <- melt(cno_sat, id.vars='zonas')

aer$data <- "aer"
noaer$data <-"no-aer"

fixed <- rbind(aer, noaer)

fixed$zonas <- rep(c("AFRE","AFRW", "MEDE", "EURS", "EURW","EURC","EURNE","BISL"),24)
#rep(c("1.AFRW","2.AFRE", "3.MIDE", "5.EURS", "6.EURW","7.EURC","4.EURE","8.BISL"),36)
names(fixed) <- c("zonas", "month", "fixed", "data")

pdf("diferencia_mesesFIXED.pdf", width=7, height=5)
xyplot(fixed~month|as.factor(zonas), group=data, data=fixed, type=c('o','l'), lwd=2, scales=list(x=list(rot=45, cex=0.7)),ylab=list(label='daily productivity difference [kWh/kWp]', cex=0.75), par.settings=myTheme, grid=TRUE, layout=c(4,2),auto.key=TRUE, aspect=2/3)

dev.off()

############################################

satF <- melt(satF, id.vars='zonas')
caerF <- melt(caerF, id.vars='zonas')
cnoF <- melt(cnoF, id.vars='zonas')

satF$data <- "sat"
caerF$data <-"aer"
cnoF$data <- "no-aer"

fixed <- rbind(satF,caerF,cnoF)

fixed$zonas <- rep(c("AFRE","AFRW", "MEDE", "EURS", "EURW","EURC","EURNE","BISL"),36)
#rep(c("1.AFRW","2.AFRE", "3.MIDE", "5.EURS", "6.EURW","7.EURC","4.EURE","8.BISL"),36)
names(fixed) <- c("zonas", "month", "fixed", "data")

pdf("ciclosAnualsFIXED.pdf", width=7, height=5)
xyplot(fixed~month|as.factor(zonas), group=data, data=fixed, type=c('o','l'), lwd=2, scales=list(x=list(rot=45, cex=0.5)),ylab='fixed productivity [kWh/kWp]', par.settings=myTheme, grid=TRUE, layout=c(4,2),auto.key=TRUE, aspect=2/3)

dev.off()

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

one$zonas <- rep(c("AFRW","AFRE", "MIDE", "EURW", "EURS","NEUR","EURE","BISL"),36)
names(one) <- c("zonas", "month", "one", "data")

pdf("ciclosAnualsONE.pdf")
xyplot(one~month|as.factor(zonas), group=data, data=one, type='l', ylab='one [kWh/kWp]',lwd=2, scales=list(rot=45),auto.key=TRUE,
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

two$zonas <- rep(c("AFRW","AFRE", "MIDE", "EURS", "EURW","EURC","EURE","BISL"),36)
names(two) <- c("zonas", "month", "two", "data")

pdf("ciclosAnualsTWO.pdf")
xyplot(two~month|as.factor(zonas), group=data, data=two, type='l', ylab='two [kWh/kWp]',lwd=2, scales=list(rot=45),auto.key=TRUE,
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

modelsDif$zonas <- rep(c("AFRW","AFRE", "MIDE", "EURW", "EURS","EURC","EURE","BISL"),36)
names(modelsDif) <- c("zonas", "month", "rel.dif", "model", "type")

pdf("ciclosAnualsMODELS.pdf")
xyplot(rel.dif~month|as.factor(zonas), group=type, data=modelsDif, type='l', lwd=2, scales=list(rot=45),auto.key=TRUE,
    panel = function(...) {
        panel.grid(col="grey", lwd=0.1)
        panel.abline(h=0, col='black', lwd=1)
               panel.xyplot(...)
       }
)

