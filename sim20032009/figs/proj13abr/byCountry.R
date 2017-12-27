## Agrego por países los resultados.

source('projectInfo.R')
source('graticule.R')
source('border.R')

library(reshape2)
## DATA ##

## Datos de diferencias relativas del scri plotMap.R

all <- stack(fixedDJF,oneDJF,twoDJF,fixedMAM,oneMAM,twoMAM,fixedJJA,oneJJA,twoJJA,fixedSON,oneSON,twoSON)

## by country usando zonal

fixedDJFbyc <- zonal(fixedDJF, country, 'mean', na.rm=TRUE)
fixedDJFbyc <- as.data.frame(fixedDJFbyc)

## o

byCountry <- extract(all, boundaries_lcc, fun=mean, na.rm=TRUE, df=TRUE)
row.names(byCountry) <- names(boundaries_lcc)
 
##

fixedDJFbyc <- zonal(fixedDJF, country, 'mean', na.rm=TRUE)

##

num <- seq(length(byCountry$ID))
for (i in num){
country[which(getValues(country)== i )] <- byCountry[i,2]}

my.at <- seq(-0.25, 0, 0.02)

pdf("useTday/SON_two_bycountry.pdf", width=7, height=4)
levelplot(country, scales=list(draw=FALSE), margin=FALSE,colorkey=list(space='right', title='kWh/kWp'), at=my.at)+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()


####

cells <- as.data.frame(country)
cells$mean <- cells[]
fixedDJFbyc[[which(fixedDJFbyc$zone ==1), 2]]

##

DJF <- stack(fixedDJF, oneDJF, twoDJF)
byCountryDJF <- extract(DJF, boundaries_lcc, fun=mean, na.rm=TRUE, df=TRUE)
names(byCountryDJF) <- c("COUNTRY","FIXED", "ONE", "TWO")
byCountryDJF$COUNTRY <- names(boundaries_lcc)
byCountryDJF$SEASON <- "DJF"
DJFbyc <- melt(byCountryDJF)

MAM <- stack(fixedMAM, oneMAM, twoMAM)
byCountryMAM <- extract(MAM, boundaries_lcc, fun=mean, na.rm=TRUE, df=TRUE)
names(byCountryMAM) <- c("COUNTRY","FIXED", "ONE", "TWO")
byCountryMAM$COUNTRY <- names(boundaries_lcc)
byCountryMAM$SEASON <- "MAM"
MAMbyc <-melt(byCountryMAM)

JJA <- stack(fixedJJA, oneJJA, twoJJA)
byCountryJJA <- extract(JJA, boundaries_lcc, fun=mean, na.rm=TRUE, df=TRUE)
names(byCountryJJA) <- c("COUNTRY","FIXED", "ONE", "TWO")
byCountryJJA$COUNTRY <- names(boundaries_lcc)
byCountryJJA$SEASON <- "JJA"
JJAbyc <- melt(byCountryJJA)

SON <- stack(fixedSON, oneSON, twoSON)
byCountrySON <- extract(SON, boundaries_lcc, fun=mean, na.rm=TRUE, df=TRUE)
names(byCountrySON) <- c("COUNTRY","FIXED", "ONE", "TWO")
byCountrySON$COUNTRY <- names(boundaries_lcc)
byCountrySON$SEASON <- "SON"
SONbyc <- melt(byCountrySON)

seasonByC <- rbind(DJFbyc, MAMbyc, JJAbyc, SONbyc)

myTheme <- custom.theme.2(pch = 19, cex =0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
myTheme$superpose.symbol$pch <-c(20,8,5) 
myTheme$dot.symbol$lwd <- 2
myTheme$plot.symbol$lwd <- 2
 
pdf("pruebadotplot3.pdf", width=7, height=7)
dotplot(COUNTRY~value|SEASON, group=variable, data=seasonByC, layout=c(4,1), par.settings=myTheme, scales=list(x=list(rot=45, cex=1), y=list(cex=0.6)), auto.key=TRUE, lwd=2)
dev.off()

myTheme$superpose.symbol$pch <-1:3
scales=list(y=list(rot=45), x=list(rot=45))

##row.names(byCountry) <- names(boundaries_lcc)

## elimino los que no tienen valores
 
ind <- which(is.na(seasonByC[,4]))
s <- seasonByC[-ind, ]

pdf("pruebadotplot6.pdf", width=7, height=7)
dotplot(value~COUNTRY|SEASON, group=variable, data=s,layout=c(1,4), par.settings=myTheme, scales=list(x=list(rot=90, cex=0.6), y=list(cex=0.6)), as.table=TRUE, auto.key=TRUE, lwd=2)
dev.off()
