## pruebas con las climatologias de aerosoles para hacer cluster con las wavelets. Pruebo con los datos de la malla LCC y sólo para representar los proyectos.

library(raster)
library(rasterVis)
library(waveslim)
library(zoo)

###############################################
## Sin proyectar a lat lon el nc primero.

wave2cluster <- function(x) {
    data <- as.data.frame(x)

    dataZoo <- zoo(t(data))

    waveDecomposition <- apply(dataZoo, 2, FUN=function(x) modwt(x, n.levels=6))
    waveVariance <- lapply(waveDecomposition, FUN=function(x) wave.variance(x))

    features <- lapply(1:ncol(dataZoo), FUN=function(x)
                                 waveVariance[[x]]$wavevar[-7]/var(dataZoo[,x]))
    return(features)
}


## bc

updir <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc"
direc <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc/calc"

listFich <- dir(path="/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc", pattern='\\.nc')
setwd(updir)

bc <- stack(listFich, varname='aero')
setwd(direc)

listafeatures_bc<- wave2cluster(bc)

bc_features <- do.call(rbind, listafeatures)
colnames(bc_features) <- c("f1","f2","f3", "f4","f5", "f6")
save(bc_features, file='bc_features.Rdata')

## sd

updir <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/sd"
direc <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc/calc"

listFich <- dir(path="/disco2/aerosoles_DATA/climatologia_AOD/AOD/sd", pattern='\\.nc')
setwd(updir)

sd <- stack(listFich, varname='aero')
setwd(direc)

listafeatures_sd<- wave2cluster(sd)

sd_features <- do.call(rbind, listafeatures_sd)
colnames(sd_features) <- c("f1","f2","f3", "f4","f5", "f6")
save(sd_features, file='sd_features.Rdata')

## or


updir <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/or"
direc <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc/calc"

listFich <- dir(path="/disco2/aerosoles_DATA/climatologia_AOD/AOD/or", pattern='\\.nc')
setwd(updir)

or <- stack(listFich, varname='aero')
setwd(direc)

listafeatures_or<- wave2cluster(or)

or_features <- do.call(rbind, listafeatures_or)
colnames(or_features) <- c("f1","f2","f3", "f4","f5", "f6")
save(or_features, file='or_features.Rdata')

## ss

updir <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/ss"
direc <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc/calc"

listFich <- dir(path="/disco2/aerosoles_DATA/climatologia_AOD/AOD/ss", pattern="^macc_regcm_2.*\\.nc$")
setwd(updir)

ss <- stack(listFich, varname='aero')
setwd(direc)

listafeatures_ss<- wave2cluster(ss)

ss_features <- do.call(rbind, listafeatures_ss)
colnames(ss_features) <- c("f1","f2","f3", "f4","f5", "f6")
save(ss_features, file='ss_features.Rdata')

## su

updir <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/su"
direc <- "/disco2/aerosoles_DATA/climatologia_AOD/AOD/bc/calc"

listFich <- dir(path="/disco2/aerosoles_DATA/climatologia_AOD/AOD/su", pattern="\\.nc")
setwd(updir)

su <- stack(listFich, varname='aero')
setwd(direc)

listafeatures_su<- wave2cluster(su)

su_features <- do.call(rbind, listafeatures_su)
colnames(su_features) <- c("f1","f2","f3", "f4","f5", "f6")
save(su_features, file='su_features.Rdata')
############################################################################

