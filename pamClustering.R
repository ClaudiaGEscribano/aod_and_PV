## This script do the clustering with the pam algorithm.

wheredata <- "../data/"
wherecalc <- "../aod_and_PV/"

setwd(wheredata)
load("or_features.Rdata")
load("ss_features.Rdata")
load("bc_features.Rdata")
load("su_features.Rdata")
load("sd_features.Rdata")

setwd(wherecalc)

## library of clustering

library(cluster)

pamx <- pam(or_features, k=20, diss=FALSE)

features <- cbind(or_features, ss_features)

clarax <- clara(or_features, 20)

claraClusters <- clarax$clustering


## trying the kmeans:

kmeansexp <- function(x, n, k){
    km <- lapply(seq(1:k),
                 FUN=function(i) kmeans(x, i, nstart=n, iter.max=100))
    return(km)
}

a <- kmeansexp(su_features, 10, 30)


## la informacion de los clusters hay que llevarla a un raster para representarla espacialmente.
library(raster)
library(rasterVis)

setwd(wheredata)
r <- raster("macc_regcm_2003_01_bc.nc")
setwd(wherecalc)
P <- setValues(r, claraClusters)

pdf("clustersprueba.pdf")
levelplot(P)
dev.off()



