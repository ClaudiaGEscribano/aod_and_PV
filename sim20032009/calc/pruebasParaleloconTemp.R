library(raster)
library(maps)
library(maptools)
library(parallel)
library(solaR)

## Calculate the annual productivity using parallel function
source('fooProd.R')
##########################################################################
## 1. DATA
#########################################################################

#load('/home/variabilidad/boundaries.Rdata')
#load('/home/claudia/variabilidad/linea.Rdata')

## Datos de radiación y temperatura de 30 años

#SISS <- brick('/home/claudia/clusters/SIS_cmsaf30_Wh')
#Tas <- brick('/home/claudia/productividad/conTemperatura/temperatura005_ECAD.nc')

SISS <- brick('../data/C-AER/rsds_day_20032009.nc')
SISS <- SISS*24
Tas <- brick('../data/C-AER/tas_day_20032009.nc')

## Time index
tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
names(SISS) <- tt
names(Tas) <- tt

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)

## latitude values as a new raster
y <- init(SISS, v='y')

########################################################################
## 2. ProdCGPV Yearly productivity
#######################################################################

## Productividad anual. Dependiendo del tipo de seguidor en la función fooProd dentro de fooParallel tendré que cambiar el modeTrk de prodCGPV
modeTrk <- 'fixed'

## Parallel blocks configuration
blocks <- 6
nodes <- detectCores()
bs <- blockSize(SISS, minblocks=blocks*nodes)
## List with the indices of blocks for each node
iCluster <- splitIndices(bs$n, nodes)
 
resCl <- mclapply(iCluster,
                  ## Each node receives an element of iCluster, a set of indices
                  function(icl){
                      resList <- lapply(icl, function(i){
                          ## An element of icl is the number of block to
                          ## be read by each node
                          vG0 <- getValues(SISS, bs$row[i], bs$nrows[i])
                          vTa <- getValues(Tas, bs$row[i], bs$nrow[i])                             
                          lat <- getValues(y, bs$row[i], bs$nrows[i])
                          vals <- cbind(lat, vG0, vTa)
                          cat(i, ':', range(lat), '\n')
                          res0 <- try(apply(vals, MARGIN=1L,
                                            FUN=fooProd,
                                            modeTrk = modeTrk))
                          cat(i, ':', range(res0), '\n')
                          if (inherits(res0, 'try-error')) res0 <- NA
                          else  do.call(cbind, res0)
                      })
                      do.call(cbind, resList)
                  },
                  mc.cores = nodes)
## The result of mclapply is a list with as many elements as nodes
resCl <- do.call(cbind, resCl)  
resCl <- t(resCl)
##nYears <- ncol(resCl)

out <- brick(SISS) # nl = nYears) 
out <- setValues(out, resCl)

##year <- function(x) as.numeric(format(x, '%Y'))
##out <- setZ(out, unique(year(tt)))
##names(out) <- unique(year(tt))

writeRaster(out, filename='fixed_yearlyProd_temp_20032009.grd', overwrite=TRUE)


