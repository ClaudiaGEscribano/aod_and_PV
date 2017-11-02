library(raster)
library(maps)
library(maptools)
library(parallel)
library(solaR)
 
## Calculat productivity using parallel function
source('fooGef.R')

##########################################################################
## 1. DATA
#########################################################################

## TOMO LOS DATOS DE LOS MODELOS SIN PROYECTAR PERO EXTRAIGO LA INFORMACIÓN DE LA LATITUD.

## Cambio de ficheros dependiendo de si la radiación la obtengo de la simulación CAER, CNO o del satélite

SISS <- brick('../../data/C-AER/rsds_day_20032009.nc')
##SISS <- brick('../../data/SAT/SISdm20032009eur.nc', varname='SIS') 

#SISS <- brick('/home/claudia/clusters/SIS_cmsaf30_Wh')
#Tas <- brick('/home/claudia/productividad/conTemperatura/temperatura005_ECAD.nc')

##SISS <- brick('rsds_no_day_20032009_proj.grd')
SISS <- SISS*24

#Tas <- brick('../../data/TEMP/tg_0.44deg_20032009.nc')
Tas <- brick('../../data/C-AER/tas_day_20032009.nc')

## Time index
tt <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
names(SISS) <- tt
names(Tas) <- tt

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)

##Tas[!is.na(Tas)] <- 25
## ## Crop objects
## e <- extent(-5, 5, 55, 72)
## SISS <- crop(SISS, e)
## Tas <- crop(Tas, e)

## ## Subset time period
## SISS <- subset(SISS, 1:365)
## Tas <- subset(Tas, 1:365)
## tt <- tt[1:365]

## latitude values as a new raster
y <- raster("../../data/C-AER/rsds_day_20032009.nc", varname='lat') #Esto es para los modelos que tienen estos datos
##y <- init(SISS, v='y')

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
                          cat('Lat: ', i, ':', range(lat), '\n')
                          res0 <- try(apply(vals, MARGIN=1L,
                                            FUN=fooGef,
                                            modeTrk = modeTrk))
                          if (!inherits(res0, 'try-error')) 
                          {
                              res0 <- do.call(cbind, res0)
                              cat('Res: ', i, ':', range(res0), '\n')
                          } else
                          {
                              res0 <- NA
                              cat("Res:", i, ": NA\n")
                          }
                          res0
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

writeRaster(out, filename='Gef_fixed_caer_yearlyProd_20032009.grd', overwrite=TRUE)


