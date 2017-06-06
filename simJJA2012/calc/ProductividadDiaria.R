## Daily productivity with intradaily solar radiation data

library(raster)
library(solaR)
library(parallel)

## load the data

SISS <-  stack("../data/PROG/PROG_rsds_3h_jja2012.nc")
SISS <- SISS*3

Tas <- stack("../data/PROG/PROG_tas_3h_jja2012.nc")

tt <- seq(as.POSIXct("2012-06-01 03:00"),as.POSIXct("2012-09-01 00:00"), 10800) ## 108000 son los segundos que tienen 3 horas.

## SOURCE ##

source('fooProd.R')

## Datos de latitud

lat <- raster("../data/PROG/PROG_rsds_3h_jja2012.nc", varname='latitude')

SISS <- setZ(SISS, tt)
Tas <- setZ(Tas, tt)
 
#iCell <- 10000
#xG <- SIS[iCell]
#xT <- Tas[iCell]
#lat <- lat[iCell]
#xx <- c(lat, xG, xT)

modeTrk <- 'fixed'

## Parallel configuration ##

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
                          lat <- getValues(lat, bs$row[i], bs$nrows[i])
                          vals <- cbind(lat, vG0, vTa)
                          cat(i, ':', range(lat), '\n')
                          res0 <- try(apply(vals, MARGIN=1L,
                                            FUN=fooProd,
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

writeRaster(out, filemane='fixed_prog_dailyProd_temp_jja2012.grd', overwrite=TRUE)

