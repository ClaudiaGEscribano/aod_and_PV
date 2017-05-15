library(raster)

##bc <- stack("../data/AOD/aod_bc_monthly20032009.grd")
##su <- stack("../data/AOD/aod_su_monthly20032009.grd")
##ss <- stack("../data/AOD/aod_ss_monthly20032009.grd")
##sd <- stack("../data/AOD/aod_sd_monthly20032009.grd")
##or <- stack("../data/AOD/aod_or_monthly20032009.grd")

bc <- stack("../data/AOD/macc_regcm_20032009_bc.nc", varname='aero')
su <- stack("../data/AOD/macc_regcm_20032009_su.nc", varname='aero')
ss <- stack("../data/AOD/macc_regcm_20032009_ss.nc", varname='aero')
sd <- stack("../data/AOD/macc_regcm_20032009_sd.nc", varname='aero')
or <- stack("../data/AOD/macc_regcm_20032009_bc.nc", varname='aero')


idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), "month")
idx2 <- seq(1:84)

bc <- setZ(bc, idx2)
su <- setZ(su, idx2)
ss <- setZ(ss, idx2)
sd <- setZ(sd, idx2)
or <- setZ(or, idx2)

aod_all <- stack(bc, su, ss, sd, or)
idx3 <- rep(seq(1:84), 5)
aod_all <- setZ(aod_all, idx3)

suma_aod <- zApply(aod_all, by=idx3, fun='sum')

## lat lon de AOD

aodlat <- raster("../data/AOD/macc_regcm_20032009_bc.nc", varname='lat')
aodlon <- raster("../data/AOD/macc_regcm_20032009_bc.nc", varname='lon')

aodlat <- rasterToPoints(aodlat)
aodlon <- rasterToPoints(aodlon)
aodlonlat <- cbind(aodlon[,3], aodlat[,3])

# Specify the lonlat as spatial points with projection as long/lat
aodlonlat <- SpatialPoints(aodlonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

mycrs <- CRS("+proj=lcc +lat_1=43 +lat_2=43 +lat_0=43 +lon_0=15 +k=0.684241 +units=m +datum=WGS84 +no_defs")
paodlonlat <- spTransform(aodlonlat, CRSobj = mycrs)
                                       
paodlonlat 
extent(paodlonlat)

projection(suma_aod) <- mycrs
extent(suma_aod) <- extent(paodlonlat)

## mapa

crs.lonlat <- CRS("+proj=longlat +datum=WGS84")

ext <- as.vector(extent(projectExtent(suma_aod, crs.lonlat)))
boundaries <- map('worldHires', fill=TRUE, interior=FALSE,exact=FALSE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)

IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=crs.lonlat)

boundaries_lcc <- spTransform(boundaries_sp, mycrs)

border_aod <- as(boundaries_lcc, 'SpatialLines') ## no funciona

save(border_aod, file='border_aod.Rdata')

#########################


writeRaster(suma_aod, filename='AOD_total_monthly20032009.grd', overwrite=TRUE)

## Total yearly AOD

suma_aod <- setZ(suma_aod, idx)

year <- function(x) as.numeric(format(x, '%y'))
suma_aodY <- zApply(suma_aod, by=year, 'mean')

writeRaster(suma_aodY, filename='AOD_total_yearly20032009.grd', overwrite=TRUE)

## Anual cicle AOD

month <- function(x) as.numeric(format(x, '%m'))
suma_aodCiclo <- zApply(suma_aod, by=month, 'mean')
 
writeRaster(suma_aodCiclo, filename='AOD_total_ciclo20032009.grd', overwrite=TRUE)

## SEASONAL:
 
monthIndex <- function(k){
    c <- c(1:7)
    j <- k
    for (i in 1:7){
        c[i] <- j
        j <- j+12}
    return(c)
}

lista <- list()
for (i in 1:12) lista[[i]] <- monthIndex(i)

## lista con un rasterBrick por mes
Meses <- lapply(lista, FUN=function(x) subset(suma_aod, x))

enero <- Meses[[1]]
febrero <- Meses[[2]]
diciembre <- Meses[[12]]

DJF_aod<- stack(enero, febrero, diciembre)
DJF_aod<- mean(DJF_aod)
writeRaster(DJF_aod, filename='DJF_aod.grd')

marzo <- Meses[[3]]
abril <- Meses[[4]]
mayo <-Meses[[5]]
 
MAM_aod<- stack(marzo, abril, mayo)
MAM_aod<- mean(MAM_aod)
writeRaster(MAM_aod, filename='MAM_aod.grd')

junio <- Meses[[6]]
julio <- Meses[[7]]
agosto <- Meses[[8]]

JJA_aod<- stack(junio, julio, agosto)
JJA_aod<- mean(JJA_aod)
writeRaster(JJA_aod, filename='JJA_aod.grd')

septiembre <- Meses[[9]]
octubre <- Meses[[10]]
noviembre <- Meses[[11]]

SON_aod<- stack(septiembre, octubre, diciembre)
SON_aod<- mean(SON_aod)
writeRaster(SON_aod, filename='SON_aod.grd')
