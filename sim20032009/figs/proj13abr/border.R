## Creo el objeto que delimita las fronteras. Este objeto está creado con la mascara.

crs.lonlat <- CRS("+proj=longlat +datum=WGS84")

ext <- as.vector(extent(projectExtent(mascara, crs.lonlat)))
boundaries <- map('world', fill=TRUE, exact=FALSE, xlim=ext[1:2], ylim= ext[3:4], plot=FALSE)

IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
boundaries_sp<- map2SpatialPolygons(boundaries, IDs=IDs, proj4string=crs.lonlat)

boundaries_lcc <- spTransform(boundaries_sp, mycrs)

border <- as(boundaries_lcc, 'SpatialLines') 

## para crear la máscara de países:

country  <- rasterize(boundaries_lcc, mascara)
