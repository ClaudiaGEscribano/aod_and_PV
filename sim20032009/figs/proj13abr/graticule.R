## lat lon lines
library(graticule)

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

lons <- seq(-20, 50, by=10)
lats <- seq(25, 55, by=5)

## optionally, specify the extents of the meridians and parallels
## here we push them out a little on each side
xl <-  range(lons) + c(-0.4, 0.4)
yl <- range(lats) + c(-0.4, 0.4)
## build the lines with our precise locations and ranges
grat <- graticule(lons, lats, proj = mycrs,
                  xlim = xl, ylim = yl)
## Labels
labs <- graticule_labels(lons, lats,
                            xline = lons[2],
                            yline = lats[2],
                            proj = mycrs)

labsLon <- labs[labs$islon,]
labsLat <- labs[!labs$islon,]

###########
