## Calculo de AOD por zonas

library(raster)

## MASCARA

zonas <- raster("zonas.grd")

## AOD ##

## cargo los datos anuales, mensuales y ciclo

AOD_yearly <- stack("../AOD_total_yearly20032009.grd")
AOD_monthly <- stack("../AOD_total_monthly20032009.grd")
AOD_ciclo <- stack("../AOD_total_ciclo20032009.grd")



