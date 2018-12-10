## este script es para comparar las salidas de productividad anual/mensual en función de la representación de la temperatura.

library(raster)

## 1. comparación de las salidas de productividad para Tmean y Tciclo.

## Se analiza si hay cambio entre hacer un ciclo diario de temperatura o poner la temperatura media diaria.

## con temperatura media diaria:

fixed_tmean_mon<- stack("/home/claudia/aod_and_PV/sim20032009/calc/proj12abr/outputTmean/fixed_caer_monthlyProd_temp_20032009.grd")
fixed_tmean_year <- stack("/home/claudia/aod_and_PV/sim20032009/calc/proj12abr/outputTmean/fixed_caer_yearlyProd_temp_20032009.grd")

## con temperatura diurna:

fixed_tday_mon <- stack("/home/claudia/aod_and_PV/sim20032009/calc/proj12abr/outputTciclo/fixed_caer_monthlyProd_tday_20032009.grd")

fixed_tday_year <- stack("/home/claudia/aod_and_PV/sim20032009/calc/proj12abr/outputTciclo/fixed_caer_yearlyProd_tday_20032009.grd")

## DIFERENCIA ABSOLUTA
fixed_dif_year <- fixed_tmean_year - fixed_tday_year
writeRaster(fixed_dif_year, filename='fixed_difference_temperature_caer.grd')

fixed_dif_mon <- fixed_tmean_mon - fixed_tday_mon
writeRaster(fixed_dif_mon, filename='fixed_difference_temperature_mon_caer.grd')

## DIFERENCIA RELATIVA

fixed_dif_year_relative <- (fixed_tmean_year - fixed_tday_year)/fixed_tday_year
writeRaster(fixed_dif_year_relative, filename='fixed_difference_relative_temperature_caer.grd', overwrite=TRUE)

fixed_dif_mon_relative <- (fixed_tmean_mon - fixed_tday_mon)/fixed_tday_mon
writeRaster(fixed_dif_mon_relative, filename='fixed_difference_relative_temperature_mon_caer.grd', overwrite=TRUE)

##############################################
## 2. Se compara entre incluir la radiación en el plano horizontal o en el plano del generador.

