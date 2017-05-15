## Calculo del coeficiente de variabilidad para todo el dominio. Comparacion con sat?

library(raster)

## variabilidad interanual rsds

#### sat ####

sat <- stack("../../data/SAT/SISdm20032009eur.nc", varname='SIS')
idx <- seq(as.Date("2003-01-01"), as.Date("2009-12-31"), 'day')
sat <- setZ(sat, idx)

year <- function(x) as.numeric(format(x, '%y'))
sat_rsds_yearly <- zApply(sat, by=year, fun='mean')

## RSDS
sat_rsds_yearly_cv <- cv(sat_rsds_yearly)
writeRaster(sat_rsds_yearly_cv, filename='sat_rsds_yearly_cv.grd', overwrite=TRUE)

## FIXED

fixedsat <- stack("../../calc/proj12abr/fixed_sat_yearlyProd_temp_20032009.grd")
sat_fixed_yearly_cv <- cv(fixedsat)
writeRaster(sat_fixed_yearly_cv, filename='sat_fixed_yearly_cv.grd')

## ONE

Onesat <- stack("../../calc/proj12abr/oneAxis_sat_yearlyProd_temp_20032009.grd")
sat_one_yearly_cv <- cv(Onesat)
writeRaster(sat_one_yearly_cv, filename='sat_one_yearly_cv.grd')


## TWO

Twosat <- stack("../../calc/proj12abr/twoAxes_sat_yearlyProd_temp_20032009.grd")
sat_two_yearly_cv <- cv(Twosat)
writeRaster(sat_two_yearly_cv, filename='sat_two_yearly_cv.grd')

#### caer ####

caer <- stack("../../data/C-AER/rsds_day_20032009.nc")
caer <- setZ(caer, idx)
caer_rsds_yearly <- zApply(caer, by=year, fun='mean')

caer_rsds_yearly_cv <- cv(caer_rsds_yearly)
writeRaster(caer_rsds_yearly_cv, filename='caer_rsds_yearly_cv.grd', overwrite=TRUE)

## FIXED

fixedcaer<- stack("../../calc/proj12abr/fixed_caer_yearlyProd_temp_20032009.grd")
fixed_caer_yearly_cv <- cv(fixedcaer)
writeRaster(fixed_caer_yearly_cv, filename='caer_fixed_yearly_cv.grd', overwrite=TRUE)

## ONE

Onecaer<- stack("../../calc/proj12abr/oneAxis_caer_yearlyProd_temp_20032009.grd")
one_caer_yearly_cv <- cv(Onecaer)
writeRaster(one_caer_yearly_cv, filename='caer_one_yearly_cv.grd')

## TWO

Twocaer <- stack("../../calc/proj12abr/twoAxes_caer_yearlyProd_temp_20032009.grd")
two_caer_yearly_cv <- cv(Twocaer)
writeRaster(two_caer_yearly_cv, filename='caer_two_yearly_cv.grd')

#### cno ####

cno <- stack("../../data/C-NO/rsds_no_day_20032009.nc")
cno <- setZ(cno, idx)
cno_rsds_yearly <- zApply(cno, by=year, fun='mean')

cno_rsds_yearly_cv <- cv(cno_rsds_yearly)
writeRaster(cno_rsds_yearly_cv, filename='cno_rsds_yearly_cv.grd', overwrite=TRUE)

## FIXED

fixedcno <- stack("../../calc/proj12abr/fixed_cno_yearlyProd_temp_20032009.grd")
fixed_cno_yearly_cv <- cv(fixedcno)
writeRaster(fixed_cno_yearly_cv, filename='cno_fixed_yearly_cv.grd')

## ONE

Onecno <- stack("../../calc/proj12abr/oneAxis_cno_yearlyProd_temp_20032009.grd")
oneAxis_cno_yearly_cv <- cv(Onecno)
writeRaster(oneAxis_cno_yearly_cv, filename='cno_oneAxis_yearly_cv.grd')

## TWO

Twocno <- stack("../../calc/proj12abr/twoAxes_cno_yearlyProd_temp_20032009.grd")
twoAxes_cno_yearly_cv <- cv(Twocno)
writeRaster(twoAxes_cno_yearly_cv, filename='cno_twoAxes_yearly_cv.grd')
