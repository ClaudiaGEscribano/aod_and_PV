## Aquí voy a escribir el código de mapas para las diferencias según temperatura.
 
source('projectInfo.R')
source('graticule.R')

## Leo los datos de salida de las simulaciones que vaya a representar.

fixed_yearly<- stack("/home/claudia/aod_and_PV/sim20032009/calc/sensitivityTas/fixed_difference_temperature_caer.grd")

fixed_mon <- stack("/home/claudia/aod_and_PV/sim20032009/calc/sensitivityTas/fixed_difference_temperature_mon_caer.grd")

fixed_yearly_relative <- stack("/home/claudia/aod_and_PV/sim20032009/calc/sensitivityTas/fixed_difference_relative_temperature_caer.grd")
fixed_mon_relative <- stack("/home/claudia/aod_and_PV/sim20032009/calc/sensitivityTas/fixed_difference_relative_temperature_mon_caer.grd")

fixed_yearly <- setProj(fixed_yearly)
fixed_mon<- setProj(fixed_mon)

fixed_yearly_relative <- setProj(fixed_yearly_relative)
fixed_mon_relative <- setProj(fixed_mon_relative)

fixed_yearly_relative <- fixed_yearly_relative*100

source('border.R')

############################################
  
pdf("/home/claudia/aod_and_PV/sim20032009/calc/sensitivityTas/yearly_differences_relative_caer.pdf", height=3, width=7)
levelplot(fixed_yearly_relative, scales=list(draw=FALSE), colorkey=list(space='bottom', title='%'), margin=FALSE)+ layer(sp.lines(border, lwd=0.5))+
    layer(sp.lines(grat, lwd=0.5)) +
    layer(sp.text(coordinates(labsLon),
                  txt = parse(text = labsLon$lab),
                  adj = c(1.1, -0.25),
                  cex = 0.3)) +
    layer(sp.text(coordinates(labsLat),
                  txt = parse(text = labsLat$lab),
                  adj = c(-0.25, -0.25),
                  cex = 0.3))
dev.off()
