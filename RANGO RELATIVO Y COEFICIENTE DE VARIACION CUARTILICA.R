# RANGO RELATIVO Y COEFICIENTE DE VARIACIÓN CUARTÍLICA
# Conjunto de datos EPA, variable tiempo dentro de cada nivel de ESTUDIOS
# Cargar primero los datos EPA

library(Rcmdr)

tabla=numSummary(EPA[,"TIEMPO"], groups=EPA$ESTUDIOS, statistics=c("mean", "sd", "IQR", "quantiles", "cv"), quantiles=c(0,
  .25,.5,.75,1))


statistics=tabla[[2]] ## Extraer la matriz de estadísticas

colnames(statistics)

## DISPERSIÓN
(RR=(statistics[,"100%"]-statistics[,"0%"])/(statistics[,"100%"]+statistics[,"0%"]))

(CVq=statistics[,"IQR"]/(abs(statistics[,"25%"]+statistics[,"75%"])))

(CV=statistics[,"sd"]/abs(statistics[,"mean"]))

####  ASIMETRÍA

# Pearson:

(Ap=3*(statistics[,"mean"]-statistics[,"50%"])/statistics[,"sd"])

# Yule-Bowley:

(Ayb=(statistics[,"25%"]-2*statistics[,"50%"]+statistics[,"75%"])/(statistics[,"75%"]-statistics[,"25%"]))




