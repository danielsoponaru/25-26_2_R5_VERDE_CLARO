########################### ANÁLISIS MACROECONÓMICO ############################
##### LIBRERÍAS ################################################################
#DPLYR
if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}
#LUBRIDATE
if (!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
} else {
  library(lubridate)
}

##### IMPORTACIÓN DE DATOS #####################################################
pib_nominal = read.csv(file = "Datos/PIB_NOMINAL_ABS.csv")
pib_real = read.csv(file = "Datos/PIB_REAL_ABS.csv")
consumo = read.csv(file = "Datos/CONSUMO_REL.csv")
inversion = read.csv(file = "Datos/INVERSION_BRUTA_ABS.csv")
gasto = read.csv(file = "Datos/GASTO_PUBLICO_ABS.csv")
exportaciones = read.csv(file = "Datos/EXPORTACIONES_ABS.csv")
importaciones = read.csv(file = "Datos/IMPORTACIONES_ABS.csv")
export_netas = read.csv(file = "Datos/EXPORTACIONES_NETAS_ABS.csv")
EUR_USD = read.csv("Datos/EUS_USD.csv")

##### PROCESAMIENTO DE DATOS ###################################################
colnames(pib_nominal) = c("date", "PIB_NOMINAL")
colnames(pib_real) = c("date", "PIB_REAL")
crec_pib_nominal = c(NA, diff(pib_nominal$PIB_NOMINAL))/pib_nominal$PIB_NOMINAL*100
pib_nominal = cbind(pib_nominal, crec_pib_nominal)
rm(crec_pib_nominal)
crec_pib_real = c(NA, diff(pib_real$PIB_REAL))/pib_real$PIB_REAL*100
pib_real = cbind(pib_real, crec_pib_real)
rm(crec_pib_real)

colnames(inversion) = c("date", "INVERSION")
colnames(gasto) = c("date", "GASTO")
colnames(export_netas) = c("date", "EXPORTACIONES_NETAS")
colnames(exportaciones) = c("date", "EXPORTACIONES")
colnames(importaciones) = c("date", "IMPORTACIONES")
crec_inversion = c(NA, diff(inversion$INVERSION))/inversion$INVERSION*100
crec_gasto = c(NA, diff(gasto$GASTO))/gasto$GASTO*100
crec_export_netas = c(NA, diff(export_netas$EXPORTACIONES_NETAS))/export_netas$EXPORTACIONES_NETAS*100
crec_exportaciones = c(NA, diff(exportaciones$EXPORTACIONES))/exportaciones$EXPORTACIONES*100
crec_importaciones = c(NA, diff(importaciones$IMPORTACIONES))/importaciones$IMPORTACIONES*100
inversion = cbind(inversion, crec_inversion)
rm(crec_inversion)
gasto = cbind(gasto, crec_gasto)
rm(crec_gasto)
export_netas = cbind(export_netas, crec_export_netas)
rm(crec_export_netas)
exportaciones = cbind(exportaciones, crec_exportaciones)
rm(crec_exportaciones)
importaciones = cbind(importaciones, crec_importaciones)
rm(crec_importaciones)

df = pib_nominal %>%
  inner_join(pib_real, by = c("date")) %>%
  inner_join(inversion, by = c("date")) %>%
  inner_join(gasto, by = c("date")) %>%
  inner_join(export_netas, by = c("date")) %>%
  inner_join(exportaciones, by = c("date")) %>%
  inner_join(importaciones, by = c("date"))

# df$PIB_NOMINAL = df$PIB_NOMINAL*10^6
# df$PIB_REAL = df$PIB_REAL*10^9
# df$IMPORTACIONES = df$IMPORTACIONES*10^9
# df$EXPORTACIONES = df$EXPORTACIONES*10^9
# df$INVERSION = df$INVERSION*10^9
# df$GASTO = df$GASTO*10^9
# df$EXPORTACIONES_NETAS = df$EXPORTACIONES_NETAS*10^9

tail(df$INVERSION+df$GASTO+df$EXPORTACIONES_NETAS)
tail(df)

#REVISAR GASTO pÜBLICO