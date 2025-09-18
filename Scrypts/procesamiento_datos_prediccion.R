##### IMPORTACIÓN DE LIBRERÍAS ############################################
library(fpp2)
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(gridExtra)

##### IMPORTACIÓN DE DATOS #################################################
ipc <- read.csv(file = "Datos/ipc_usa.csv")
pib <- read.csv(file = "Datos/pib_exogenas_usa.csv", sep = ";")

##### PREPARACIÓN DE SERIES TEMPORALES ####################################
## --- IPC (Índice de Precios al Consumidor) ------------------------------
ipc <- ipc %>% select(-DATE)
IPC <- ts(data = ipc, start = 1970, frequency = 12)

## --- PIB -----------------------------------------------------------------
pib <- pib %>% select(-Country, -Code, -ContinentCode, -Year, -Month)
PIB <- pib$GDP.billion.currency.units
PIB <- na.omit(PIB)
PIB <- ts(data = PIB, start = 1970, frequency = 4)

## --- VARIABLES ADICIONALES -----------------------------------------------
# Oferta monetaria (mensual)
Money <- pib$Money.supply.billion.currency.units
Money <- na.omit(Money)
MONEY <- ts(data = Money, start = 1970, frequency = 12)

# Tasa de desempleo (mensual)
Unemp <- pib$Unemployment.rate.percent
Unemp <- na.omit(Unemp)
UNEMP <- ts(data = Unemp, start = 1970, frequency = 12)

# Índice bursátil (mensual)
Stock <- pib$Stock.market.index
Stock <- na.omit(Stock)
STOCK <- ts(data = Stock, start = 1970, frequency = 12)

##### ANÁLISIS EXPLORATORIO ################################################
cat("=== GRÁFICAS ORIGINALES ===\n")

# Crear gráficas de las series originales
p1 <- autoplot(IPC) + ggtitle("IPC Original") + ylab("IPC")
p2 <- autoplot(PIB) + ggtitle("PIB Original") + ylab("PIB (billones)")
p3 <- autoplot(MONEY) + ggtitle("Oferta Monetaria Original") + ylab("Money Supply")
p4 <- autoplot(UNEMP) + ggtitle("Desempleo Original") + ylab("Unemployment %")
p5 <- autoplot(STOCK) + ggtitle("Índice Bursátil Original") + ylab("Stock Index")

# Mostrar gráficas
grid.arrange(p1, p2, p3, p4, p5, ncol=2, nrow=3)

##### ANÁLISIS DE ESTACIONARIEDAD ##########################################
cat("\n=== TESTS DE ESTACIONARIEDAD (SERIES ORIGINALES) ===\n")

# Función para evaluar estacionariedad
test_stationarity <- function(ts_data, name) {
  cat(paste("\n--- Análisis de:", name, "---\n"))
  
  # Test ADF (H0: no estacionaria)
  adf_test <- adf.test(ts_data)
  cat("Test ADF - p-value:", round(adf_test$p.value, 4))
  if(adf_test$p.value < 0.05) {
    cat(" -> ESTACIONARIA\n")
  } else {
    cat(" -> NO ESTACIONARIA\n")
  }
  
  # Test KPSS (H0: estacionaria)
  kpss_test <- kpss.test(ts_data)
  cat("Test KPSS - p-value:", round(kpss_test$p.value, 4))
  if(kpss_test$p.value > 0.05) {
    cat(" -> ESTACIONARIA\n")
  } else {
    cat(" -> NO ESTACIONARIA\n")
  }
  
  # Ljung-Box test para correlación
  lb_test <- Box.test(ts_data, lag = 20, type="Ljung")
  cat("Ljung-Box - p-value:", round(lb_test$p.value, 4))
  if(lb_test$p.value < 0.05) {
    cat(" -> HAY CORRELACIÓN\n")
  } else {
    cat(" -> NO HAY CORRELACIÓN\n")
  }
}

# Evaluar cada serie
test_stationarity(IPC, "IPC")
test_stationarity(PIB, "PIB") 
test_stationarity(MONEY, "Oferta Monetaria")
test_stationarity(UNEMP, "Desempleo")
test_stationarity(STOCK, "Índice Bursátil")

##### TRANSFORMACIONES PARA ESTACIONARIEDAD ################################
cat("\n=== APLICANDO TRANSFORMACIONES ===\n")

## --- TRANSFORMACIÓN IPC --------------------------------------------------
cat("\nTransformando IPC...\n")
# IPC suele tener tendencia exponencial, aplicamos log y diferencias
lambda_ipc <- BoxCox.lambda(IPC)
cat("Lambda óptimo IPC:", round(lambda_ipc, 4), "\n")

# Aplicar transformación Box-Cox (o log si lambda ≈ 0)
if(abs(lambda_ipc) < 0.1) {
  IPC_transformed <- log(IPC)
  cat("Aplicando transformación logarítmica\n")
} else {
  IPC_transformed <- BoxCox(IPC, lambda = lambda_ipc)
  cat("Aplicando transformación Box-Cox\n")
}

# Diferencias para eliminar tendencia
IPC_diff1 <- diff(IPC_transformed, differences = 1)
autoplot(IPC_diff1) + ggtitle("IPC - Primera Diferencia")

# Test de estacionariedad después de transformación
test_stationarity(IPC_diff1, "IPC Transformado")

## --- TRANSFORMACIÓN PIB --------------------------------------------------
cat("\nTransformando PIB...\n")
lambda_pib <- BoxCox.lambda(PIB)
cat("Lambda óptimo PIB:", round(lambda_pib, 4), "\n")

if(abs(lambda_pib) < 0.1) {
  PIB_transformed <- log(PIB)
  cat("Aplicando transformación logarítmica\n")
} else {
  PIB_transformed <- BoxCox(PIB, lambda = lambda_pib)
  cat("Aplicando transformación Box-Cox\n")
}

# PIB trimestral, probar diferencias estacionales primero
PIB_diff_seasonal <- diff(PIB_transformed, lag = 4) # Diferencia estacional
PIB_diff_both <- diff(PIB_diff_seasonal, differences = 1) # Primera diferencia

autoplot(PIB_diff_both) + ggtitle("PIB - Diferencias Estacional y Regular")
test_stationarity(PIB_diff_both, "PIB Transformado")

## --- TRANSFORMACIÓN OFERTA MONETARIA -------------------------------------
cat("\nTransformando Oferta Monetaria...\n")
lambda_money <- BoxCox.lambda(MONEY)
cat("Lambda óptimo Money:", round(lambda_money, 4), "\n")

MONEY_transformed <- log(MONEY) # Suele ser mejor log para variables monetarias
MONEY_diff1 <- diff(MONEY_transformed, differences = 1)

autoplot(MONEY_diff1) + ggtitle("Oferta Monetaria - Primera Diferencia (log)")
test_stationarity(MONEY_diff1, "Oferta Monetaria Transformada")

## --- TRANSFORMACIÓN DESEMPLEO --------------------------------------------
cat("\nTransformando Desempleo...\n")
# El desempleo puede ser ya estacionario o necesitar solo diferencias
UNEMP_diff1 <- diff(UNEMP, differences = 1)

autoplot(UNEMP_diff1) + ggtitle("Desempleo - Primera Diferencia")
test_stationarity(UNEMP_diff1, "Desempleo Transformado")

## --- TRANSFORMACIÓN ÍNDICE BURSÁTIL --------------------------------------
cat("\nTransformando Índice Bursátil...\n")
lambda_stock <- BoxCox.lambda(STOCK)
cat("Lambda óptimo Stock:", round(lambda_stock, 4), "\n")

STOCK_transformed <- log(STOCK)
STOCK_diff1 <- diff(STOCK_transformed, differences = 1)

autoplot(STOCK_diff1) + ggtitle("Índice Bursátil - Primera Diferencia (log)")
test_stationarity(STOCK_diff1, "Índice Bursátil Transformado")

##### GRÁFICAS DE SERIES TRANSFORMADAS #####################################
cat("\n=== COMPARACIÓN: ANTES Y DESPUÉS ===\n")

# Crear comparaciones lado a lado
par(mfrow=c(2,2))

# IPC
plot(IPC, main="IPC Original", col="blue")
plot(IPC_diff1, main="IPC Transformado", col="red")

# PIB
plot(PIB, main="PIB Original", col="blue") 
plot(PIB_diff_both, main="PIB Transformado", col="red")

par(mfrow=c(1,1))

##### ANÁLISIS DE AUTOCORRELACIÓN ##########################################
cat("\n=== ANÁLISIS ACF/PACF DE SERIES TRANSFORMADAS ===\n")

# Función para mostrar ACF y PACF
show_correlations <- function(ts_data, title) {
  cat(paste("\n--- ACF/PACF para:", title, "---\n"))
  
  # Crear gráficas ACF y PACF
  ggtsdisplay(ts_data, main = title)
}

# Mostrar correlaciones para series transformadas
show_correlations(IPC_diff1, "IPC Transformado")
show_correlations(PIB_diff_both, "PIB Transformado")
show_correlations(MONEY_diff1, "Oferta Monetaria Transformada")
show_correlations(UNEMP_diff1, "Desempleo Transformado")
show_correlations(STOCK_diff1, "Índice Bursátil Transformado")

##### MODELOS AUTOMÁTICOS PARA VERIFICACIÓN ################################
cat("\n=== MODELOS ARIMA AUTOMÁTICOS ===\n")

# Ajustar modelos automáticos para ver qué sugiere auto.arima
fit_ipc <- auto.arima(IPC)
fit_pib <- auto.arima(PIB)
fit_money <- auto.arima(MONEY)
fit_unemp <- auto.arima(UNEMP)
fit_stock <- auto.arima(STOCK)

cat("Modelo sugerido para IPC:", paste(fit_ipc$call), "\n")
cat("Modelo sugerido para PIB:", paste(fit_pib$call), "\n")
cat("Modelo sugerido para Money:", paste(fit_money$call), "\n")
cat("Modelo sugerido para Desempleo:", paste(fit_unemp$call), "\n")
cat("Modelo sugerido para Stock:", paste(fit_stock$call), "\n")

##### VALIDACIÓN DE RESIDUOS ################################################
cat("\n=== VALIDACIÓN DE RESIDUOS ===\n")

# Verificar que los modelos capturen bien la información
checkresiduals(fit_ipc)
checkresiduals(fit_pib)

##### RESUMEN DE TRANSFORMACIONES APLICADAS ################################
cat("\n=== RESUMEN DE TRANSFORMACIONES APLICADAS ===\n")
cat("1. IPC: Transformación Box-Cox/Log + Primera Diferencia\n")
cat("2. PIB: Transformación Box-Cox/Log + Diferencia Estacional (lag=4) + Primera Diferencia\n") 
cat("3. Oferta Monetaria: Transformación Log + Primera Diferencia\n")
cat("4. Desempleo: Primera Diferencia\n")
cat("5. Índice Bursátil: Transformación Log + Primera Diferencia\n")

cat("\nTodas las series han sido transformadas para ser estacionarias y aptas para predicción.\n")

##### PREPARAR DATOS PARA MODELADO #########################################
cat("\n=== DATOS PREPARADOS PARA MODELADO ===\n")

# Guardar series transformadas para uso posterior
series_transformadas <- list(
  IPC_diff = IPC_diff1,
  PIB_diff = PIB_diff_both, 
  MONEY_diff = MONEY_diff1,
  UNEMP_diff = UNEMP_diff1,
  STOCK_diff = STOCK_diff1
)

# Guardar parámetros de transformación para revertir predicciones
params_transformacion <- list(
  lambda_ipc = ifelse(abs(lambda_ipc) < 0.1, 0, lambda_ipc),
  lambda_pib = ifelse(abs(lambda_pib) < 0.1, 0, lambda_pib),
  lambda_money = 0, # log
  lambda_stock = 0, # log
  
  # Información sobre diferenciación
  ipc_diff_order = 1,
  pib_seasonal_diff = TRUE,
  pib_diff_order = 1,
  money_diff_order = 1,
  unemp_diff_order = 1,
  stock_diff_order = 1
)

cat("Series transformadas guardadas en 'series_transformadas'\n")
cat("Parámetros de transformación guardados en 'params_transformacion'\n")
cat("\nLas series están ahora listas para modelado ARIMA, VAR, etc.\n")