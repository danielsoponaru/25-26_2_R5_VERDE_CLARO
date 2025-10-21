# ==========================================================
# Análisis de series temporales con las 5 variables del PIB
# ==========================================================

# Librerías necesarias
library(readr)
library(dplyr)
library(imputeTS)
library(ggplot2)
library(forecast)
library(tseries)
library(tsoutliers)
library(visdat)
library(stats)
library(lubridate)

# ============================
# 1. Cargar y preparar los datos
# ============================

df <- read.csv("Datos/pib_exogenas_usa.csv", sep = ";")
df1 <- read.csv("Datos/CPI_usa.csv")

# Renombrar columnas y preparar fechas
colnames(df) <- c("Country","Code","ContinentCode","Year","Month",
                  "GDP","Money_supply","Unemployment_rate","Stock_market_index")

df$Date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"))
df1$DATE <- as.Date(df1$DATE)

# Convertir a numéricas las columnas relevantes
df <- df %>%
  mutate(across(c(GDP, Money_supply, Unemployment_rate, Stock_market_index),
                as.numeric)) %>%
  select(GDP, Money_supply, Unemployment_rate, Stock_market_index, Date, Year, Month)

df1 <- df1 %>%
  mutate(CPI = as.numeric(CPI)) %>%
  arrange(DATE)

# Visualización de datos faltantes
vis_miss(df)
vis_miss(df1)

# ============================
# 2. Crear series trimestrales (último dato del trimestre)
# ============================

df <- df %>%
  mutate(Quarter = ceiling(Month / 3)) %>%
  arrange(Year, Quarter, Month)

df_quarterly <- df %>%
  filter(Month %in% c(3, 6, 9, 12)) %>%
  filter(Year %in% 2010:2021) %>%
  group_by(Year, Quarter) %>%
  summarise(
    GDP = first(na.omit(GDP)),
    MS  = last(Money_supply),
    UR  = last(Unemployment_rate),
    SMI = last(Stock_market_index)
  ) %>%
  ungroup() %>%
  arrange(Year, Quarter)

# Crear trimestre para CPI (tomar último dato del trimestre)
df1 <- df1 %>%
  mutate(Year = year(DATE),
         Month = month(DATE),
         Quarter = ceiling(Month / 3)) %>%
  filter(Year %in% 2010:2021) %>%
  group_by(Year, Quarter) %>%
  summarise(CPI = last(CPI)) %>%
  ungroup()

# Unir CPI al dataset trimestral principal
df_quarterly <- df_quarterly %>%
  left_join(df1, by = c("Year", "Quarter"))

# Crear columna Date representando inicio de trimestre
df_quarterly$Date <- as.Date(paste(df_quarterly$Year, (df_quarterly$Quarter - 1) * 3 + 1, "01", sep = "-"))

# Filtrar rango de fechas útil
df_quarterly <- df_quarterly %>%
  filter(Date >= as.Date("2010-07-01") & Date <= as.Date("2021-06-30"))

# Verificar estructura
vis_miss(df_quarterly)
head(df_quarterly)

# ============================
# 3. Crear objetos ts (frecuencia trimestral)
# ============================

start_year <- min(df_quarterly$Year, na.rm = TRUE)
start_quarter <- min(df_quarterly$Quarter, na.rm = TRUE)

GDP_ts <- ts(df_quarterly$GDP, start = c(start_year, start_quarter), frequency = 4)
MS_ts  <- ts(df_quarterly$MS,  start = c(start_year, start_quarter), frequency = 4)
UR_ts  <- ts(df_quarterly$UR,  start = c(start_year, start_quarter), frequency = 4)
SMI_ts <- ts(df_quarterly$SMI, start = c(start_year, start_quarter), frequency = 4)
CPI_ts <- ts(df_quarterly$CPI, start = c(start_year, start_quarter), frequency = 4)

# ============================
# 4. Detección y ajuste de outliers
# ============================

outliers_GDP <- tsclean(GDP_ts)
outliers_MS  <- tsclean(MS_ts)
outliers_UR  <- tsclean(UR_ts)
outliers_SMI <- tsclean(SMI_ts)
outliers_CPI <- tsclean(CPI_ts)

# Graficar comparación
par(mfrow = c(3,2))
ts.plot(GDP_ts, outliers_GDP, col=c("black","red"), lty=c(1,2),
        main="GDP: Original vs Ajustada")
ts.plot(MS_ts, outliers_MS, col=c("black","red"), lty=c(1,2),
        main="MS: Original vs Ajustada")
ts.plot(UR_ts, outliers_UR, col=c("black","red"), lty=c(1,2),
        main="UR: Original vs Ajustada")
ts.plot(SMI_ts, outliers_SMI, col=c("black","red"), lty=c(1,2),
        main="SMI: Original vs Ajustada")
ts.plot(CPI_ts, outliers_CPI, col=c("black","red"), lty=c(1,2),
        main="CPI: Original vs Ajustada")
par(mfrow=c(1,1))

# ============================
# 5. Control de varianza, estacionalidad y tendencia
# ============================

# ==========================================
# Función de comprobación de estacionariedad
# ==========================================

comprobacion_tratamiento <- function(serie, nombre_serie = "Serie") {
  
  cat("Resultados para:", nombre_serie)
  
  # ADF Test
  adf <- adf.test(serie)
  cat("ADF test p-value:", round(adf$p.value, 4))
  if (adf$p.value < 0.05) {
    cat(" → Serie estacionaria según ADF ✅\n")
  } else {
    cat(" → Serie NO estacionaria según ADF ❌\n")
  }
  
  # KPSS Test
  kpss <- kpss.test(serie, null = "Level")
  cat("KPSS test p-value:", round(kpss$p.value, 4))
  if (kpss$p.value > 0.05) {
    cat(" → Serie estacionaria según KPSS ✅\n")
  } else {
    cat(" → Serie NO estacionaria según KPSS ❌\n")
  }
  
  # Ljung-Box Test (ruido blanco)
  ljung <- Box.test(serie, lag = 10, type = "Ljung-Box")
  cat("Ljung-Box test p-value:", round(ljung$p.value, 4))
  if (ljung$p.value > 0.05) {
    cat(" → Residuos sin autocorrelación (ruido blanco) ✅\n")
  } else {
    cat(" → Residuos autocorrelacionados ❌\n")
  }
  
  cat("\n")
}

# ============================================================
# GDP
# ============================================================
# GDP_BASE <- outliers_GDP
#outliers_GDP <- window(GDP_BASE, start = c(2010, 1))

# --- VARIANZA ---
ggtsdisplay(outliers_GDP, main = "GDP - Serie original")

lambda_GDP <- BoxCox.lambda(outliers_GDP)
cat("Lambda GDP:", lambda_GDP)

boxcox_GDP <- BoxCox(outliers_GDP, lambda_GDP)
ggtsdisplay(boxcox_GDP, main = "GDP - Tras BoxCox")

# # --- TENDENCIA ---
# diff_boxcox_GDP <- diff(boxcox_GDP)
# ggtsdisplay(diff_boxcox_GDP, main = "GDP - Doble diferencia con estacionalidad")
# comprobacion_tratamiento(diff_boxcox_GDP, "GDP")


# TENDENCIA DECOMPOSE
tendencia_GDP <- decompose(boxcox_GDP, type = "additive")$trend
tend_boxcox_GDP <- na.omit(boxcox_GDP-tendencia_GDP)
ggtsdisplay(tend_boxcox_GDP)

# ESTACIONALIDAD
est_tend_boxcox_GDP <- diff(tend_boxcox_GDP, lag = 3)
ggtsdisplay(est_tend_boxcox_GDP)

# Estacionaria?
comprobacion_tratamiento(est_tend_boxcox_GDP, "GDP") #Esta ok

# ============================================================
# MS
# ============================================================

# --- VARIANZA ---
ggtsdisplay(outliers_MS, main = "MS - Serie original")

lambda_MS <- BoxCox.lambda(outliers_MS)
cat("Lambda MS:", lambda_MS)

boxcox_MS <- BoxCox(outliers_MS, lambda_MS)
ggtsdisplay(boxcox_MS, main = "MS - Tras BoxCox")

# --- ESTACIONALIDAD ---
diff_boxcox_MS <- diff(diff(diff(boxcox_MS)), lag = 4)
ggtsdisplay(diff_boxcox_MS, main = "MS - Diferenciada (lag=1)")


comprobacion_tratamiento(diff_boxcox_MS, "MS") #Esta ok


# ============================================================
# UR
# ============================================================

# --- VARIANZA ---
ggtsdisplay(outliers_UR, main = "UR - Serie original")

lambda_UR <- BoxCox.lambda(outliers_UR)
cat("Lambda UR:", lambda_UR)

boxcox_UR <- BoxCox(outliers_UR, lambda_UR)
ggtsdisplay(boxcox_UR, main = "UR - Tras BoxCox")

# --- TENDENCIA ---
tendencia_UR <- decompose(boxcox_UR, type = "additive")$trend
tend_boxcox_UR <- na.omit(boxcox_UR - tendencia_UR)
# diff_boxcox_UR <- diff(boxcox_UR)
ggtsdisplay(tend_boxcox_UR, main = "UR - Diferenciada (lag=1)")

# --- ESTACIONALIDAD ---
est_tend_boxcox_UR <- diff(tend_boxcox_UR, lag = 2)
comprobacion_tratamiento(est_tend_boxcox_UR, "UR") # Esta ok


# ============================================================
# SMI
# ============================================================

# --- VARIANZA ---
ggtsdisplay(outliers_SMI, main = "SMI - Serie original")

lambda_SMI <- BoxCox.lambda(outliers_SMI)
cat("Lambda SMI:", lambda_SMI)

boxcox_SMI <- BoxCox(outliers_SMI, lambda_SMI)
ggtsdisplay(boxcox_SMI, main = "SMI - Tras BoxCox")

# --- Tendencia ---
tendencia_SMI <- decompose(boxcox_SMI, type = "additive")$trend
tend_boxcox_SMI <- na.omit(boxcox_SMI - tendencia_SMI)

ggtsdisplay(tend_boxcox_SMI, main = "SMI - Diferenciada (lag=1)")

# --- ESTACIONALIDAD ---
diff_boxcox_SMI <- diff(boxcox_SMI, differences = 2)
ggtsdisplay(diff_boxcox_SMI, main = "SMI - Diferenciada (lag=1)")

comprobacion_tratamiento(diff_boxcox_SMI, "SMI") # Esta ok


# ============================================================
# CPI
# ============================================================

# --- VARIANZA ---
ggtsdisplay(outliers_CPI, main = "CPI - Serie original")

lambda_CPI <- BoxCox.lambda(outliers_CPI)
cat("Lambda CPI:", lambda_CPI)

boxcox_CPI <- BoxCox(outliers_CPI, lambda_CPI)
ggtsdisplay(boxcox_CPI, main = "CPI - Tras BoxCox")

# --- Tendencia Y Estacionalidad ---
tendencia_CPI <- decompose(boxcox_CPI, type = "additive")$trend
tend_boxcox_CPI <- na.omit(boxcox_CPI - tendencia_CPI)

ggtsdisplay(tend_boxcox_CPI, main = "CPI - Diferenciada (lag=1)")

comprobacion_tratamiento(tend_boxcox_CPI, "CPI") # Esta ok


# ==========================================================
# 6. MODELADO ARIMAX MULTIVARIABLE AUTOMÁTICO Y ROBUSTO
# ==========================================================

# ==========================================================
# Función auxiliar de métricas seguras
# ==========================================================
safe_accuracy <- function(modelo) {
  acc <- tryCatch({
    accuracy(modelo)
  }, error = function(e) {
    data.frame(ME = NA, RMSE = NA, MAE = NA, MPE = NA, MAPE = NA)
  })
  return(acc)
}

# ==========================================================
# Función principal: Ajuste ARIMAX con validación
# ==========================================================
evaluar_arimax <- function(y, xreg, nombre) {
  df_temp <- na.omit(cbind(y, xreg))
  y_clean <- df_temp[, 1]
  x_clean <- df_temp[, -1, drop = FALSE]
  
  cat("\n📈 Ajustando modelo para:", nombre, "\n")
  
  modelo <- tryCatch({
    auto.arima(y_clean,
               xreg = x_clean,
               stepwise = FALSE,       # búsqueda completa
               approximation = FALSE,  # más preciso
               seasonal = FALSE,       # sin patrón trimestral fuerte
               allowdrift = TRUE,
               trace = FALSE)
  }, error = function(e) {
    cat("⚠️ Error al ajustar", nombre, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(modelo)) {
    return(data.frame(Variable = nombre, AICc = NA, RMSE = NA, MAPE = NA))
  }
  
  # Métricas de rendimiento
  acc <- safe_accuracy(modelo)
  
  cat("✅ Modelo elegido:", capture.output(modelo)[1], "\n")
  cat("AICc:", round(modelo$aicc, 2), " | MAPE:", round(acc[5], 3), "\n\n")
  
  # Devolver resumen
  return(data.frame(Variable = nombre,
                    AICc = modelo$aicc,
                    RMSE = acc[2],
                    MAPE = acc[5]))
}

# ==========================================================
# 3️⃣ Crear matriz de regresores
# ==========================================================
regresores <- data.frame(
  GDP = outliers_GDP,
  MS  = outliers_MS,
  UR  = outliers_UR,
  SMI = outliers_SMI,
  CPI = outliers_CPI
)

# ==========================================================
# 4️⃣ Ajustar modelos ARIMAX para cada variable
# ==========================================================
resultados <- list(
  evaluar_arimax(regresores$GDP, regresores %>% select(MS, UR, SMI, CPI), "GDP"),
  evaluar_arimax(regresores$MS,  regresores %>% select(GDP, UR, SMI, CPI), "MS"),
  evaluar_arimax(regresores$UR,  regresores %>% select(GDP, MS, SMI, CPI), "UR"),
  evaluar_arimax(regresores$SMI, regresores %>% select(GDP, MS, UR, CPI), "SMI"),
  evaluar_arimax(regresores$CPI, regresores %>% select(GDP, MS, UR, SMI), "CPI")
)

# ==========================================================
# 5️⃣ Generar tabla resumen comparativa
# ==========================================================
tabla_resultados <- do.call(rbind, resultados) %>%
  arrange(AICc)

cat("\n======================================\n")
cat("🏁 RESULTADOS FINALES ARIMAX\n")
cat("======================================\n")
print(tabla_resultados)

# ==========================================================
# 6️⃣ (Opcional) Visualización de residuos
# ==========================================================
# Puedes inspeccionar el modelo del PIB, por ejemplo:
# fit_GDP <- auto.arima(outliers_GDP, xreg = cbind(outliers_MS, outliers_UR, outliers_SMI, outliers_CPI))
# checkresiduals(fit_GDP)




