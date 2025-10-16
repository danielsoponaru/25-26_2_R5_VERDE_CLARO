# ==========================================================
# Análisis de series temporales con las 4 variables del PIB
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
library(ggplot2)


#   FALTA POR METER EL XSLX DE IPC, TENER EN CUENTA

# ============================
# 1. Cargar, preparado de los datos y visualizacion de los datos faltantes
# ============================

df <- read.csv("Datos/pib_exogenas_usa.csv",sep = ";")

colnames(df) <- c("Country","Code","ContinentCode","Year","Month",
                  "GDP","Money_supply","Unemployment_rate","Stock_market_index")

df$Date <- as.Date(paste(df$Year, df$Month, "01", sep="-"))

df<- df %>%
  mutate(across(c(GDP, Money_supply, Unemployment_rate, Stock_market_index), 
                as.numeric)) %>% 
  select(c(GDP,Money_supply,Unemployment_rate,Stock_market_index,Date,Year, Month))

vis_miss(df)

# Observamos que los datos presentan NAs en GDP ya que presenta datos trimestrales,
# aparte de los datos por imputar de las variables MS, UR y SMI.

# ============================
# 2. Crear series temporales trimestrales (último dato del trimestre)
# ============================

df <- df %>%
  mutate(Quarter = ceiling(Month / 3)) %>%
  arrange(Year, Quarter, Month)

df_quarterly <- df %>%
  filter(Month %in% c(3, 6, 9, 12)) %>%
  filter(Year %in% 2010:2021) %>% #He filtrado desde 2010 hasta 2021 que es donde tenemos los valores faltantes
  group_by(Year, Quarter) %>%
  summarise(
    GDP = first(na.omit(GDP)),
    MS  = last(Money_supply),
    UR  = last(Unemployment_rate),
    SMI = last(Stock_market_index)
  ) %>%
  ungroup() %>%
  arrange(Year, Quarter)


vis_miss(df_quarterly)

df_quarterly$Date <- as.Date(paste(df_quarterly$Year, (df_quarterly$Quarter - 1) * 3 + 1, "01", sep = "-"))

df_quarterly <- df_quarterly %>%
  filter(Date >= as.Date("2010-07-01") & Date <= as.Date("2021-06-30"))

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

# ============================
# 4. Detección y ajuste de outliers (versión moderada)
# ============================

outliers_GDP <- tsclean(GDP_ts)
outliers_MS  <- tsclean(MS_ts)
outliers_UR  <- tsclean(UR_ts)
outliers_SMI <- tsclean(SMI_ts)

par(mfrow=c(2,2))
ts.plot(GDP_ts, outliers_GDP, col=c("black","red"), lty=c(1,2),
        main="GDP: Original vs Ajustada (tsclean)")
ts.plot(MS_ts, outliers_MS, col=c("black","red"), lty=c(1,2),
        main="MS: Original vs Ajustada (tsclean)")
ts.plot(UR_ts, outliers_UR, col=c("black","red"), lty=c(1,2),
        main="UR: Original vs Ajustada (tsclean)")
ts.plot(SMI_ts, outliers_SMI, col=c("black","red"), lty=c(1,2),
        main="SMI: Original vs Ajustada (tsclean)")
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







