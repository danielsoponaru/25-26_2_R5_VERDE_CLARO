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
  filter(Year %in% 1970:2021) %>% #He filtrado hasta 2021 que es donde tenemos los valores faltantes
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

# Crear columna de fecha representando el inicio del trimestre
df_quarterly$Date <- as.Date(paste(df_quarterly$Year, (df_quarterly$Quarter - 1) * 3 + 1, "01", sep = "-"))

# Visualizar primeras filas
head(df_quarterly)

# ============================
# Crear objetos ts (frecuencia trimestral)
# ============================

start_year <- min(df_quarterly$Year, na.rm = TRUE)
start_quarter <- min(df_quarterly$Quarter, na.rm = TRUE)

GDP_ts <- ts(df_quarterly$GDP, start = c(start_year, start_quarter), frequency = 4)
MS_ts  <- ts(df_quarterly$MS,  start = c(start_year, start_quarter), frequency = 4)
UR_ts  <- ts(df_quarterly$UR,  start = c(start_year, start_quarter), frequency = 4)
SMI_ts <- ts(df_quarterly$SMI, start = c(start_year, start_quarter), frequency = 4)

# ============================
# 4. Detección de outliers y ajuste de seeries
# ============================



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
legend("topleft", legend=c("Original","Ajustada"), col=c("black","red"), lty=1:2)

ts.plot(MS_ts, outliers_MS, col=c("black","red"), lty=c(1,2),
        main="MS: Original vs Ajustada (tsclean)")
ts.plot(UR_ts, outliers_UR, col=c("black","red"), lty=c(1,2),
        main="UR: Original vs Ajustada (tsclean)")
ts.plot(SMI_ts, outliers_SMI, col=c("black","red"), lty=c(1,2),
        main="SMI: Original vs Ajustada (tsclean)")
par(mfrow=c(1,1))



