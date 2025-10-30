# Series temporales - PIB y variables exógenas

# Librerías
library(dplyr)
library(imputeTS)
library(forecast)
library(tseries)
library(tsoutliers)
library(visdat)
library(stats)
library(lubridate)
library(vars)
library(forecast)
library(ggplot2)
library(vars)
library(gridExtra)

# Cargar datos

df <- read.csv("Datos/pib_exogenas_usa.csv", sep = ";")
df1 <- read.csv("Datos/ipc_usa.csv")

colnames(df) <- c("Country","Code","ContinentCode","Year","Month",
                  "GDP","Money_supply","Unemployment_rate","Stock_market_index")

df$Date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"))
df1$DATE <- as.Date(df1$DATE)

df <- df %>%
  mutate(across(c(GDP, Money_supply, Unemployment_rate),
                as.numeric))


df1 <- df1 %>%
  mutate(CPI = as.numeric(CPI)) %>%
  arrange(DATE)

vis_miss(df)
vis_miss(df1)

# Series trimestrales

df <- df %>%
  mutate(Quarter = ceiling(Month / 3)) %>%
  arrange(Year, Quarter, Month)

# Preparar df_quarterly
df_quarterly <- df %>%
  filter(Month %in% c(3, 6, 9, 12)) %>%
  filter((Year > 2012 | (Year == 2012 & Quarter >= 3)) &
           (Year < 2022 | (Year == 2022 & Quarter <= 2))) %>%
  group_by(Year, Quarter) %>%
  summarise(
    GDP = first(na.omit(GDP)),
    MS  = last(Money_supply),
    UR  = last(Unemployment_rate)
  ) %>%
  ungroup() %>%
  arrange(Year, Quarter)

# Preparar df1 (CPI)
df1 <- df1 %>%
  mutate(Year = year(DATE),
         Month = month(DATE),
         Quarter = ceiling(Month / 3)) %>%
  group_by(Year, Quarter) %>%
  summarise(CPI = last(CPI)) %>%
  ungroup() %>%
  filter((Year > 2010 | (Year == 2010 & Quarter >= 3)) &
           (Year < 2022 | (Year == 2022 & Quarter <= 2)))

# Crear columna Date y filtrar
df_quarterly$Date <- as.Date(paste(df_quarterly$Year, (df_quarterly$Quarter - 1) * 3 + 1, "01", sep = "-"))

df_quarterly <- df_quarterly %>%
  filter(Date >= as.Date("2012-07-01") & Date <= as.Date("2022-06-30"))

vis_miss(df_quarterly)
head(df_quarterly)

# Objetos ts
start_year <- 2012
start_quarter <- 3
end_year <- 2022
end_quarter <- 2

GDP_ts <- ts(df_quarterly$GDP, start = c(start_year, start_quarter), end = c(end_year, end_quarter), frequency = 4)
MS_ts  <- ts(df_quarterly$MS,  start = c(start_year, start_quarter), end = c(end_year, end_quarter), frequency = 4)
UR_ts  <- ts(df_quarterly$UR,  start = c(start_year, start_quarter), end = c(end_year, end_quarter), frequency = 4)
CPI_ts <- ts(df1$CPI, start = c(2010, 3), frequency = 4)

# Detección de outliers

outliers_GDP <- tsclean(GDP_ts)
outliers_MS  <- tsclean(MS_ts)
outliers_UR  <- tsclean(UR_ts)
outliers_CPI <- tsclean(CPI_ts)

# Estacionariedad

# Comprobación de estacionariedad

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

# GDP

ggtsdisplay(outliers_GDP, main = "GDP - Original")
log_GDP <- log(outliers_GDP)
log2_gdp <- log(log_GDP)
ggtsdisplay(log2_gdp, main = "GDP - Log")

tendencia_GDP <- log2_gdp %>% 
  diff(differences = 1) %>% 
  diff(differences = 1, lag = 4)

ggtsdisplay(tendencia_GDP, main = "GDP - Diferenciada")

comprobacion_tratamiento(tendencia_GDP, "GDP")

# MS

ggtsdisplay(outliers_MS, main = "MS - Original")
log_ms <- log(outliers_MS)
ggtsdisplay(log_ms, main = "MS - Log")

diff_boxcox_MS <- log_ms %>% 
  diff(differences = 2) %>% 
  diff(lag = 4, differences = 1)

ggtsdisplay(diff_boxcox_MS, main = "MS - Diferenciada")
comprobacion_tratamiento(diff_boxcox_MS, "MS")

# UR

ggtsdisplay(outliers_UR, main = "UR - Original")
log_ur <- log(outliers_UR)
ggtsdisplay(log_ur, main = "UR - Log")

diff_boxcox_UR <- log_ur %>% 
  diff(differences = 2)

ggtsdisplay(diff_boxcox_UR, main = "UR - Diferenciada")
comprobacion_tratamiento(diff_boxcox_UR, "UR")

# CPI

ggtsdisplay(outliers_CPI, main = "CPI - Original")

log_CPI <- log(outliers_CPI)
ggtsdisplay(log_CPI, main = "CPI - Log")

tendencia_CPI <- log_CPI %>% 
  diff(differences = 1) %>% 
  diff(differences = 2, lag = 4)

ggtsdisplay(tendencia_CPI, main = "CPI - Diferenciada")

comprobacion_tratamiento(tendencia_CPI, "CPI")




################## GDP #################




# Configuracion temporal
train_start <- c(2014, 1)
train_end   <- c(2021, 4)
test_start  <- c(2022, 1)
test_end    <- c(2022, 2)
h <- 2

# Preparar series
tend_train <- window(tendencia_GDP, start = train_start, end = train_end)
tend_test  <- window(tendencia_GDP, start = test_start, end = test_end)
tend_full  <- window(tendencia_GDP, start = train_start, end = test_end)

ms_train <- window(diff_boxcox_MS, start = train_start, end = train_end)
ms_test  <- window(diff_boxcox_MS, start = test_start, end = test_end)
ms_full  <- window(diff_boxcox_MS, start = train_start, end = test_end)

ur_train <- window(diff_boxcox_UR, start = train_start, end = train_end)
ur_test  <- window(diff_boxcox_UR, start = test_start, end = test_end)
ur_full  <- window(diff_boxcox_UR, start = train_start, end = test_end)

actual_val <- window(outliers_GDP, start = test_start, end = test_end)

# Crear lista para almacenar metricas
metricas_list <- list()

# ====================================
# MODELO 1: ARIMA simple
# ====================================
cat("\n[MODELO 1] ARIMA GDP\n")

arima_gdp <- auto.arima(tend_train, seasonal = FALSE, stationary = FALSE, 
                        stepwise = FALSE, approximation = FALSE)

fc_arima <- forecast(arima_gdp, h = h)
pred_t_arima <- fc_arima$mean

# Revertir transformaciones
serie_diff1 <- diff(log2_gdp, differences = 1)
last_diff_regular <- tail(serie_diff1, 4)
rev_season_arima <- diffinv(pred_t_arima, xi = last_diff_regular, lag = 4)
rev_season_arima <- head(rev_season_arima, h)

last_log2 <- tail(log2_gdp, 1)
rev_diff_arima <- diffinv(rev_season_arima, xi = last_log2, lag = 1)
rev_diff_arima <- head(rev_diff_arima, h)

pred_arima_val <- ts(exp(exp(rev_diff_arima)), start = test_start, frequency = 4)

# Metricas validacion
mae_arima <- mean(abs(actual_val - pred_arima_val))
rmse_arima <- sqrt(mean((actual_val - pred_arima_val)^2))
mape_arima <- mean(abs((actual_val - pred_arima_val) / actual_val)) * 100
aicc_arima <- arima_gdp$aicc
bic_arima <- arima_gdp$bic

# Prediccion futura
arima_gdp_full <- auto.arima(tend_full, seasonal = FALSE, stationary = FALSE,
                             stepwise = FALSE, approximation = FALSE)
fc_arima_future <- forecast(arima_gdp_full, h = 2)
pred_t_arima_future <- fc_arima_future$mean

serie_diff1_full <- diff(log2_gdp, differences = 1)
last_diff_full <- tail(serie_diff1_full, 4)
rev_season_arima_fut <- diffinv(pred_t_arima_future, xi = last_diff_full, lag = 4)
rev_season_arima_fut <- head(rev_season_arima_fut, 2)

last_log2_fut <- tail(log2_gdp, 1)
rev_diff_arima_fut <- diffinv(rev_season_arima_fut, xi = last_log2_fut, lag = 1)
rev_diff_arima_fut <- head(rev_diff_arima_fut, 2)

pred_arima_future <- ts(exp(exp(rev_diff_arima_fut)), start = c(2022, 3), frequency = 4)

q3_arima <- as.numeric(pred_arima_future)[1]
q4_arima <- as.numeric(pred_arima_future)[2]

metricas_list[["ARIMA"]] <- list(
  MAE = mae_arima, RMSE = rmse_arima, MAPE = mape_arima,
  AICc = aicc_arima, BIC = bic_arima, Q3 = q3_arima, Q4 = q4_arima
)

# ====================================
# MODELO 2: ARIMAX con UR
# ====================================
cat("\n[MODELO 2] ARIMAX GDP + UR\n")

arimax_ur <- auto.arima(tend_train, xreg = ur_train, seasonal = FALSE, 
                        stationary = FALSE, stepwise = FALSE, approximation = FALSE)

fc_arimax_ur <- forecast(arimax_ur, xreg = as.matrix(ur_test), h = h)
pred_t_arimax_ur <- fc_arimax_ur$mean

# Revertir transformaciones
serie_diff1 <- diff(log_GDP, differences = 1)
last_diff_regular <- tail(serie_diff1, 4)
rev_season_arimax_ur <- diffinv(pred_t_arimax_ur, xi = last_diff_regular, lag = 4)
rev_season_arimax_ur <- tail(rev_season_arimax_ur, h)

last_log <- tail(log_GDP, 1)
rev_diff_arimax_ur <- diffinv(rev_season_arimax_ur, xi = last_log, lag = 1)
rev_diff_arimax_ur <- tail(rev_diff_arimax_ur, h)

pred_arimax_ur_val <- ts(exp(rev_diff_arimax_ur), start = test_start, frequency = 4)

# Metricas validacion
mae_arimax_ur <- mean(abs(actual_val - pred_arimax_ur_val))
rmse_arimax_ur <- sqrt(mean((actual_val - pred_arimax_ur_val)^2))
mape_arimax_ur <- mean(abs((actual_val - pred_arimax_ur_val) / actual_val)) * 100
aicc_arimax_ur <- arimax_ur$aicc
bic_arimax_ur <- arimax_ur$bic

# Prediccion futura
ur_future_pred <- auto.arima(ur_full, seasonal = FALSE, stationary = FALSE,
                             stepwise = FALSE, approximation = FALSE)
fc_ur_future <- forecast(ur_future_pred, h = 2)
ur_future_transf <- fc_ur_future$mean

arimax_ur_full <- auto.arima(tend_full, xreg = ur_full, seasonal = FALSE,
                             stationary = FALSE, stepwise = FALSE, approximation = FALSE)
fc_arimax_ur_future <- forecast(arimax_ur_full, xreg = as.matrix(ur_future_transf), h = 2)
pred_t_arimax_ur_future <- fc_arimax_ur_future$mean

serie_diff1_full <- diff(log_GDP, differences = 1)
last_diff_full <- tail(serie_diff1_full, 4)
rev_season_arimax_ur_fut <- diffinv(pred_t_arimax_ur_future, xi = last_diff_full, lag = 4)
rev_season_arimax_ur_fut <- tail(rev_season_arimax_ur_fut, 2)

last_log_fut <- tail(log_GDP, 1)
rev_diff_arimax_ur_fut <- diffinv(rev_season_arimax_ur_fut, xi = last_log_fut, lag = 1)
rev_diff_arimax_ur_fut <- tail(rev_diff_arimax_ur_fut, 2)

pred_arimax_ur_future <- ts(exp(rev_diff_arimax_ur_fut), start = c(2022, 3), frequency = 4)

q3_arimax_ur <- as.numeric(pred_arimax_ur_future)[1]
q4_arimax_ur <- as.numeric(pred_arimax_ur_future)[2]

metricas_list[["ARIMAX_UR"]] <- list(
  MAE = mae_arimax_ur, RMSE = rmse_arimax_ur, MAPE = mape_arimax_ur,
  AICc = aicc_arimax_ur, BIC = bic_arimax_ur, Q3 = q3_arimax_ur, Q4 = q4_arimax_ur
)

# ====================================
# MODELO 3: ARIMAX con MS
# ====================================
cat("\n[MODELO 3] ARIMAX GDP + MS\n")

arimax_ms <- auto.arima(tend_train, xreg = as.matrix(ms_train), seasonal = FALSE,
                        stationary = FALSE, stepwise = FALSE, approximation = FALSE)

fc_arimax_ms <- forecast(arimax_ms, xreg = as.matrix(ms_test), h = h)
pred_t_arimax_ms <- fc_arimax_ms$mean

# Revertir transformaciones
rev_season_arimax_ms <- diffinv(pred_t_arimax_ms, xi = last_diff_regular, lag = 4)
rev_season_arimax_ms <- tail(rev_season_arimax_ms, h)

rev_diff_arimax_ms <- diffinv(rev_season_arimax_ms, xi = last_log, lag = 1)
rev_diff_arimax_ms <- tail(rev_diff_arimax_ms, h)

pred_arimax_ms_val <- ts(exp(rev_diff_arimax_ms), start = test_start, frequency = 4)

# Metricas validacion
mae_arimax_ms <- mean(abs(actual_val - pred_arimax_ms_val))
rmse_arimax_ms <- sqrt(mean((actual_val - pred_arimax_ms_val)^2))
mape_arimax_ms <- mean(abs((actual_val - pred_arimax_ms_val) / actual_val)) * 100
aicc_arimax_ms <- arimax_ms$aicc
bic_arimax_ms <- arimax_ms$bic

# Prediccion futura
ms_future_pred <- auto.arima(ms_full, seasonal = FALSE, stationary = FALSE,
                             stepwise = FALSE, approximation = FALSE)
fc_ms_future <- forecast(ms_future_pred, h = 2)
ms_future_transf <- fc_ms_future$mean

arimax_ms_full <- auto.arima(tend_full, xreg = as.matrix(ms_full), seasonal = FALSE,
                             stationary = FALSE, stepwise = FALSE, approximation = FALSE)
fc_arimax_ms_future <- forecast(arimax_ms_full, xreg = as.matrix(ms_future_transf), h = 2)
pred_t_arimax_ms_future <- fc_arimax_ms_future$mean

rev_season_arimax_ms_fut <- diffinv(pred_t_arimax_ms_future, xi = last_diff_full, lag = 4)
rev_season_arimax_ms_fut <- tail(rev_season_arimax_ms_fut, 2)

rev_diff_arimax_ms_fut <- diffinv(rev_season_arimax_ms_fut, xi = last_log_fut, lag = 1)
rev_diff_arimax_ms_fut <- tail(rev_diff_arimax_ms_fut, 2)

pred_arimax_ms_future <- ts(exp(rev_diff_arimax_ms_fut), start = c(2022, 3), frequency = 4)

q3_arimax_ms <- as.numeric(pred_arimax_ms_future)[1]
q4_arimax_ms <- as.numeric(pred_arimax_ms_future)[2]

metricas_list[["ARIMAX_MS"]] <- list(
  MAE = mae_arimax_ms, RMSE = rmse_arimax_ms, MAPE = mape_arimax_ms,
  AICc = aicc_arimax_ms, BIC = bic_arimax_ms, Q3 = q3_arimax_ms, Q4 = q4_arimax_ms
)

# ====================================
# MODELO 4: ARIMAX con MS + UR
# ====================================
cat("\n[MODELO 4] ARIMAX GDP + MS + UR\n")

xreg_train <- cbind(MS = as.numeric(ms_train), UR = as.numeric(ur_train))
xreg_test  <- cbind(MS = as.numeric(ms_test), UR = as.numeric(ur_test))

arimax_multi <- auto.arima(tend_train, xreg = xreg_train, seasonal = FALSE,
                           stationary = FALSE, stepwise = FALSE, approximation = FALSE)

fc_arimax_multi <- forecast(arimax_multi, xreg = xreg_test, h = h)
pred_t_arimax_multi <- fc_arimax_multi$mean

# Revertir transformaciones
rev_season_arimax_multi <- diffinv(pred_t_arimax_multi, xi = last_diff_regular, lag = 4)
rev_season_arimax_multi <- tail(rev_season_arimax_multi, h)

rev_diff_arimax_multi <- diffinv(rev_season_arimax_multi, xi = last_log, lag = 1)
rev_diff_arimax_multi <- tail(rev_diff_arimax_multi, h)

pred_arimax_multi_val <- ts(exp(rev_diff_arimax_multi), start = test_start, frequency = 4)

# Metricas validacion
mae_arimax_multi <- mean(abs(actual_val - pred_arimax_multi_val))
rmse_arimax_multi <- sqrt(mean((actual_val - pred_arimax_multi_val)^2))
mape_arimax_multi <- mean(abs((actual_val - pred_arimax_multi_val) / actual_val)) * 100
aicc_arimax_multi <- arimax_multi$aicc
bic_arimax_multi <- arimax_multi$bic

# Prediccion futura
xreg_future <- cbind(MS = as.numeric(ms_future_transf), UR = as.numeric(ur_future_transf))
xreg_full <- cbind(MS = as.numeric(ms_full), UR = as.numeric(ur_full))

arimax_multi_full <- auto.arima(tend_full, xreg = xreg_full, seasonal = FALSE,
                                stationary = FALSE, stepwise = FALSE, approximation = FALSE)
fc_arimax_multi_future <- forecast(arimax_multi_full, xreg = xreg_future, h = 2)
pred_t_arimax_multi_future <- fc_arimax_multi_future$mean

rev_season_arimax_multi_fut <- diffinv(pred_t_arimax_multi_future, xi = last_diff_full, lag = 4)
rev_season_arimax_multi_fut <- tail(rev_season_arimax_multi_fut, 2)

rev_diff_arimax_multi_fut <- diffinv(rev_season_arimax_multi_fut, xi = last_log_fut, lag = 1)
rev_diff_arimax_multi_fut <- tail(rev_diff_arimax_multi_fut, 2)

pred_arimax_multi_future <- ts(exp(rev_diff_arimax_multi_fut), start = c(2022, 3), frequency = 4)

q3_arimax_multi <- as.numeric(pred_arimax_multi_future)[1]
q4_arimax_multi <- as.numeric(pred_arimax_multi_future)[2]

metricas_list[["ARIMAX_MS_UR"]] <- list(
  MAE = mae_arimax_multi, RMSE = rmse_arimax_multi, MAPE = mape_arimax_multi,
  AICc = aicc_arimax_multi, BIC = bic_arimax_multi, Q3 = q3_arimax_multi, Q4 = q4_arimax_multi
)

# ====================================
# MODELO 5: VAR con MS
# ====================================
cat("\n[MODELO 5] VAR GDP + MS\n")

log_gdp <- log(outliers_GDP)
log_ms  <- log(outliers_MS)
log_ur  <- log(outliers_UR)

gdp_train <- window(log_gdp, start = train_start, end = train_end)
ms_train_var  <- window(log_ms, start = train_start, end = train_end)
ur_train_var  <- window(log_ur, start = train_start, end = train_end)

gdp_full <- window(log_gdp, start = train_start, end = test_end)
ms_full_var  <- window(log_ms, start = train_start, end = test_end)
ur_full_var  <- window(log_ur, start = train_start, end = test_end)

data_train_ms_var <- ts.union(GDP = gdp_train, MS = ms_train_var)
data_train_ms_var <- na.omit(data_train_ms_var)

lag_sel_ms_var <- VARselect(data_train_ms_var, lag.max = 8, type = "const")
optimal_lag_ms_var <- lag_sel_ms_var$selection["AIC(n)"]

var_model_ms <- VAR(data_train_ms_var, p = optimal_lag_ms_var, type = "const")

var_fc_ms_val <- predict(var_model_ms, n.ahead = 2)
pred_log_ms_val <- var_fc_ms_val$fcst$GDP[, "fcst"]
pred_var_ms_val <- ts(exp(pred_log_ms_val), start = test_start, frequency = 4)

mae_var_ms <- mean(abs(actual_val - pred_var_ms_val))
rmse_var_ms <- sqrt(mean((actual_val - pred_var_ms_val)^2))
mape_var_ms <- mean(abs((actual_val - pred_var_ms_val) / actual_val)) * 100

data_full_ms_var <- ts.union(GDP = gdp_full, MS = ms_full_var)
data_full_ms_var <- na.omit(data_full_ms_var)

lag_sel_ms_var_full <- VARselect(data_full_ms_var, lag.max = 8, type = "const")
var_model_ms_full <- VAR(data_full_ms_var, p = lag_sel_ms_var_full$selection["AIC(n)"], type = "const")

var_fc_ms_future <- predict(var_model_ms_full, n.ahead = 2)
pred_log_ms_future <- var_fc_ms_future$fcst$GDP[, "fcst"]
pred_var_ms_future <- ts(exp(pred_log_ms_future), start = c(2022, 3), frequency = 4)

q3_var_ms <- as.numeric(pred_var_ms_future)[1]
q4_var_ms <- as.numeric(pred_var_ms_future)[2]

aicc_var_ms <- NA
bic_var_ms <- NA

metricas_list[["VAR_MS"]] <- list(
  MAE = mae_var_ms, RMSE = rmse_var_ms, MAPE = mape_var_ms,
  AICc = aicc_var_ms, BIC = bic_var_ms, Q3 = q3_var_ms, Q4 = q4_var_ms
)

# ====================================
# MODELO 6: VAR con UR
# ====================================
cat("\n[MODELO 6] VAR GDP + UR\n")

data_train_ur_var <- ts.union(GDP = gdp_train, UR = ur_train_var)
data_train_ur_var <- na.omit(data_train_ur_var)

lag_sel_ur_var <- VARselect(data_train_ur_var, lag.max = 8, type = "const")
optimal_lag_ur_var <- lag_sel_ur_var$selection["AIC(n)"]

var_model_ur <- VAR(data_train_ur_var, p = optimal_lag_ur_var, type = "const")

var_fc_ur_val <- predict(var_model_ur, n.ahead = 2)
pred_log_ur_val <- var_fc_ur_val$fcst$GDP[, "fcst"]
pred_var_ur_val <- ts(exp(pred_log_ur_val), start = test_start, frequency = 4)

mae_var_ur <- mean(abs(actual_val - pred_var_ur_val))
rmse_var_ur <- sqrt(mean((actual_val - pred_var_ur_val)^2))
mape_var_ur <- mean(abs((actual_val - pred_var_ur_val) / actual_val)) * 100

data_full_ur_var <- ts.union(GDP = gdp_full, UR = ur_full_var)
data_full_ur_var <- na.omit(data_full_ur_var)

lag_sel_ur_var_full <- VARselect(data_full_ur_var, lag.max = 8, type = "const")
var_model_ur_full <- VAR(data_full_ur_var, p = lag_sel_ur_var_full$selection["AIC(n)"], type = "const")

var_fc_ur_future <- predict(var_model_ur_full, n.ahead = 2)
pred_log_ur_future <- var_fc_ur_future$fcst$GDP[, "fcst"]
pred_var_ur_future <- ts(exp(pred_log_ur_future), start = c(2022, 3), frequency = 4)

q3_var_ur <- as.numeric(pred_var_ur_future)[1]
q4_var_ur <- as.numeric(pred_var_ur_future)[2]

aicc_var_ur <- NA
bic_var_ur <- NA

metricas_list[["VAR_UR"]] <- list(
  MAE = mae_var_ur, RMSE = rmse_var_ur, MAPE = mape_var_ur,
  AICc = aicc_var_ur, BIC = bic_var_ur, Q3 = q3_var_ur, Q4 = q4_var_ur
)

# ====================================
# COMPARACION DE METRICAS
# ====================================
cat("\n\n")
cat(rep("=", 80), "\n", sep="")
cat("COMPARACION DE METRICAS DE TODOS LOS MODELOS\n")
cat(rep("=", 80), "\n\n", sep="")

tabla_comparacion <- data.frame(
  Modelo = names(metricas_list),
  MAE = sapply(metricas_list, function(x) round(x$MAE, 4)),
  RMSE = sapply(metricas_list, function(x) round(x$RMSE, 4)),
  MAPE = sapply(metricas_list, function(x) round(x$MAPE, 4)),
  AICc = sapply(metricas_list, function(x) ifelse(is.na(x$AICc), "N/A", round(x$AICc, 2))),
  BIC = sapply(metricas_list, function(x) ifelse(is.na(x$BIC), "N/A", round(x$BIC, 2))),
  Q3_2022 = sapply(metricas_list, function(x) round(x$Q3, 4)),
  Q4_2022 = sapply(metricas_list, function(x) round(x$Q4, 4))
)

print(tabla_comparacion)

# Identificar mejor modelo por cada metrica
cat("\n\nMejor desempeno por metrica:\n")
cat("Menor MAE:", tabla_comparacion$Modelo[which.min(as.numeric(tabla_comparacion$MAE))], "\n")
cat("Menor RMSE:", tabla_comparacion$Modelo[which.min(as.numeric(tabla_comparacion$RMSE))], "\n")
cat("Menor MAPE:", tabla_comparacion$Modelo[which.min(as.numeric(tabla_comparacion$MAPE))], "\n")

# ====================================
# GRAFICOS DE CADA MODELO
# ====================================
cat("\n\nGenerando graficos...\n")

serie_real <- window(outliers_GDP, start = c(2018, 1), end = test_end)

# Grafico 1: ARIMA
serie_arima <- ts(c(as.numeric(serie_real), as.numeric(pred_arima_future)),
                  start = start(serie_real), frequency = 4)
pred_arima_hl <- ts(tail(serie_arima, 2), start = c(2022, 3), frequency = 4)

g1 <- autoplot(serie_arima, series = "Serie", size = 1, color = "gray50") +
  autolayer(pred_arima_hl, series = "Prediccion ARIMA", color = "#1f77b4", size = 1.5) +
  ggtitle("MODELO 1: ARIMA") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafico 2: ARIMAX UR
serie_arimax_ur <- ts(c(as.numeric(serie_real), as.numeric(pred_arimax_ur_future)),
                      start = start(serie_real), frequency = 4)
pred_arimax_ur_hl <- ts(tail(serie_arimax_ur, 2), start = c(2022, 3), frequency = 4)

g2 <- autoplot(serie_arimax_ur, series = "Serie", size = 1, color = "gray50") +
  autolayer(pred_arimax_ur_hl, series = "Prediccion ARIMAX-UR", color = "#ff7f0e", size = 1.5) +
  ggtitle("MODELO 2: ARIMAX (GDP + UR)") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafico 3: ARIMAX MS
serie_arimax_ms <- ts(c(as.numeric(serie_real), as.numeric(pred_arimax_ms_future)),
                      start = start(serie_real), frequency = 4)
pred_arimax_ms_hl <- ts(tail(serie_arimax_ms, 2), start = c(2022, 3), frequency = 4)

g3 <- autoplot(serie_arimax_ms, series = "Serie", size = 1, color = "gray50") +
  autolayer(pred_arimax_ms_hl, series = "Prediccion ARIMAX-MS", color = "#2ca02c", size = 1.5) +
  ggtitle("MODELO 3: ARIMAX (GDP + MS)") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafico 4: ARIMAX MS+UR
serie_arimax_multi <- ts(c(as.numeric(serie_real), as.numeric(pred_arimax_multi_future)),
                         start = start(serie_real), frequency = 4)
pred_arimax_multi_hl <- ts(tail(serie_arimax_multi, 2), start = c(2022, 3), frequency = 4)

g4 <- autoplot(serie_arimax_multi, series = "Serie", size = 1, color = "gray50") +
  autolayer(pred_arimax_multi_hl, series = "Prediccion ARIMAX-MS+UR", color = "#d62728", size = 1.5) +
  ggtitle("MODELO 4: ARIMAX (GDP + MS + UR)") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafico 5: VAR MS
serie_var_ms <- ts(c(as.numeric(serie_real), as.numeric(pred_var_ms_future)),
                   start = start(serie_real), frequency = 4)
pred_var_ms_hl <- ts(tail(serie_var_ms, 2), start = c(2022, 3), frequency = 4)

g5 <- autoplot(serie_var_ms, series = "Serie", size = 1, color = "gray50") +
  autolayer(pred_var_ms_hl, series = "Prediccion VAR-MS", color = "#17becf", size = 1.5) +
  ggtitle("MODELO 5: VAR (GDP + MS)") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafico 6: VAR UR
serie_var_ur <- ts(c(as.numeric(serie_real), as.numeric(pred_var_ur_future)),
                   start = start(serie_real), frequency = 4)
pred_var_ur_hl <- ts(tail(serie_var_ur, 2), start = c(2022, 3), frequency = 4)

g6 <- autoplot(serie_var_ur, series = "Serie", size = 1, color = "gray50") +
  autolayer(pred_var_ur_hl, series = "Prediccion VAR-UR", color = "#8c564b", size = 1.5) +
  ggtitle("MODELO 6: VAR (GDP + UR)") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

# Grafico comparativo
g_comp <- autoplot(serie_real, series = "GDP Real", size = 1, color = "black") +
  autolayer(pred_arima_hl, series = "ARIMA", color = "#1f77b4", size = 1.2, linetype = "solid") +
  autolayer(pred_arimax_ur_hl, series = "ARIMAX-UR", color = "#ff7f0e", size = 1.2, linetype = "dashed") +
  autolayer(pred_arimax_ms_hl, series = "ARIMAX-MS", color = "#2ca02c", size = 1.2, linetype = "dotdash") +
  autolayer(pred_arimax_multi_hl, series = "ARIMAX-MS+UR", color = "#d62728", size = 1.2, linetype = "longdash") +
  autolayer(pred_var_ms_hl, series = "VAR-MS", color = "#17becf", size = 1.2, linetype = "twodash") +
  autolayer(pred_var_ur_hl, series = "VAR-UR", color = "#8c564b", size = 1.2, linetype = "dotted") +
  ggtitle("Comparacion de Predicciones - Todos los Modelos") +
  xlab("Trimestre") + ylab("GDP") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"))

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
print(g6)
print(g_comp)




###################### IPC ######################




# ARIMA CPI

# Rango temporal
start_year_arima <- 2013
start_quarter_arima <- 1
end_year_arima <- 2021
end_quarter_arima <- 4

test_start_arima <- c(2022, 1)
test_end_arima   <- c(2022, 2)

# Entrenamiento y validación
cpi_train_arima <- window(tendencia_CPI, start = c(start_year_arima, start_quarter_arima), end = c(end_year_arima, end_quarter_arima))
cpi_test_arima  <- window(tendencia_CPI, start = test_start_arima, end = test_end_arima)
h_arima <- length(cpi_test_arima)

# Ajuste ARIMA
model_arima <- auto.arima(
  cpi_train_arima,
  seasonal = FALSE,
  stationary = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(model_arima)
checkresiduals(model_arima)

# Predicción validación
fc_arima <- forecast(model_arima, h = h_arima)
pred_transf_val_arima <- fc_arima$mean

# Revertir diferencia estacional
last_diff_val_arima <- tail(diff(log_CPI, differences = 2), 4)
pred_rev_season_val_arima <- diffinv(pred_transf_val_arima, xi = last_diff_val_arima, lag = 4)
pred_rev_season_val_arima <- head(pred_rev_season_val_arima, h_arima)

# Revertir diferencia regular
last_log_val_arima <- tail(log_CPI, 1)
pred_rev_diff_val_arima <- diffinv(pred_rev_season_val_arima, xi = last_log_val_arima, lag = 1)
pred_rev_diff_val_arima <- tail(pred_rev_diff_val_arima, h_arima)

# Revertir log
pred_val_ts_arima <- ts(exp(pred_rev_diff_val_arima), start = test_start_arima, frequency = frequency(outliers_CPI))

# Predicción futura
cpi_train_full_arima <- window(tendencia_CPI, start = c(start_year_arima, start_quarter_arima), end = test_end_arima)
model_arima_future <- auto.arima(cpi_train_full_arima,
                                 seasonal = FALSE,
                                 stationary = FALSE,
                                 stepwise = FALSE,
                                 approximation = FALSE)

fc_arima_future <- forecast(model_arima_future, h = 2)
pred_transf_future_arima <- fc_arima_future$mean

# Revertir transformaciones
last_diff_future_arima <- tail(diff(log_CPI, differences = 2), 4)
pred_rev_season_future_arima <- diffinv(pred_transf_future_arima, xi = last_diff_future_arima, lag = 4)
pred_rev_season_future_arima <- head(pred_rev_season_future_arima, 2)

last_log_future_arima <- tail(log_CPI, 1)
pred_rev_diff_future_arima <- diffinv(pred_rev_season_future_arima, xi = last_log_future_arima, lag = 1)
pred_rev_diff_future_arima <- tail(pred_rev_diff_future_arima, 2)

pred_future_ts_arima <- ts(exp(pred_rev_diff_future_arima), start = c(2022,3), frequency = frequency(outliers_CPI))

# Métricas
actual_cpi_val_arima <- window(outliers_CPI, start = test_start_arima, end = test_end_arima)

MAE_cpi_val_arima  <- mean(abs(actual_cpi_val_arima - pred_val_ts_arima))
RMSE_cpi_val_arima <- sqrt(mean((actual_cpi_val_arima - pred_val_ts_arima)^2))
MAPE_cpi_val_arima <- mean(abs((actual_cpi_val_arima - pred_val_ts_arima) / actual_cpi_val_arima)) * 100

print("ARIMA CPI - Validación")
print(paste("MAE :", round(MAE_cpi_val_arima, 2)))
print(paste("RMSE:", round(RMSE_cpi_val_arima, 2)))
print(paste("MAPE:", round(MAPE_cpi_val_arima, 2), "%"))


# SARIMA CPI

# Rango temporal
start_year_sarima <- 2013
start_quarter_sarima <- 1
end_year_sarima <- 2021
end_quarter_sarima <- 4

test_start_sarima <- c(2022, 1)
test_end_sarima   <- c(2022, 2)

# Entrenamiento y validación
cpi_train_sarima <- window(tendencia_CPI, start = c(start_year_sarima, start_quarter_sarima), end = c(end_year_sarima, end_quarter_sarima))
cpi_test_sarima  <- window(tendencia_CPI, start = test_start_sarima, end = test_end_sarima)
h_sarima <- length(cpi_test_sarima)

# Ajuste SARIMA
model_sarima <- auto.arima(
  cpi_train_sarima,
  seasonal = TRUE,
  stationary = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(model_sarima)
checkresiduals(model_sarima)

# Predicción validación
fc_sarima <- forecast(model_sarima, h = h_sarima)
pred_transf_val_sarima <- fc_sarima$mean

# Revertir diferencia estacional
last_diff_val_sarima <- tail(diff(log_CPI, differences = 2), 4)
pred_rev_season_val_sarima <- diffinv(pred_transf_val_sarima, xi = last_diff_val_sarima, lag = 4)
pred_rev_season_val_sarima <- head(pred_rev_season_val_sarima, h_sarima)

# Revertir diferencia regular
last_log_val_sarima <- tail(log_CPI, 1)
pred_rev_diff_val_sarima <- diffinv(pred_rev_season_val_sarima, xi = last_log_val_sarima, lag = 1)
pred_rev_diff_val_sarima <- tail(pred_rev_diff_val_sarima, h_sarima)

# Revertir log
pred_val_ts_sarima <- ts(exp(pred_rev_diff_val_sarima), start = test_start_sarima, frequency = frequency(outliers_CPI))

# Predicción futura
cpi_train_full_sarima <- window(tendencia_CPI, start = c(start_year_sarima, start_quarter_sarima), end = test_end_sarima)
model_sarima_future <- auto.arima(cpi_train_full_sarima,
                                  seasonal = TRUE,
                                  stationary = FALSE,
                                  stepwise = FALSE,
                                  approximation = FALSE)

fc_sarima_future <- forecast(model_sarima_future, h = 2)
pred_transf_future_sarima <- fc_sarima_future$mean

# Revertir transformaciones
last_diff_future_sarima <- tail(diff(log_CPI, differences = 2), 4)
pred_rev_season_future_sarima <- diffinv(pred_transf_future_sarima, xi = last_diff_future_sarima, lag = 4)
pred_rev_season_future_sarima <- head(pred_rev_season_future_sarima, 2)

last_log_future_sarima <- tail(log_CPI, 1)
pred_rev_diff_future_sarima <- diffinv(pred_rev_season_future_sarima, xi = last_log_future_sarima, lag = 1)
pred_rev_diff_future_sarima <- tail(pred_rev_diff_future_sarima, 2)

pred_future_ts_sarima <- ts(exp(pred_rev_diff_future_sarima), start = c(2022,3), frequency = frequency(outliers_CPI))

# Métricas
actual_cpi_val_sarima <- window(outliers_CPI, start = test_start_sarima, end = test_end_sarima)

MAE_cpi_val_sarima  <- mean(abs(actual_cpi_val_sarima - pred_val_ts_sarima))
RMSE_cpi_val_sarima <- sqrt(mean((actual_cpi_val_sarima - pred_val_ts_sarima)^2))
MAPE_cpi_val_sarima <- mean(abs((actual_cpi_val_sarima - pred_val_ts_sarima) / actual_cpi_val_sarima)) * 100

print("SARIMA CPI - Validación")
print(paste("MAE :", round(MAE_cpi_val_sarima, 2)))
print(paste("RMSE:", round(RMSE_cpi_val_sarima, 2)))
print(paste("MAPE:", round(MAPE_cpi_val_sarima, 2), "%"))


# ARIMAX CPI

# Configuración de fechas
ms_start <- start(diff_boxcox_MS)
ur_start <- start(diff_boxcox_UR)

train_start <- pmax(c(2013, 1), ms_start, ur_start)
train_end   <- c(2021, 4)
test_start  <- c(2022, 1)
test_end    <- c(2022, 2)

# Series alineadas
cpi_train <- window(tendencia_CPI, start = train_start, end = train_end)
cpi_test  <- window(tendencia_CPI, start = test_start, end = test_end)
cpi_full  <- window(tendencia_CPI, start = train_start, end = test_end)

ms_train <- window(diff_boxcox_MS, start = train_start, end = train_end)
ms_test  <- window(diff_boxcox_MS, start = test_start, end = test_end)
ms_full  <- window(diff_boxcox_MS, start = train_start, end = test_end)

ur_train <- window(diff_boxcox_UR, start = train_start, end = train_end)
ur_test  <- window(diff_boxcox_UR, start = test_start, end = test_end)
ur_full  <- window(diff_boxcox_UR, start = train_start, end = test_end)

h <- length(cpi_test)

# ARIMAX 1: CPI con MS

print("ARIMAX 1: CPI con MS")

# Predecir MS
arima_ms_pred <- auto.arima(ms_full, seasonal = FALSE, 
                            stationary = FALSE, stepwise = FALSE, 
                            approximation = FALSE)
fc_ms_future <- forecast(arima_ms_pred, h = 2)
ms_future_transf <- fc_ms_future$mean

# Modelo validación
arimax_cpi_ms_val <- auto.arima(cpi_train,
                                xreg = as.matrix(ms_train),
                                seasonal = FALSE,
                                stationary = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)

summary(arimax_cpi_ms_val)
checkresiduals(arimax_cpi_ms_val)

# Predicción validación
arimax_fc_ms_val <- forecast(arimax_cpi_ms_val, xreg = as.matrix(ms_test), h = h)
pred_t_ms_val <- arimax_fc_ms_val$mean

# Revertir transformaciones
serie_diff2 <- diff(log_CPI, differences = 2)
last_diff2 <- tail(serie_diff2, 4)

rev_season_ms_val <- diffinv(pred_t_ms_val, xi = last_diff2, lag = 4)
rev_season_ms_val <- tail(rev_season_ms_val, h)

last_log <- tail(log_CPI, 1)
rev_diff_ms_val <- diffinv(rev_season_ms_val, xi = last_log, lag = 1)
rev_diff_ms_val <- tail(rev_diff_ms_val, h)

pred_cpi_ms_val_ts <- ts(exp(rev_diff_ms_val), start = test_start, frequency = 4)

# Métricas
actual_cpi_val <- window(outliers_CPI, start = test_start, end = test_end)

MAE_cpi_ms_val  <- mean(abs(actual_cpi_val - pred_cpi_ms_val_ts))
RMSE_cpi_ms_val <- sqrt(mean((actual_cpi_val - pred_cpi_ms_val_ts)^2))
MAPE_cpi_ms_val <- mean(abs((actual_cpi_val - pred_cpi_ms_val_ts) / actual_cpi_val)) * 100

print("ARIMAX CPI-MS Validación")
print(paste("MAE :", round(MAE_cpi_ms_val, 2)))
print(paste("RMSE:", round(RMSE_cpi_ms_val, 2)))
print(paste("MAPE:", round(MAPE_cpi_ms_val, 2), "%"))

# Predicción futura
arimax_cpi_ms_full <- auto.arima(cpi_full,
                                 xreg = as.matrix(ms_full),
                                 seasonal = FALSE,
                                 stationary = FALSE,
                                 stepwise = FALSE,
                                 approximation = FALSE)

arimax_fc_ms_future <- forecast(arimax_cpi_ms_full, 
                                xreg = as.matrix(ms_future_transf), 
                                h = 2)
pred_t_ms_future <- arimax_fc_ms_future$mean

rev_season_ms_fut <- diffinv(pred_t_ms_future, xi = last_diff2, lag = 4)
rev_season_ms_fut <- tail(rev_season_ms_fut, 2)

rev_diff_ms_fut <- diffinv(rev_season_ms_fut, xi = last_log, lag = 1)
rev_diff_ms_fut <- tail(rev_diff_ms_fut, 2)

pred_cpi_ms_future_ts <- ts(exp(rev_diff_ms_fut), start = c(2022, 3), frequency = 4)

print(pred_cpi_ms_future_ts)

# ARIMAX 2: CPI con UR

print("ARIMAX 2: CPI con UR")

# Predecir UR
arima_ur_pred <- auto.arima(ur_full, seasonal = FALSE, 
                            stationary = FALSE, stepwise = FALSE, 
                            approximation = FALSE)
fc_ur_future <- forecast(arima_ur_pred, h = 2)
ur_future_transf <- fc_ur_future$mean

# Modelo validación
arimax_cpi_ur_val <- auto.arima(cpi_train,
                                xreg = as.matrix(ur_train),
                                seasonal = FALSE,
                                stationary = FALSE,
                                stepwise = FALSE,
                                approximation = FALSE)

summary(arimax_cpi_ur_val)
checkresiduals(arimax_cpi_ur_val)

# Predicción validación
arimax_fc_ur_val <- forecast(arimax_cpi_ur_val, xreg = as.matrix(ur_test), h = h)
pred_t_ur_val <- arimax_fc_ur_val$mean

rev_season_ur_val <- diffinv(pred_t_ur_val, xi = last_diff2, lag = 4)
rev_season_ur_val <- tail(rev_season_ur_val, h)

rev_diff_ur_val <- diffinv(rev_season_ur_val, xi = last_log, lag = 1)
rev_diff_ur_val <- tail(rev_diff_ur_val, h)

pred_cpi_ur_val_ts <- ts(exp(rev_diff_ur_val), start = test_start, frequency = 4)

# Métricas
MAE_cpi_ur_val  <- mean(abs(actual_cpi_val - pred_cpi_ur_val_ts))
RMSE_cpi_ur_val <- sqrt(mean((actual_cpi_val - pred_cpi_ur_val_ts)^2))
MAPE_cpi_ur_val <- mean(abs((actual_cpi_val - pred_cpi_ur_val_ts) / actual_cpi_val)) * 100

print("ARIMAX CPI-UR Validación")
print(paste("MAE :", round(MAE_cpi_ur_val, 2)))
print(paste("RMSE:", round(RMSE_cpi_ur_val, 2)))
print(paste("MAPE:", round(MAPE_cpi_ur_val, 2), "%"))

# Predicción futura
arimax_cpi_ur_full <- auto.arima(cpi_full,
                                 xreg = as.matrix(ur_full),
                                 seasonal = FALSE,
                                 stationary = FALSE,
                                 stepwise = FALSE,
                                 approximation = FALSE)

arimax_fc_ur_future <- forecast(arimax_cpi_ur_full, 
                                xreg = as.matrix(ur_future_transf), 
                                h = 2)
pred_t_ur_future <- arimax_fc_ur_future$mean

rev_season_ur_fut <- diffinv(pred_t_ur_future, xi = last_diff2, lag = 4)
rev_season_ur_fut <- tail(rev_season_ur_fut, 2)

rev_diff_ur_fut <- diffinv(rev_season_ur_fut, xi = last_log, lag = 1)
rev_diff_ur_fut <- tail(rev_diff_ur_fut, 2)

pred_cpi_ur_future_ts <- ts(exp(rev_diff_ur_fut), start = c(2022, 3), frequency = 4)

print(pred_cpi_ur_future_ts)

# ARIMAX 3: CPI con MS + UR

print("ARIMAX 3: CPI con MS + UR")

# Matrices de regresores
xreg_train <- cbind(MS = as.numeric(ms_train), 
                    UR = as.numeric(ur_train))

xreg_test  <- cbind(MS = as.numeric(ms_test), 
                    UR = as.numeric(ur_test))

xreg_full  <- cbind(MS = as.numeric(ms_full), 
                    UR = as.numeric(ur_full))

xreg_future <- cbind(MS = as.numeric(ms_future_transf), 
                     UR = as.numeric(ur_future_transf))

# Modelo validación
arimax_cpi_multi_val <- auto.arima(cpi_train,
                                   xreg = xreg_train,
                                   seasonal = FALSE,
                                   stationary = FALSE,
                                   stepwise = FALSE,
                                   approximation = FALSE)

summary(arimax_cpi_multi_val)
checkresiduals(arimax_cpi_multi_val)

# Predicción validación
arimax_fc_multi_val <- forecast(arimax_cpi_multi_val, xreg = xreg_test, h = h)
pred_t_multi_val <- arimax_fc_multi_val$mean

rev_season_multi_val <- diffinv(pred_t_multi_val, xi = last_diff2, lag = 4)
rev_season_multi_val <- tail(rev_season_multi_val, h)

rev_diff_multi_val <- diffinv(rev_season_multi_val, xi = last_log, lag = 1)
rev_diff_multi_val <- tail(rev_diff_multi_val, h)

pred_cpi_multi_val_ts <- ts(exp(rev_diff_multi_val), start = test_start, frequency = 4)

# Métricas
MAE_cpi_multi_val  <- mean(abs(actual_cpi_val - pred_cpi_multi_val_ts))
RMSE_cpi_multi_val <- sqrt(mean((actual_cpi_val - pred_cpi_multi_val_ts)^2))
MAPE_cpi_multi_val <- mean(abs((actual_cpi_val - pred_cpi_multi_val_ts) / actual_cpi_val)) * 100

print("ARIMAX CPI-MS+UR Validación")
print(paste("MAE :", round(MAE_cpi_multi_val, 2)))
print(paste("RMSE:", round(RMSE_cpi_multi_val, 2)))
print(paste("MAPE:", round(MAPE_cpi_multi_val, 2), "%"))

# Predicción futura
arimax_cpi_multi_full <- auto.arima(cpi_full,
                                    xreg = xreg_full,
                                    seasonal = FALSE,
                                    stationary = FALSE,
                                    stepwise = FALSE,
                                    approximation = FALSE)

arimax_fc_multi_future <- forecast(arimax_cpi_multi_full, 
                                   xreg = xreg_future, 
                                   h = 2)
pred_t_multi_future <- arimax_fc_multi_future$mean

rev_season_multi_fut <- diffinv(pred_t_multi_future, xi = last_diff2, lag = 4)
rev_season_multi_fut <- tail(rev_season_multi_fut, 2)

rev_diff_multi_fut <- diffinv(rev_season_multi_fut, xi = last_log, lag = 1)
rev_diff_multi_fut <- tail(rev_diff_multi_fut, 2)

pred_cpi_multi_future_ts <- ts(exp(rev_diff_multi_fut), start = c(2022, 3), frequency = 4)

print(pred_cpi_multi_future_ts)

# Comparación ARIMAX
comparacion_arimax_cpi <- data.frame(
  Modelo = c("ARIMAX CPI-MS", "ARIMAX CPI-UR", "ARIMAX CPI-MS+UR"),
  MAE = c(MAE_cpi_ms_val, MAE_cpi_ur_val, MAE_cpi_multi_val),
  RMSE = c(RMSE_cpi_ms_val, RMSE_cpi_ur_val, RMSE_cpi_multi_val),
  MAPE = c(MAPE_cpi_ms_val, MAPE_cpi_ur_val, MAPE_cpi_multi_val),
  Q3_2022 = c(as.numeric(pred_cpi_ms_future_ts)[1], 
              as.numeric(pred_cpi_ur_future_ts)[1], 
              as.numeric(pred_cpi_multi_future_ts)[1]),
  Q4_2022 = c(as.numeric(pred_cpi_ms_future_ts)[2], 
              as.numeric(pred_cpi_ur_future_ts)[2], 
              as.numeric(pred_cpi_multi_future_ts)[2])
)

print(comparacion_arimax_cpi)


# VAR

# Configuración
train_start <- c(2014, 1)
train_end   <- c(2021, 4)
test_start  <- c(2022, 1)
test_end    <- c(2022, 2)

# Series tratadas
cpi_train <- window(tendencia_CPI, start = train_start, end = train_end)
ms_train  <- window(diff_boxcox_MS, start = train_start, end = train_end)
ur_train  <- window(diff_boxcox_UR, start = train_start, end = train_end)

cpi_full <- window(tendencia_CPI, start = train_start, end = test_end)
ms_full  <- window(diff_boxcox_MS, start = train_start, end = test_end)
ur_full  <- window(diff_boxcox_UR, start = train_start, end = test_end)

# Valores para reversión
log_cpi_full <- log(outliers_CPI)
diff1_cpi_full <- diff(log_cpi_full, differences = 1)
last_log_cpi <- tail(log_cpi_full, 1)
last_4_diff1_cpi <- tail(diff1_cpi_full, 4)

log_ms_full <- log(outliers_MS)
diff1_ms_full <- diff(log_ms_full, differences = 1)
diff2_ms_full <- diff(log_ms_full, differences = 2)
last_log_ms <- tail(log_ms_full, 1)
last_diff1_ms <- tail(diff1_ms_full, 1)
last_diff2_ms <- tail(diff2_ms_full, 1)
last_4_diff2_ms <- tail(diff2_ms_full, 4)

log_ur_full <- log(outliers_UR)
diff1_ur_full <- diff(log_ur_full, differences = 1)
diff2_ur_full <- diff(log_ur_full, differences = 2)
last_log_ur <- tail(log_ur_full, 1)
last_diff1_ur <- tail(diff1_ur_full, 1)

# Función revertir CPI
revertir_cpi <- function(pred_diff2) {
  n_pred <- length(pred_diff2)
  
  rev_diff1 <- numeric(n_pred + 4)
  rev_diff1[1:4] <- last_4_diff1_cpi
  for (i in 1:n_pred) {
    rev_diff1[i + 4] <- rev_diff1[i] + pred_diff2[i]
  }
  rev_diff1 <- tail(rev_diff1, n_pred)
  
  rev_log <- numeric(n_pred + 1)
  rev_log[1] <- last_log_cpi
  for (i in 1:n_pred) {
    rev_log[i + 1] <- rev_log[i] + rev_diff1[i]
  }
  rev_log <- tail(rev_log, n_pred)
  
  exp(rev_log)
}

# VAR 1: CPI + MS

print("VAR 1: CPI + MS")

data_train_ms <- ts.union(CPI = cpi_train, MS = ms_train)
data_train_ms <- na.omit(data_train_ms)

lag_sel_ms <- VARselect(data_train_ms, lag.max = 4, type = "const")
optimal_lag_ms <- as.numeric(lag_sel_ms$selection["AIC(n)"])
print(paste("Lag óptimo:", optimal_lag_ms))

var_ms <- VAR(data_train_ms, p = optimal_lag_ms, type = "const")
summary(var_ms)

# Predicción validación
var_pred_ms_val <- predict(var_ms, n.ahead = 2)
pred_cpi_ms_val_transf <- var_pred_ms_val$fcst$CPI[, "fcst"]

pred_cpi_ms_val <- revertir_cpi(pred_cpi_ms_val_transf)
pred_ms_val_ts <- ts(pred_cpi_ms_val, start = test_start, frequency = 4)

# Métricas
actual_val <- window(outliers_CPI, start = test_start, end = test_end)
MAE_ms <- mean(abs(actual_val - pred_ms_val_ts))
RMSE_ms <- sqrt(mean((actual_val - pred_ms_val_ts)^2))
MAPE_ms <- mean(abs((actual_val - pred_ms_val_ts) / actual_val)) * 100

print("VAR CPI+MS Validación")
print(paste("MAE :", round(MAE_ms, 4)))
print(paste("RMSE:", round(RMSE_ms, 4)))
print(paste("MAPE:", round(MAPE_ms, 4), "%"))

# Predicción futura
data_full_ms <- ts.union(CPI = cpi_full, MS = ms_full)
data_full_ms <- na.omit(data_full_ms)

lag_sel_ms_full <- VARselect(data_full_ms, lag.max = 4, type = "const")
optimal_lag_ms_full <- as.numeric(lag_sel_ms_full$selection["AIC(n)"])
var_ms_full <- VAR(data_full_ms, p = optimal_lag_ms_full, type = "const")

var_pred_ms_future <- predict(var_ms_full, n.ahead = 2)
pred_cpi_ms_future_transf <- var_pred_ms_future$fcst$CPI[, "fcst"]

pred_cpi_ms_future <- revertir_cpi(pred_cpi_ms_future_transf)
pred_ms_future_ts <- ts(pred_cpi_ms_future, start = c(2022, 3), frequency = 4)

print(pred_ms_future_ts)

# VAR 2: CPI + UR

print("VAR 2: CPI + UR")

data_train_ur <- ts.union(CPI = cpi_train, UR = ur_train)
data_train_ur <- na.omit(data_train_ur)

lag_sel_ur <- VARselect(data_train_ur, lag.max = 4, type = "const")
optimal_lag_ur <- as.numeric(lag_sel_ur$selection["AIC(n)"])
print(paste("Lag óptimo:", optimal_lag_ur))

var_ur <- VAR(data_train_ur, p = optimal_lag_ur, type = "const")
summary(var_ur)

# Predicción validación
var_pred_ur_val <- predict(var_ur, n.ahead = 2)
pred_cpi_ur_val_transf <- var_pred_ur_val$fcst$CPI[, "fcst"]

pred_cpi_ur_val <- revertir_cpi(pred_cpi_ur_val_transf)
pred_ur_val_ts <- ts(pred_cpi_ur_val, start = test_start, frequency = 4)

# Métricas
MAE_ur <- mean(abs(actual_val - pred_ur_val_ts))
RMSE_ur <- sqrt(mean((actual_val - pred_ur_val_ts)^2))
MAPE_ur <- mean(abs((actual_val - pred_ur_val_ts) / actual_val)) * 100

print("VAR CPI+UR Validación")
print(paste("MAE :", round(MAE_ur, 4)))
print(paste("RMSE:", round(RMSE_ur, 4)))
print(paste("MAPE:", round(MAPE_ur, 4), "%"))

# Predicción futura
data_full_ur <- ts.union(CPI = cpi_full, UR = ur_full)
data_full_ur <- na.omit(data_full_ur)

lag_sel_ur_full <- VARselect(data_full_ur, lag.max = 4, type = "const")
optimal_lag_ur_full <- as.numeric(lag_sel_ur_full$selection["AIC(n)"])
var_ur_full <- VAR(data_full_ur, p = optimal_lag_ur_full, type = "const")

var_pred_ur_future <- predict(var_ur_full, n.ahead = 2)
pred_cpi_ur_future_transf <- var_pred_ur_future$fcst$CPI[, "fcst"]

pred_cpi_ur_future <- revertir_cpi(pred_cpi_ur_future_transf)
pred_ur_future_ts <- ts(pred_cpi_ur_future, start = c(2022, 3), frequency = 4)

print(pred_ur_future_ts)

# VAR 3: CPI + MS + UR

print("VAR 3: CPI + MS + UR")

data_train_multi <- ts.union(CPI = cpi_train, MS = ms_train, UR = ur_train)
data_train_multi <- na.omit(data_train_multi)

lag_sel_multi <- VARselect(data_train_multi, lag.max = 4, type = "const")
optimal_lag_multi <- as.numeric(lag_sel_multi$selection["AIC(n)"])
print(paste("Lag óptimo:", optimal_lag_multi))

var_multi <- VAR(data_train_multi, p = optimal_lag_multi, type = "const")
summary(var_multi)

# Predicción validación
var_pred_multi_val <- predict(var_multi, n.ahead = 2)
pred_cpi_multi_val_transf <- var_pred_multi_val$fcst$CPI[, "fcst"]

pred_cpi_multi_val <- revertir_cpi(pred_cpi_multi_val_transf)
pred_multi_val_ts <- ts(pred_cpi_multi_val, start = test_start, frequency = 4)

# Métricas
MAE_multi <- mean(abs(actual_val - pred_multi_val_ts))
RMSE_multi <- sqrt(mean((actual_val - pred_multi_val_ts)^2))
MAPE_multi <- mean(abs((actual_val - pred_multi_val_ts) / actual_val)) * 100

print("VAR CPI+MS+UR Validación")
print(paste("MAE :", round(MAE_multi, 4)))
print(paste("RMSE:", round(RMSE_multi, 4)))
print(paste("MAPE:", round(MAPE_multi, 4), "%"))

# Predicción futura
data_full_multi <- ts.union(CPI = cpi_full, MS = ms_full, UR = ur_full)
data_full_multi <- na.omit(data_full_multi)

lag_sel_multi_full <- VARselect(data_full_multi, lag.max = 4, type = "const")
optimal_lag_multi_full <- as.numeric(lag_sel_multi_full$selection["AIC(n)"])
var_multi_full <- VAR(data_full_multi, p = optimal_lag_multi_full, type = "const")

var_pred_multi_future <- predict(var_multi_full, n.ahead = 2)
pred_cpi_multi_future_transf <- var_pred_multi_future$fcst$CPI[, "fcst"]

pred_cpi_multi_future <- revertir_cpi(pred_cpi_multi_future_transf)
pred_multi_future_ts <- ts(pred_cpi_multi_future, start = c(2022, 3), frequency = 4)

print(pred_multi_future_ts)

# Tablas comparativas
comparacion_var <- data.frame(
  Modelo = c("VAR CPI+MS", "VAR CPI+UR", "VAR CPI+MS+UR"),
  MAE = c(MAE_ms, MAE_ur, MAE_multi),
  RMSE = c(RMSE_ms, RMSE_ur, RMSE_multi),
  MAPE = c(MAPE_ms, MAPE_ur, MAPE_multi),
  Q3_2022 = c(as.numeric(pred_ms_future_ts)[1], 
              as.numeric(pred_ur_future_ts)[1],
              as.numeric(pred_multi_future_ts)[1]),
  Q4_2022 = c(as.numeric(pred_ms_future_ts)[2], 
              as.numeric(pred_ur_future_ts)[2],
              as.numeric(pred_multi_future_ts)[2])
)

print(comparacion_var)

# Resumen general
metrics_df <- data.frame(
  Modelo = c("ARIMA CPI",
             "SARIMA CPI",
             "ARIMAX CPI-MS",
             "ARIMAX CPI-UR",
             "ARIMAX CPI-MS+UR",
             "VAR CPI+MS",
             "VAR CPI+UR",
             "VAR CPI+MS+UR"),
  
  MAE  = c(MAE_cpi_val_arima,
           MAE_cpi_val_sarima,
           MAE_cpi_ms_val,
           MAE_cpi_ur_val,
           MAE_cpi_multi_val,
           MAE_ms,
           MAE_ur,
           MAE_multi),
  
  RMSE = c(RMSE_cpi_val_arima,
           RMSE_cpi_val_sarima,
           RMSE_cpi_ms_val,
           RMSE_cpi_ur_val,
           RMSE_cpi_multi_val,
           RMSE_ms,
           RMSE_ur,
           RMSE_multi),
  
  MAPE = c(MAPE_cpi_val_arima,
           MAPE_cpi_val_sarima,
           MAPE_cpi_ms_val,
           MAPE_cpi_ur_val,
           MAPE_cpi_multi_val,
           MAPE_ms,
           MAPE_ur,
           MAPE_multi),
  
  Q3_2022 = c(as.numeric(pred_future_ts_arima)[1],
              as.numeric(pred_future_ts_sarima)[1],
              as.numeric(pred_cpi_ms_future_ts)[1],
              as.numeric(pred_cpi_ur_future_ts)[1],
              as.numeric(pred_cpi_multi_future_ts)[1],
              as.numeric(pred_ms_future_ts)[1],
              as.numeric(pred_ur_future_ts)[1],
              as.numeric(pred_multi_future_ts)[1]),
  
  Q4_2022 = c(as.numeric(pred_future_ts_arima)[2],
              as.numeric(pred_future_ts_sarima)[2],
              as.numeric(pred_cpi_ms_future_ts)[2],
              as.numeric(pred_cpi_ur_future_ts)[2],
              as.numeric(pred_cpi_multi_future_ts)[2],
              as.numeric(pred_ms_future_ts)[2],
              as.numeric(pred_ur_future_ts)[2],
              as.numeric(pred_multi_future_ts)[2])
)

print(metrics_df)

# Gráficos
serie_real <- window(outliers_CPI, start = c(2018, 1), end = test_end)

# ARIMA
serie_combinada_arima <- ts(
  c(as.numeric(serie_real), as.numeric(pred_future_ts_arima)),
  start = c(2018, 1), frequency = 4
)
pred_future_highlight_arima <- ts(tail(serie_combinada_arima, 2), start = c(2022, 3), frequency = 4)

p1 <- autoplot(serie_combinada_arima, size = 1, color = "gray30") +
  autolayer(pred_future_highlight_arima, series = "ARIMA", color = "#0072B2", size = 1.8) +
  ggtitle("ARIMA CPI") +
  xlab("Año") + ylab("CPI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

print(p1)

# SARIMA
serie_combinada_sarima <- ts(
  c(as.numeric(serie_real), as.numeric(pred_future_ts_sarima)),
  start = c(2018, 1), frequency = 4
)
pred_future_highlight_sarima <- ts(tail(serie_combinada_sarima, 2), start = c(2022, 3), frequency = 4)

p2 <- autoplot(serie_combinada_sarima, size = 1, color = "gray30") +
  autolayer(pred_future_highlight_sarima, series = "SARIMA", color = "#E69F00", size = 1.8) +
  ggtitle("SARIMA CPI") +
  xlab("Año") + ylab("CPI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

print(p2)

# ARIMAX comparativo
p3 <- autoplot(serie_real, series = "Real", size = 1, color = "black") +
  autolayer(pred_cpi_ms_future_ts, series = "CPI+MS", color = "#0072B2", size = 1.2) +
  autolayer(pred_cpi_ur_future_ts, series = "CPI+UR", color = "#E69F00", size = 1.2) +
  autolayer(pred_cpi_multi_future_ts, series = "CPI+MS+UR", color = "#56B4E9", size = 1.2) +
  ggtitle("ARIMAX - Modelos") +
  xlab("Año") + ylab("CPI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

print(p3)

# VAR comparativo
p4 <- autoplot(serie_real, series = "Real", size = 1, color = "black") +
  autolayer(pred_ms_future_ts, series = "CPI+MS", color = "#0072B2", size = 1.2) +
  autolayer(pred_ur_future_ts, series = "CPI+UR", color = "#E69F00", size = 1.2) +
  autolayer(pred_multi_future_ts, series = "CPI+MS+UR", color = "#56B4E9", size = 1.2) +
  ggtitle("VAR - Modelos") +
  xlab("Año") + ylab("CPI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))

print(p4)

# Comparativo general
p5 <- autoplot(serie_real, series = "Real", size = 1, color = "black") +
  autolayer(pred_future_ts_arima, series = "ARIMA", color = "#0072B2", size = 1) +
  autolayer(pred_future_ts_sarima, series = "SARIMA", color = "#E69F00", size = 1) +
  autolayer(pred_cpi_ms_future_ts, series = "ARIMAX MS", color = "#56B4E9", size = 1) +
  autolayer(pred_cpi_ur_future_ts, series = "ARIMAX UR", color = "#CC79A7", size = 1) +
  autolayer(pred_cpi_multi_future_ts, series = "ARIMAX MS+UR", color = "#009E73", size = 1) +
  autolayer(pred_ms_future_ts, series = "VAR MS", color = "#D55E00", size = 1) +
  autolayer(pred_ur_future_ts, series = "VAR UR", color = "#F0E442", size = 1) +
  autolayer(pred_multi_future_ts, series = "VAR MS+UR", color = "#0173B2", size = 1) +
  ggtitle("Todos los Modelos") +
  xlab("Año") + ylab("CPI") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 8))

print(p5)