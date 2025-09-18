##### LIBRERÍAS ################################################################
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

if (!require(forecast)) {
  install.packages("forecast")
  library(forecast)
} else {
  library(forecast)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}

##### IMPORTACIÓN DE DATOS #####################################################
ipc <- read.csv(file = "Datos/ipc_usa.csv")
pib <- read.csv(file = "Datos/pib_exogenas_usa.csv", sep = ";")

##### ANÁLISIS EXPLORATORIO (EDA) ##############################################

## --- IPC ---------------------------------------------------------------------
ipc <- ipc %>% select(-DATE)
IPC <- ts(data = ipc, start = 1970, frequency = 12)
autoplot(IPC)

## --- PIB ---------------------------------------------------------------------
# Eliminamos columnas no necesarias
pib <- pib %>% select(-Country, -Code, -ContinentCode, -Year, -Month)

# Serie temporal PIB (trimestral)
PIB <- pib$GDP.billion.currency.units
PIB <- na.omit(PIB)
PIB <- ts(data = PIB, start = 1970, frequency = 4)
autoplot(PIB)

## --- VARIABLES INDIVIDUALES --------------------------------------------------

# Oferta monetaria (mensual)
Money <- pib$Money.supply.billion.currency.units
Money <- na.omit(Money)
MONEY <- ts(data = Money, start = 1970, frequency = 12)
autoplot(MONEY)

# Tasa de desempleo (mensual)
Unemp <- pib$Unemployment.rate.percent
Unemp <- na.omit(Unemp)
UNEMP <- ts(data = Unemp, start = 1970, frequency = 12)
autoplot(UNEMP)

# Índice bursátil (mensual)
Stock <- pib$Stock.market.index
Stock <- na.omit(Stock)
STOCK <- ts(data = Stock, start = 1970, frequency = 12)
autoplot(STOCK)

