##### LIBRERÍAS ################################################################
#DPLYR
#library(help = dplyr)
if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

#FORECAST
#library(help = forecast)
if (!require(forecast)){
  install.packages("forecast")
  library(forecast)
} else {
  library(forecast)
}

##### IMPORATCIÓN DE DATOS #####################################################
ipc = read.csv(file = "Datos/ipc_usa.csv")
pib = read.csv(file = "Datos/pib_exogenas_usa.csv", sep = ";")

##### ANÁLISIS EXPLORATORIO (EDA) ##############################################
#IPC
dim(ipc)
colnames(ipc)
str(ipc)
ipc = ipc %>% select(-DATE)
ipc = ts(data = ipc, start = 1970, frequency = 12)
autoplot(ipc)

#PIB
dim(pib)
colnames(pib)
head(pib)
unique(pib$Country); pib = pib %>% select(-Country)
unique(pib$Code); pib = pib %>% select(-Code)
unique(pib$ContinentCode); pib = pib %>% select(-ContinentCode)
unique(pib$Year); pib = pib %>% select(-Year)
unique(pib$Month); pib = pib %>% select(-Month)
pib = ts(data = pib, start = 1970, frequency = 12)
