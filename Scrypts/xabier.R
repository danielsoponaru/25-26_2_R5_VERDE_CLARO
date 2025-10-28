# Cargar librerías
library(ggplot2)

# Leer el archivo CSV
datos <- read.csv("Datos/DCOILWTICO (1).csv")

# Ver las primeras filas para entender la estructura
head(datos)

# Si las columnas se llaman 'DATE' y 'DCOILWTICO', convertir la fecha
datos$observation_date <- as.Date(datos$observation_date)

# Graficar una línea de tiempo
grafico = ggplot(datos, aes(x = observation_date, y = DCOILWTICO)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Precio del Petróleo WTI",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Fecha",
    y = "Precio (USD por barril)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black", size = 10),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )

ggsave(filename = "Graficos/PRECIO_PETROLEO.jpg", grafico)


########################################################
# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)

# Leer el CSV
data <- read_csv("Datos/API_FP.CPI.TOTL.ZG_DS2_es_csv_v2_115681[1].csv", skip = 4)

# Filtrar solo Estados Unidos
usa_data <- data %>%
  filter(`Country Name` == "Estados Unidos") %>%
  select(`Country Name`, matches("^[0-9]{4}$")) %>%   # columnas de años
  tidyr::pivot_longer(cols = -`Country Name`,
                      names_to = "Year",
                      values_to = "Value")

# Convertir columnas a formato correcto
usa_data$Year <- as.numeric(usa_data$Year)
usa_data$Value <- as.numeric(usa_data$Value)

# Graficar
inflacion = ggplot(usa_data, aes(x = Year, y = Value)) +
  geom_line(color = "#3C3B6E", size = 1) +
  geom_point(color = "#3C3B6E") +
  labs(title = "Tasa de inflación (% anual) de Estados Unidos",
       subtitle = "Fuente: Banco Mundial de Datos",
       x = "Año",
       y = "Inflación (%)") + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black", size = 10),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )

ggsave(filename = "Graficos/INFLACION.jpg", inflacion)
