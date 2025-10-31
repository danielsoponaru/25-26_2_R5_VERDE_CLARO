library(ggplot2)
library(plotly)
library(dplyr)
library(readr)

#################TASA LABORAL MASCULINO
datos <- read_csv("Datos/LNS11300001.csv")

# Preparar los datos: crear columna Year y filtrar desde 1990
datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

# Gráfico con el mismo estilo que el anterior
p <- ggplot(datos, aes(x = observation_date, y = LNS11300001)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de participación laboral masculina en Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estado Unidos",
    x = "Año",
    y = "Porcentaje de hombres en la fuerza laboral"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )

# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_PARTICIPACION.jpg", p)

#################TASA LABORAL MASCULINO
datos <- read_csv("Datos/LNS11300002.csv")

datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

#Gráfico 
p <- ggplot(datos, aes(x = observation_date, y = LNS11300002)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de participación laboral femenina en Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Año",
    y = "Porcentaje de mujeres en la fuerza laboral"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )
# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_PARTICIPACION_FEMENINA.jpg", p)

#######################TASA DESEMPLEO
datos <- read_csv("Datos/UNRATE.csv")
datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

# Gráfico con el mismo estilo
p <- ggplot(datos, aes(x = observation_date, y = UNRATE)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de desempleo en Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Año",
    y = "Tasa de desempleo (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )


# Convertir a interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_DESEMPLEO.jpg", p)

##################TASA DE PARTICIPACION LABORAR 16-19

datos <- read_csv("Datos/LNS11300012.csv")

# Preparar los datos: crear columna Year y filtrar desde 1990
datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

# Gráfico
p <- ggplot(datos, aes(x = observation_date, y = LNS11300012)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de participación laboral (16-19 años) en Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Año",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )


# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_PARTICIPACION_16_19.jpg", p)

##################TASA DE PARTICIPACION LABORAR 20-24

datos <- read_csv("Datos/LNS11300036 (1).csv")

datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

# Gráfico
p <- ggplot(datos, aes(x = observation_date, y = LNS11300036)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de participación laboral (20-24 años) en Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Año",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )



# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_PARTICIPACION_20_24.jpg", p)

##################TASA DE PARTICIPACION LABORAR 25-54
datos <- read_csv("Datos/LNS11300060.csv")

datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

#grafico
p <- ggplot(datos, aes(x = observation_date, y = LNS11300060)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de participación laboral (25-54 años) en Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Año",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )

# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_PARTICIPACION_25_54.jpg", p)

##################TASA DE PARTICIPACION LABORAR +55

datos <- read_csv("Datos/LNS11324230.csv")
datos <- datos %>%
  mutate(
    Year = as.numeric(format(observation_date, "%Y"))
  ) %>%
  filter(Year >= 1990)

p <- ggplot(datos, aes(x = observation_date, y = LNS11324230)) +
  geom_line(color = "#3C3B6E", size = 0.75) +
  labs(
    title = "Tasa de participación laboral (+55 años) en Estados Unidos",
    subtitle = "Fuente: Banco Federal Estados Unidos",
    x = "Año",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#B0B2D3", color = NA),
    plot.background = element_rect(fill = "#B0B2D3", color = NA)
  )


# Convertir a gráfico interactivo con plotly
p_interactivo <- ggplotly(p)
p_interactivo

ggsave(filename = "Graficos/TASA_PARTICIPACION_55.jpg", p)

