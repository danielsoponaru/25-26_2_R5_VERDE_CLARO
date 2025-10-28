library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

#ESPERANZA DE VIDA AL NACER EN ESTADOS UNIDOS (1960-2023)

datos <- read_csv("Datos/API_SP.DYN.LE00.IN_DS2_es_csv_v2_1113163.csv", skip = 4)


usa <- datos %>%
  filter(`Country Name` == "Estados Unidos") %>%
  pivot_longer(
    cols = `1960`:`2023`,
    names_to = "Año",
    values_to = "Esperanza_de_vida"
  ) %>%
  mutate(Año = as.numeric(Año))

grafico1 = ggplot(usa, aes(x = Año, y = Esperanza_de_vida)) +
  geom_point(color = "#3C3B6E", size = 2) +
  labs(
    title = "Esperanza de vida al nacer en Estados Unidos (1960–2023)",
    subtitle = "Fuente: Banco Mundial de Datos",
    x = "Año",
    y = "Esperanza de vida (años)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey85"),
    plot.background = element_rect(fill = "#B0B2D3", color = NA),
    panel.background = element_rect(fill = "#B0B2D3", color = NA)
  )

ggsave(filename = "Graficos/ESPERANZA_VIDA.jpg", grafico1)


######


library(readr)
library(ggplot2)
library(dplyr)

#GASTO EN SALUD PER CÁPITA - ESTADOS UNIDOS)

datos <- read_csv("Datos/HLTHSCPCHCSA.csv")

datos <- datos %>%
  rename(
    Fecha = observation_date,
    Gasto_per_capita = HLTHSCPCHCSA
  )
datos$Fecha <- as.Date(datos$Fecha)

grafico2 = ggplot(datos, aes(x = Fecha, y = Gasto_per_capita)) +
  geom_area(fill = "#3C3B6E", alpha = 0.6) +
  geom_line(color = "#3C3B6E", size = 1) +
  labs(
    title = "Gasto en Salud per cápita - Estados Unidos",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "",
    y = "Dólares estadounidenses"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey85"),
    plot.background = element_rect(fill = "#B0B2D3", color = NA),
    panel.background = element_rect(fill = "#B0B2D3", color = NA)
  )

ggsave(filename = "Graficos/GASTOS_SALUD_PP.jpg", grafico2)
