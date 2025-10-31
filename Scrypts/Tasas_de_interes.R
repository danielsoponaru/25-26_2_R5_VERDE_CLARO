# Cargar los datos
datos <- read.csv("Datos/datos_tasas_interes.csv")

# Limpiar los datos
datos <- datos %>%
  mutate(
    Fecha = as.Date(Fecha, format = "%d.%m.%Y"),
    Actual = as.numeric(gsub(",", ".", gsub("%", "", Actual)))
  ) %>%
  arrange(Fecha)

# Crear gráfico con ggplot2
ggplot(datos, aes(x = Fecha, y = Actual)) +
  geom_line(color = "#3C3B6E", linewidth = 1.2) +
  labs(
    title = "Evolución de la Tasa de Interés",
    subtitle = "Fuente: Banco Federal de Estados Unidos",
    x = "Fecha",
    y = "Tasa (%)"
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

