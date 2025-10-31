########################### ANÁLISIS MACROECONÓMICO ############################

##### IMPORTACIÓN DE DATOS #####################################################
#TAMAÑO DE LA ECONOMÍA
pib_nominal = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=NGDPSAXDCUSQ&scale=left&cosd=1950-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-14&revision_date=2025-10-14&nd=1950-01-01")
pib_real = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDPC1&scale=left&cosd=1947-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-14&revision_date=2025-10-14&nd=1947-01-01")
consumo = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DPCERE1Q156NBEA&scale=left&cosd=1947-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-17&revision_date=2025-10-17&nd=1947-01-01")
inversion = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=A006RE1Q156NBEA&scale=left&cosd=1947-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-17&revision_date=2025-10-17&nd=1947-01-01")
exportaciones = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=B020RE1Q156NBEA&scale=left&cosd=1947-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-17&revision_date=2025-10-17&nd=1947-01-01")
importaciones = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=B021RE1Q156NBEA&scale=left&cosd=1947-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-17&revision_date=2025-10-17&nd=1947-01-01")
export_netas = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=A019RE1Q156NBEA&scale=left&cosd=1947-01-01&coed=2025-04-01&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-10-17&revision_date=2025-10-17&nd=1947-01-01")

#EUR_USD
xml = read_xml("https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/usd.xml")
obs_xml = html_children(html_children(html_children(xml)[[2]])[[2]])
EUR_USD = data.frame()
for (i in 1:length(obs_xml)) {
  ind = as.character(obs_xml[[i]])
  ind = str_trim(str_remove_all(ind, "Obs|>|<|\"|STATUS|CONF|/|OBS|\\_"))
  ind = str_trim(str_replace(ind, "=A =F", ""))
  val = str_split(ind, " ")
  fecha = str_remove(val[[1]][1], "TIMEPERIOD=")
  valor = str_remove(val[[1]][2], "VALUE=")
  instancia = c(fecha, valor)
  EUR_USD = rbind(EUR_USD, instancia)
}

##### PROCESAMIENTO DE DATOS ###################################################
#TAMAÑO ECONOMÍA
colnames(pib_nominal) = c("date", "PIB_NOMINAL")
colnames(pib_real) = c("date", "PIB_REAL")
colnames(consumo) = c("date", "CONSUMO_REL")
colnames(inversion) = c("date", "INVERSION_REL")
colnames(export_netas) = c("date", "EXPORTACIONES_NETAS_REL")
colnames(exportaciones) = c("date", "EXPORTACIONES_REL")
colnames(importaciones) = c("date", "IMPORTACIONES_REL")

df = pib_nominal %>%
  inner_join(pib_real, by = c("date")) %>%
  inner_join(consumo, by = c("date")) %>%
  inner_join(inversion, by = c("date")) %>%
  inner_join(export_netas, by = c("date")) %>%
  inner_join(exportaciones, by = c("date")) %>%
  inner_join(importaciones, by = c("date"))

df$PIB_NOMINAL = df$PIB_NOMINAL*10^6
df$PIB_REAL = df$PIB_REAL*10^9
df = df %>% 
  mutate(CONSUMO_ABS = PIB_REAL * CONSUMO_REL / 100, .before = CONSUMO_REL) %>%
  mutate(INVERSION_ABS = PIB_REAL * INVERSION_REL / 100, .before = INVERSION_REL) %>%
  mutate(EXPORTACIONES_ABS = PIB_REAL * EXPORTACIONES_REL / 100, .before = EXPORTACIONES_REL) %>%
  mutate(IMPORTACIONES_ABS = PIB_REAL * IMPORTACIONES_REL / 100, .before = IMPORTACIONES_REL) %>%
  mutate(EXPORTACIONES_NETAS_ABS = PIB_REAL * EXPORTACIONES_NETAS_REL / 100, .before = EXPORTACIONES_NETAS_REL) %>%
  mutate(GASTO_REL = 100 - CONSUMO_REL - INVERSION_REL - EXPORTACIONES_NETAS_REL, .before = EXPORTACIONES_NETAS_ABS) %>%
  mutate(GASTO_ABS = PIB_REAL * GASTO_REL / 100, .before = GASTO_REL)

df = df %>% 
  mutate(CREC_PIB_NOMINAL = c(NA, diff(PIB_NOMINAL)) / PIB_NOMINAL * 100, .after = PIB_NOMINAL) %>%
  mutate(CREC_PIB_REAL = c(NA, diff(PIB_REAL)) / PIB_REAL * 100, .after = PIB_REAL) %>%
  mutate(CREC_CONSUMO = c(NA, diff(CONSUMO_ABS)) / CONSUMO_ABS * 100, .after = CONSUMO_REL) %>%
  mutate(CREC_INVERSION = c(NA, diff(INVERSION_ABS)) / INVERSION_ABS * 100, .after = INVERSION_REL) %>%
  mutate(CREC_GASTO = c(NA, diff(GASTO_ABS)) / GASTO_ABS * 100, .after = GASTO_REL) %>%
  mutate(CREC_EXPORTACIONES = c(NA, diff(EXPORTACIONES_ABS)) / EXPORTACIONES_ABS * 100, .after = EXPORTACIONES_REL) %>%
  mutate(CREC_IMPORTACIONES = c(NA, diff(IMPORTACIONES_ABS)) / IMPORTACIONES_ABS * 100, .after = IMPORTACIONES_REL) %>%
  mutate(CREC_EXPORTACIONES_NETAS = c(NA, diff(EXPORTACIONES_NETAS_ABS)) / EXPORTACIONES_NETAS_ABS * 100, .after = EXPORTACIONES_NETAS_REL) 

colnames(df)[1] = "DATE"

str(df)
df$DATE = as.Date(df$DATE)

#EUR_USD
colnames(EUR_USD) = c("date", "EUR_USD")
EUR_USD$date = as.Date(EUR_USD$date)
EUR_USD$EUR_USD = as.numeric(EUR_USD$EUR_USD)
ts = ts(EUR_USD$EUR_USD)
trend = round(ma(ts, order = 300), 5)
E_U = EUR_USD %>% mutate(TREND = trend)
#HAY QUE MIRAR ESTO
autoplot(ts, ylab = "EUS/USD", xlab = "DATE")
autoplot(trend, ylab = "EUS/USD", xlab = "DATE")

EUR_USD_TREND = cbind(EUR_USD, trend)
EUR_USD_TREND$trend = as.numeric(EUR_USD_TREND$trend)

##### VISUALIZACIÓN DE LOS DATOS ###############################################
#TAMAÑO DE LA ECONOMÍA
colnames(df) = c("FECHA", 
                 "PIB_NOMINAL_ABSOLUTO",
                 "INDICE_DE_CRECIMIENTO_DEL_PIB_NOMINAL",
                 "PIB_REAL_ABSOLUTO",
                 "INDICE_DE_CRECIMIENTO_DEL_PIB_REAL",
                 "CONSUMO_ABSOLUTO",
                 "PORCENTAJE_DE_CONSUMO_DEL_PIB",
                 "INDICE_DE_CRECIMIENTO_DEL_CONSUMO",
                 "INVERSION_ABSOLUTA",
                 "PORCENTAJE_DE_INVERSION_DEL_PIB",
                 "INDICE_DE_CRECIMIENTO_DE_LA_INVERSION",
                 "GASTO_PUBLICO_ABSOLUTO",
                 "PORCENTAJE_DE_GASTO_PUBLICO_DEL_PIB",
                 "INDICE_DE_CRECIMIENTO_DEL_GASTO_PUBLICO",
                 "EXPORTACIONES_NETAS_ABSOLUTA",
                 "PORCENTAJE_DE_EXPORTACIONES_NETAS_DEL_PIB",
                 "INDICE_DE_CRECIMIENTO_DE_LAS_EXPORTACIONES_NETAS",
                 "EXPORTACIONES_ABSOLUTA",
                 "PORCENTAJE_DE_EXPORTACIONES_DEL_PIB",
                 "INDICE_DE_CRECIMIENTO_DE_LAS_EXPORTACIONES",
                 "IMPORTACIONES_ABSOLUTA",
                 "PORCENTAJE_DE_IMPORTACIONES_DEL_PIB",
                 "INDICE_DE_CRECIMIENTO_DE_LAS_IMPORTACIONES")
graficos = list()
for (i in colnames(df)[2:dim(df)[2]]) {
  grafico = ggplot(df, aes(x = FECHA, y = !!ensym(i))) + 
    geom_line(color = "#3C3B6E", size = 0.75) +
    labs(
      title = paste0("Evolución de ", str_replace_all(str_to_lower(i), "_", " ")),
      subtitle = "Fuente: Banco Federal de Estados Unidos",
      x = "Año",
      y = i
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
  graficos[[i]] = grafico
}

for (j in seq_along(graficos)) {
  plot_ = graficos[[j]]
  name_plot = names(graficos)[j]
  ggsave(filename = paste0("Graficos/", name_plot, ".jpg"), plot = plot_)
}

#EUR/USD
EUR_USD_plot = ggplot(EUR_USD_TREND, aes(x = date, y = EUR_USD)) +
  geom_line(color = "black") +
  geom_line(aes(y = trend), color = "red", size = 1.3) +
  labs(
    title = paste0("Evolución de EUR/USD"),
    subtitle = "Fuente: Banco Central Europeo",
    x = "Año",
    y = "EUR/USD"
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

ggsave(filename = "Graficos/EUR_USD.jpg", plot = EUR_USD_plot)
