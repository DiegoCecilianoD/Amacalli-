# Diego Ceciliano Díaz
# Equipo 1 - Amacalli
# Pública (Agosto - Diciembre 2025)

# Librerías 
library(tidyverse)
library(sf)            
library(janitor)       
library(leaflet)       
library(ggplot2)
library(jsonlite)
library(dplyr)
library(viridis)
library(tidyr)
library(patchwork)

# Reportes de Agua Tlalpan y CDMX

# --- Datos recientes (2022–2024)
url_recientes <- "https://datos.cdmx.gob.mx/api/3/action/datastore_search?resource_id=a8069e94-c7cb-45d7-8166-561e80884422&limit=5000"
data_recientes <- fromJSON(url_recientes)$result$records
agua_2022_2024 <- as_tibble(data_recientes)

# --- Datos históricos (2018–2021)
url_historicos <- "https://datos.cdmx.gob.mx/api/3/action/datastore_search?resource_id=65a6b1a6-5d6e-49b9-aeed-ca7b22e8de03&limit=5000"
data_historicos <- fromJSON(url_historicos)$result$records
agua_2018_2021 <- as_tibble(data_historicos)

# Limpiar nombres de columnas
agua_2018_2021 <- clean_names(agua_2018_2021)
agua_2022_2024 <- clean_names(agua_2022_2024)

# Verifica las diferencias
names(agua_2018_2021)
names(agua_2022_2024)

# --- Limpieza y estandarización ---
agua_18_21_std <- agua_2018_2021 %>%
  clean_names() %>%
  transmute(
    id,
    folio,
    fecha_reporte = ymd_hms(fecha, quiet = TRUE),
    reporte = tipo_de_falla,
    alcaldia_catalogo = str_to_title(alcaldia),
    colonia_catalogo = str_to_title(coalesce(colonia_datos_abiertos, colonia_registro_sacmex)),
    longitud = as.numeric(longitud),
    latitud = as.numeric(latitud)
  )

agua_22_24_std <- agua_2022_2024 %>%
  clean_names() %>%
  transmute(
    id,
    folio = folio_incidente,
    fecha_reporte = ymd_hms(fecha_reporte, quiet = TRUE),
    reporte,
    alcaldia_catalogo = str_to_title(alcaldia_catalogo),
    colonia_catalogo = str_to_title(colonia_catalogo),
    longitud = as.numeric(longitud),
    latitud = as.numeric(latitud)
  )

# --- Unión final ---
agua_cdmx_unida <- bind_rows(agua_18_21_std, agua_22_24_std) %>%
  drop_na(longitud, latitud, fecha_reporte, alcaldia_catalogo)

glimpse(agua_cdmx_unida)

# --- Filtrar Tlalpan ---
agua_tlalpan <- agua_cdmx_unida%>%
  filter(alcaldia_catalogo == "Tlalpan")

# --- Ver rango temporal ---
range(agua_tlalpan$fecha_reporte, na.rm = TRUE)

agua_tlalpan <- agua_tlalpan %>%
  mutate(fecha_reporte = as.Date(fecha_reporte))


# Exploración (""2019-01-11 UTC" "2023-03-27 UTC"")

# a. Reportes por tipo
agua_tlalpan %>%
  count(reporte, sort = TRUE)

# b. Evolución temporal
# --- Línea de tiempo de reportes en Tlalpan (2018–2024) ---
# Crear agregación mensual y completar con ceros
agua_tlalpan_mensual <- agua_tlalpan %>%
  mutate(mes = floor_date(fecha_reporte, "month")) %>%
  count(mes, reporte) %>%
  complete(
    mes = seq(min(mes, na.rm = TRUE), max(mes, na.rm = TRUE), by = "month"),
    reporte,
    fill = list(n = 0)
  )

# --- Gráfico ---
grafico_1 <- ggplot(agua_tlalpan_mensual, aes(x = mes, y = n, color = reporte, group = reporte)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 2.2, alpha = 0.9) +
  scale_color_viridis_d(option = "plasma", end = 0.9, name = "Tipo de incidente") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b\n%Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Evolución mensual de reportes de agua en Tlalpan (2018–2024)",
    subtitle = "Todas las categorías se mantienen visibles, incluso en meses sin reportes",
    x = "Mes y año",
    y = "Número de reportes",
    caption = "Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0, color = "#222222"),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1, color = "gray40"),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 10.5, color = "gray25"),
    axis.text.y = element_text(size = 10.5, color = "gray25"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.4),
    plot.margin = margin(15, 20, 10, 15)
  )

# Guardar el gráfico como PNG
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/grafico_1.png", grafico_1, width = 10, height = 6, dpi = 300)

# c. Mapa rápido de densidad 
grafico_2 <- ggplot(agua_tlalpan, aes(x = longitud, y = latitud)) +
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.85,
    color = NA
  ) +
  geom_point(alpha = 0.15, color = "gray35", size = 0.7) +
  scale_fill_viridis_c(option = "plasma", end = 0.9, name = "Densidad de reportes") +
  coord_equal() +
  labs(
    title = "Mapa de calor de reportes de agua en Tlalpan",
    subtitle = "Concentración espacial de incidencias (fugas, falta de agua, mala calidad, etc.)",
    x = "Longitud",
    y = "Latitud",
    caption = "Fuente: Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0, color = "#222222"),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1, color = "gray40"),
    axis.text = element_text(size = 10.5, color = "gray25"),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(15, 15, 10, 15)
  )

# Guardar el gráfico como PNG
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/grafico_2.png", grafico_2, width = 10, height = 6, dpi = 300)

grafico_3 <- agua_tlalpan %>%
  count(colonia_catalogo, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(colonia_catalogo = forcats::fct_reorder(colonia_catalogo, n)) %>%
  ggplot(aes(x = colonia_catalogo, y = n, fill = n)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", end = 0.9) +
  labs(
    title = "Colonias con más reportes de agua en Tlalpan",
    subtitle = "Top 15 colonias según el número de reportes ciudadanos (2018–2024)",
    x = "Colonia",
    y = "Número de reportes",
    caption = "Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0, color = "#222222"),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1, color = "gray40"),
    axis.text.x = element_text(size = 10.5, color = "gray25"),
    axis.text.y = element_text(size = 11, color = "gray25"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(15, 20, 10, 15)
  )

# Guardar el gráfico como PNG
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/grafico_3.png", grafico_3, width = 10, height = 6, dpi = 300)

# Incidentes con colonia (mapa)

# --- 0. Función para quitar acentos ---
quitar_acentos <- function(x) {
  x %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ñ", "n")
}

# --- 1. Cargar shapefile de colonias CDMX y arreglar geometrías ---
colonias <- st_read(
  "https://datos.cdmx.gob.mx/dataset/02c6ce99-dbd8-47d8-aee1-ae885a12bb2f/resource/026b42d3-a609-44c7-a83d-22b2150caffc/download/colonias.geojson",
  quiet = TRUE
) %>%
  mutate(colonia_std = str_to_upper(quitar_acentos(colonia))) %>%
  st_make_valid() %>%          # geometrías válidas
  st_cast("POLYGON") %>%       # desarmar multipolígonos complejos
  mutate(area = st_area(geometry)) %>%
  arrange(desc(area))           # dibujar polígonos grandes primero

# --- 2. Preparar dataset de reportes ---
agua_tlalpan <- agua_tlalpan %>%
  mutate(colonia_catalogo_std = str_to_upper(quitar_acentos(colonia_catalogo)))

reportes_sf <- agua_tlalpan %>%
  drop_na(longitud, latitud) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = st_crs(colonias))

# --- 3. Mapa de calor ---
mapa_calor <- ggplot() +
  geom_sf(data = colonias, fill = "white", color = "gray80", linewidth = 0.3) +
  stat_density_2d(
    data = agua_tlalpan,
    aes(x = longitud, y = latitud, fill = after_stat(level), alpha = after_stat(level)),
    geom = "polygon",
    contour = TRUE
  ) +
  scale_fill_viridis_c(option = "plasma", end = 0.9, name = "Densidad") +
  scale_alpha(range = c(0.2, 0.7), guide = FALSE) +
  geom_sf(data = reportes_sf, color = "gray35", size = 0.3, alpha = 0.2) +
  coord_sf() +
  labs(
    title = "Mapa de calor de reportes de agua",
    subtitle = "Distribución espacial de incidencias (2018–2024)"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_text(size = 10.5, color = "gray25")
  )

# --- 4. Choropleth (reportes por colonia) ---
reportes_por_colonia <- agua_tlalpan %>%
  group_by(colonia_catalogo_std) %>%
  summarise(n_reportes = n(), .groups = "drop")

colonias_cdmx_reportes <- colonias %>%
  left_join(reportes_por_colonia,
            by = c("colonia_std" = "colonia_catalogo_std"))

mapa_choropleth <- ggplot(colonias_cdmx_reportes) +
  geom_sf(aes(fill = n_reportes), color = "gray60", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", end = 0.9, na.value = "white", name = "Reportes") +
  labs(
    title = "Reportes de agua por colonia en la CDMX",
    subtitle = "Número total de incidencias (2018–2024)"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_text(size = 10.5, color = "gray25")
  )

# --- 5. Combinar mapas con patchwork ---
mapa_combinado <- mapa_calor + mapa_choropleth + plot_layout(ncol = 2, widths = c(1,1))

# --- 6. Mostrar resultado ---
mapa_combinado

ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/, mapa_combinado.png", mapa_combinado, width = 10, height = 6, dpi = 300)


# Contaminación del agua 

# --- 1. Cargar límites de alcaldías ---
url_alcaldias <- "https://datos.cdmx.gob.mx/dataset/alcaldias/resource/deb5c583-84e2-4e07-a706-1b3a0dbc99b0/download/alcaldias.geojson"
alcaldias <- st_read(url_alcaldias, quiet = TRUE)

# --- 2. Cargar datos de contaminación ---
url_contaminacion <- "https://datos.cdmx.gob.mx/dataset/contaminacion-de-agua-en-la-ciudad-de-mexico/resource/898506cf-cf25-4d1f-8b6a-192a6a8145be/download/contaminacion-agua.geojson"
contaminacion <- st_read(url_contaminacion, quiet = TRUE)

# --- 3. Transformar CRS para coincidir ---
contaminacion <- st_transform(contaminacion, st_crs(alcaldias))

#  4. Convertir resumen a tibble normal 
resumen_alcaldias_tbl <- resumen_alcaldias %>% st_set_geometry(NULL)

# 5. Unir con geometría original usando left_join
alcaldias_cdmx <- alcaldias %>%
  left_join(resumen_alcaldias_tbl, by = "NOMGEO")

# 6. Gráfico
grafico_4 <- ggplot(alcaldias_cdmx) +
  geom_sf(aes(fill = nivel_contaminacion), color = "gray60", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    end = 0.9,
    na.value = "gray95",
    name = "Nivel de contaminación"
  ) +
  labs(
    title = "Calidad del agua por alcaldía en CDMX",
    subtitle = "Promedio de niveles de contaminación (GRIDCODE)",
    caption = "Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    plot.caption = element_text(size = 10, color = "gray40", margin = margin(t = 8)),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_text(size = 10.5, color = "gray25")
  )

# Guardar el gráfico como PNG
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/grafico_4.png", grafico_4, width = 10, height = 6, dpi = 300)

# Alcaldias + reportes 
# --- 1. Cargar límites de alcaldías ---
url_alcaldias <- "https://datos.cdmx.gob.mx/dataset/alcaldias/resource/deb5c583-84e2-4e07-a706-1b3a0dbc99b0/download/alcaldias.geojson"
alcaldias <- st_read(url_alcaldias, quiet = TRUE)

# --- 2. Convertir reportes de agua a objeto sf ---
# Asegúrate que tu dataset de reportes se llama 'agua_cdmx' y tiene columnas latitud/longitud
reportes_sf <- agua_cdmx_unida %>%
  drop_na(longitud, latitud) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = st_crs(alcaldias))

# --- 3. Contar reportes por alcaldía ---
# Unión espacial de puntos con polígonos de alcaldías
reportes_alcaldias <- st_join(reportes_sf, alcaldias, join = st_within)

resumen_reportes <- reportes_alcaldias %>%
  st_set_geometry(NULL) %>%       # quitar geometría para resumir
  group_by(NOMGEO) %>%
  summarise(n_reportes = n(), .groups = "drop")

# --- 4. Unir resumen con geometría de alcaldías ---
alcaldias_cdmx <- alcaldias %>%
  left_join(resumen_reportes, by = "NOMGEO")

# --- 5. Calcular límites completos de la CDMX ---
bbox <- st_bbox(alcaldias)

# --- 6. Graficar mapa completo ---
grafico_5 <- ggplot(alcaldias_cdmx) +
  # Choropleth por número de reportes
  geom_sf(aes(fill = n_reportes), color = "gray60", linewidth = 0.3) +
  # Puntos de reportes
  geom_sf(data = reportes_sf, color = "red", size = 0.3, alpha = 0.2) +
  # Escala de color
  scale_fill_viridis_c(option = "plasma", na.value = "gray95", name = "Número de reportes") +
  # Limites automáticos de toda la CDMX
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  # Etiquetas y formato 
  labs(
    title = "Reportes de agua en CDMX por alcaldía",
    subtitle = "Distribución y cantidad de incidencias ciudadanas (2018–2024)",
    caption = "Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    plot.caption = element_text(size = 10, color = "gray40", margin = margin(t = 8)),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_text(size = 10.5, color = "gray25")
  )

print(grafico_5)

# Guardar el gráfico como PNG
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/grafico_5.png", grafico_5, width = 10, height = 6, dpi = 300)

# Infraestructura 
url_infra <- "https://datos.cdmx.gob.mx/dataset/cobertura-de-infraestructura-de-agua-potable-m-hab/resource/1c42cd15-c3c0-4cef-aa74-7c7efa4798fd/download/cobertura-agua.geojson"

# Leer el GeoJSON directamente
infra <- st_read(url_infra, quiet = TRUE)

# Revisar los primeros registros
head(infra)

# Ver resumen general
summary(infra)

# Revisar nombres de columnas
names(infra)

install.packages("forcats")
library(forcats)

# --- 2. Agrupar y resumir ---
infra_resumen <- infra %>%
  group_by(alcaldia) %>%
  summarise(
    PromFugas = mean(PromFugas, na.rm = TRUE),
    LongInf_m = mean(LongInf_m, na.rm = TRUE),
    m_x_hab = mean(m_x_hab, na.rm = TRUE),
    C_m_x_hab = mean(C_m_x_hab, na.rm = TRUE)
  ) %>%
  filter(!is.na(alcaldia))

# --- 3. Tema base ---
tema_ranking <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    plot.caption = element_text(size = 10, hjust = 0, color = "gray40"),
    axis.text.y = element_text(size = 12)
  )

# --- 4. Función para gráfico de ranking ---
ranking_plot <- function(data, variable, titulo, subtitulo, etiqueta) {
  data %>%
    arrange(desc(!!sym(variable))) %>%
    mutate(alcaldia = fct_reorder(alcaldia, !!sym(variable))) %>%
    ggplot(aes(x = alcaldia, y = !!sym(variable), fill = !!sym(variable))) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    coord_flip() +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = NULL,
      y = etiqueta,
      caption = "Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
    ) +
    tema_ranking
}

# --- 5. Gráficos individuales ---

# (1) Infraestructura por habitante
ranking_mxhab <- ranking_plot(
  infra_resumen,
  "m_x_hab",
  "Infraestructura de agua potable por habitante",
  "Metros de infraestructura promedio por alcaldía",
  "Metros / habitante"
)

# (2) Promedio de fugas
ranking_fugas <- ranking_plot(
  infra_resumen,
  "PromFugas",
  "Promedio de fugas en la red hídrica",
  "Indicador de eficiencia del sistema por alcaldía",
  "Promedio de fugas"
)

# (3) Longitud promedio
ranking_long <- ranking_plot(
  infra_resumen,
  "LongInf_m",
  "Longitud promedio de infraestructura hídrica",
  "Metros de red de agua potable promedio por alcaldía",
  "Longitud (m)"
)

# (4) Cobertura media
ranking_cobertura <- ranking_plot(
  infra_resumen,
  "C_m_x_hab",
  "Cobertura media de infraestructura hídrica",
  "Indicador de cobertura promedio por alcaldía",
  "Cobertura media"
)

# --- 6. Mostrar los gráficos ---
ranking_mxhab
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/ranking_mxhab.png",ranking_mxhab, width = 10, height = 6, dpi = 300)

ranking_fugas
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/ranking_fugas.png", ranking_fugas, width = 10, height = 6, dpi = 300)

ranking_long
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/ranking_long.png", ranking_long, width = 10, height = 6, dpi = 300)

ranking_cobertura
ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/ranking_cobertura.png", ranking_cobertura, width = 10, height = 6, dpi = 300)


# --- 1. Filtrar Tlalpan y limpiar NA ---
tlalpan_col <- infra %>%
  filter(alcaldia == "TLALPAN") %>%
  mutate(
    m_x_hab = ifelse(is.na(m_x_hab), 0, m_x_hab),
    C_m_x_hab = ifelse(is.na(C_m_x_hab), 0, C_m_x_hab)
  )

# --- 2. Crear un ranking combinado: menor cobertura y menor m_x_hab ---
tlalpan_col <- tlalpan_col %>%
  mutate(ranking_score = rank(-C_m_x_hab) + rank(-m_x_hab))  # menor cobertura y menor infraestructura suman peor puntaje

top10_combined <- tlalpan_col %>%
  arrange(ranking_score) %>%
  slice_head(n = 10)

# --- 3. Gráfico ---

grafico_combined <- ggplot(top10_combined, aes(x = reorder(colonia, ranking_score), y = m_x_hab, fill = C_m_x_hab)) +
  geom_col(color = "gray50", linewidth = 0.3) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", end = 0.9, name = "Cobertura media", na.value = "white") +
  labs(
    title = "Top 10 colonias con menor infraestructura y cobertura de agua en Tlalpan",
    subtitle = "Combinación de metros de infraestructura por habitante y cobertura media",
    x = "Colonia",
    y = "Metros de infraestructura por habitante",
    caption = "Fuente: Datos Abiertos CDMX / SACMEX — Equipo 1"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(face = "bold", size = 16, color = "#222222"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11, color = "#222222"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 1)
  )

grafico_combined

ggsave("~/Downloads/Pública/Agua Tlalpan/Pública/grafico_combined.png", grafico_combined, width = 10, height = 6, dpi = 300)


