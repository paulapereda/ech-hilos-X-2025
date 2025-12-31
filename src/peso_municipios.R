pacman::p_load(hrbrthemes, tidyverse, ggrepel, here, sf)

personas <- readRDS("~/Library/CloudStorage/Dropbox/Consultoría UNICEF/Producto 1 & 2/Producto 2 - Informe Preliminar/R/data/02_final/personas_ampliada.rds")

peso_demografico <- personas %>%
  filter(departamento == "Montevideo") %>%
  rename(municipio = municipio_pais) %>%
  mutate(
    municipio = ifelse(municipio == "9898", NA_character_, municipio),
    perna01_tramo = ifelse(perna01_tramo %in% c("0-4", "5-9", "10-14", "15-19"), "Menores de 18 años", "Mayores de 18 años")
  ) %>%
  group_by(municipio, perna01_tramo) %>%
  summarise(n = n()) %>%
  mutate(
    peso = n / sum(n),
    label = paste0(round(peso * 100, 1), "%")
  ) %>%
  ungroup() %>%
  filter(perna01_tramo == "Menores de 18 años")

municipios <- st_read("~/Library/CloudStorage/Dropbox/Consultoría UNICEF/Producto 1 & 2/Producto 2 - Informe Preliminar/R/data/unidades_geoestadísticas/ccz_mvd_23_pg.gpkg") %>%
  rename(municipio = NOMMUNICIPIO) %>%
  mutate(municipio = case_when(
    municipio == "A" ~ "Municipio A",
    municipio == "B" ~ "Municipio B",
    municipio == "C" ~ "Municipio C",
    municipio == "CH" ~ "Municipio CH",
    municipio == "D" ~ "Municipio D",
    municipio == "E" ~ "Municipio E",
    municipio == "F" ~ "Municipio F",
    municipio == "G" ~ "Municipio G"
  ))

municipios <- municipios %>%
  left_join(peso_demografico, by = "municipio")

municipios_labels <- municipios %>%
  mutate(centroid = st_centroid(geom))

ggplot(municipios) +
  geom_sf(aes(fill = peso), color = "white") +
  geom_sf_text(
    data = municipios_labels,
    aes(geometry = centroid, label = label),
    inherit.aes = FALSE,
    fontface = "bold",
    color = "grey20",
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#b56576",
    high = "#6d597a",
    name = "% niños, niñas y adolescentes",
    trans = "log",
    na.value = "grey90"
  ) +
  labs(
    title = "Peso demográfico relativo de la población de niños, niñas y\nadolescentes por municipio (Montevideo)",
    fill = NULL,
    x = NULL,
    y = NULL,
    caption = "Fuente: elaboración propia a partir de los Censos 2023 | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 14) +
  theme(
    plot.caption = ggtext::element_markdown(size = 12),
    panel.grid.major = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )

ggsave(here("output", "nna_municipios.jpg"), dpi = 300, width = 10, height = 7)
