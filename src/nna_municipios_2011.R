pacman::p_load(hrbrthemes, tidyverse, labelled, janitor, ggrepel, haven, here, sf)

personas <- read_sav(here("data", "personas.sav")) %>%
  remove_labels() %>% 
  clean_names() %>% 
  mutate(
    perna01_tramo = case_when(
      perna01 >= 0   & perna01 <= 4   ~ "0-4",
      perna01 >= 5   & perna01 <= 9   ~ "5-9",
      perna01 >= 10  & perna01 <= 14  ~ "10-14",
      perna01 >= 15  & perna01 <= 19  ~ "15-19",
      perna01 >= 20  & perna01 <= 24  ~ "20-24",
      perna01 >= 25  & perna01 <= 29  ~ "25-29",
      perna01 >= 30  & perna01 <= 34  ~ "30-34",
      perna01 >= 35  & perna01 <= 39  ~ "35-39",
      perna01 >= 40  & perna01 <= 44  ~ "40-44",
      perna01 >= 45  & perna01 <= 49  ~ "45-49",
      perna01 >= 50  & perna01 <= 54  ~ "50-54",
      perna01 >= 55  & perna01 <= 59  ~ "55-59",
      perna01 >= 60  & perna01 <= 64  ~ "60-64",
      perna01 >= 65  & perna01 <= 69  ~ "65-69",
      perna01 >= 70  & perna01 <= 74  ~ "70-74",
      perna01 >= 75  & perna01 <= 79  ~ "75-79",
      perna01 >= 80                   ~ "80+",
      TRUE                            ~ NA_character_
    )
  )

peso_demografico <- personas %>%
  filter(dpto == "01") %>%
  mutate(
    perna01_tramo = ifelse(perna01_tramo %in% c("0-4", "5-9", "10-14", "15-19"), "Menores de 18 años", "Mayores de 18 años")
  ) %>%
  group_by(ccz, perna01_tramo) %>%
  summarise(n = n()) %>%
  mutate(
    peso = n / sum(n),
    label = paste0(round(peso * 100, 1), "%")
  ) %>%
  ungroup() %>%
  filter(perna01_tramo == "Menores de 18 años")

municipios <- st_read(here("data", "mapas vectoriales 2011", "ine_ccz_mvd.shp")) %>% 
  rename(ccz = CCZ)

municipios <- municipios %>%
  left_join(peso_demografico, by = "ccz")

municipios_labels <- municipios %>%
  mutate(centroid = st_centroid(geometry))

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
    title = "Peso demográfico relativo de la población de niños, niñas y\nadolescentes por municipio, 2011 (Montevideo)",
    fill = NULL,
    x = NULL,
    y = NULL,
    caption = "Fuente: elaboración propia a partir de los Censos 2011 | @paubgood"
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

ggsave(here("output", "nna_municipios_2011.jpg"), dpi = 300, width = 10, height = 7)
