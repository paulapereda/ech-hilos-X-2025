pacman::p_load(hrbrthemes, tidyverse, ggrepel, here, sf)

hogar <- read_rds("~/Library/CloudStorage/Dropbox/Consultoría UNICEF/Producto 1 & 2/Producto 2 - Informe Preliminar/R/data/02_final/hogar.rds") %>% 
  filter(departamento == "Montevideo") %>% 
  select(direccion_id, localidad, area, region_4, municipio_pais, vivid, hogid, 
         hogma01_1, hogma01_1_1, hogma01_2, hogma01_2_1, hogpr01_con_rraa) %>% 
  mutate(id_hogar = paste(direccion_id, vivid, hogid, sep = "-"),
         n_perros = as.numeric(hogma01_1_1),
         n_gatos = as.numeric(hogma01_2_1),
         n_perros = if_else(is.na(n_perros), 0L, n_perros),
         n_gatos = if_else(is.na(n_gatos), 0L, n_gatos))

hogar_resumen_por_hogar <- hogar %>%
  mutate(
    n_perros = if_else(is.na(n_perros), 0L, n_perros),
    n_gatos = if_else(is.na(n_gatos), 0L, n_gatos)
  ) %>%
  group_by(hogid, municipio_pais) %>%
  summarise(
    total_perros = sum(n_perros, na.rm = TRUE),
    total_gatos = sum(n_gatos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    categoria_mascotas = case_when(
      total_perros > total_gatos ~ "Más perros que gatos",
      total_gatos > total_perros ~ "Más gatos que perros",
      total_perros == 0 & total_gatos == 0 ~ "Ninguno",
      total_perros == total_gatos ~ "Igual cantidad"
    ),
    categoria_mascotas = factor(categoria_mascotas, levels = c("Más perros que gatos",
                                                               "Más gatos que perros",
                                                               "Igual cantidad",
                                                               "Ninguno"))
  ) %>%
  filter(!(categoria_mascotas == "Ninguno")) %>% 
  count(municipio_pais, categoria_mascotas, name = "cantidad") %>%
  group_by(municipio_pais) %>%
  mutate(
    total_hogares = sum(cantidad),
    porcentaje = round(100 * cantidad / total_hogares, 1),
    label = paste0(porcentaje, "%")
  ) %>%
  ungroup() %>%
  select(municipio_pais, categoria_mascotas, porcentaje, label) %>% 
  filter(!is.na(municipio_pais)) %>% 
  rename(municipio = municipio_pais)

hogar_dominante <- hogar_resumen_por_hogar %>%
  group_by(municipio) %>%
  slice_max(order_by = porcentaje, n = 1, with_ties = FALSE) %>%
  ungroup()

municipios <- st_read("~/Library/CloudStorage/Dropbox/Consultoría UNICEF/Producto 1 & 2/Producto 2 - Informe Preliminar/R/data/unidades_geoestadísticas/ccz_mvd_23_pg.gpkg") %>%
 filter(NOMDEPTO == "MONTEVIDEO") %>% 
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
  )) %>% 
  select(municipio, geom)

municipios_plot <- municipios %>%
  left_join(hogar_dominante, by = "municipio")

ggplot(municipios_plot) +
  geom_sf(aes(fill = categoria_mascotas), color = "grey20") +
  scale_fill_manual(values = c("#b56576", "#6d597a")) +
  labs(
    title = "¿Qué tipo de mascota es más común por hogar?",
    subtitle = "Montevideo, 2023",
    fill = NULL,
    x = NULL,
    y = NULL,
    caption = "Fuente: elaborado a partir de los Censos 2023 | @paubgood"
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
    legend.position = "bottom"
  )


ggsave(here("output", "gatos_perros.jpg"), dpi = 300, width = 10, height = 7)

hogar_por_municipio <- hogar %>%
  select(municipio_pais, hogpr01_con_rraa, n_perros, n_gatos) %>%
  filter(!is.na(municipio_pais)) %>%
  mutate(
    personas_hogar = as.numeric(hogpr01_con_rraa),
    personas_hogar = if_else(is.na(personas_hogar), 0, personas_hogar),
    n_perros = if_else(is.na(n_perros), 0L, n_perros),
    n_gatos = if_else(is.na(n_gatos), 0L, n_gatos)
  ) %>%
  group_by(municipio_pais) %>%
  summarise(
    total_perros = sum(n_perros, na.rm = TRUE),
    total_gatos = sum(n_gatos, na.rm = TRUE),
    total_personas = sum(personas_hogar, na.rm = TRUE),
    perros_por_persona = round(total_perros / total_personas, 3),
    gatos_por_persona = round(total_gatos / total_personas, 3),
    .groups = "drop"
  ) %>%
  rename(municipio = municipio_pais)
