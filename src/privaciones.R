pacman::p_load(hrbrthemes, tidyverse, here)

privaciones <- tibble(
  indicador = c(
    "Vinculación educativa", "Rezago educativo", "Años de escolarización",
    "Hacinamiento", "Tenencia insegura", "Materialidad y problemas", "Internet",
    "Calefacción", "Saneamiento", "Pensiones", "Cuidados",
    "Seguridad social menores", "Informalidad", "Desempleo", "Subempleo"
  ),
  menores_6_cens = c(2.85, 1.82, 29.87, 21.44, 6.50, 19.26, 16.17, 9.23, 6.54, 0.18, 11.58, 2.00, 19.92, 7.98, 9.55),
  menores_6_unc = c(3.43, 2.31, 56.37, 33.75, 9.25, 24.59, 22.53, 12.20, 7.21, 0.40, 21.55, 2.73, 31.71, 13.09, 14.58),
  edad_6_12_cens = c(2.04, 2.27, 24.91, 16.29, 6.83, 17.53, 12.97, 7.89, 6.28, 0.26, 6.53, 1.84, 17.97, 6.80, 10.63),
  edad_6_12_unc = c(3.04, 3.05, 51.14, 23.41, 10.49, 24.43, 18.60, 11.61, 6.75, 0.60, 13.72, 2.70, 33.25, 12.05, 16.93),
  edad_13_17_cens = c(5.46, 4.87, 24.70, 13.21, 7.56, 16.87, 11.75, 7.56, 5.99, 0.35, 5.61, 1.75, 17.32, 7.82, 10.35),
  edad_13_17_unc = c(7.59, 6.50, 51.89, 17.91, 11.18, 23.45, 16.29, 10.95, 6.72, 0.95, 10.78, 2.62, 31.96, 13.93, 17.85),
  edad_18_64_cens = c(2.03, 1.70, 17.30, 7.18, 4.57, 10.55, 8.23, 5.27, 3.32, 0.39, 4.33, 0.94, 13.07, 5.88, 7.46),
  edad_18_64_unc = c(2.86, 2.87, 50.20, 10.97, 8.06, 16.59, 14.63, 9.29, 4.17, 1.22, 9.82, 1.41, 30.85, 14.18, 15.32),
  edad_65mas_cens = c(0.40, 0.25, 4.69, 1.60, 1.10, 3.37, 3.62, 1.71, 1.29, 1.65, 1.25, 0.38, 4.19, 1.40, 2.06),
  edad_65mas_unc = c(0.68, 0.59, 18.82, 2.67, 3.06, 9.73, 22.58, 5.49, 2.97, 12.29, 5.09, 0.56, 17.67, 5.21, 5.36)
)

privaciones_limpio <- privaciones %>%
  pivot_longer(
    cols = -indicador,
    names_to = "grupo_variable",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = case_when(
      str_ends(grupo_variable, "_cens") ~ "Censurada",
      str_ends(grupo_variable, "_unc") ~ "No censurada"
    ),
    edad = case_when(
      str_starts(grupo_variable, "menores_6_") ~ "Menores de 6 años",
      str_starts(grupo_variable, "edad_6_12_") ~ "6 a 12 años",
      str_starts(grupo_variable, "edad_13_17_") ~ "13 a 17 años",
      str_starts(grupo_variable, "edad_18_64_") ~ "18 a 64 años",
      str_starts(grupo_variable, "edad_65mas_") ~ "65 o más años"
    ),
    edad = factor(edad, levels = rev(c(
      "Menores de 6 años",
      "6 a 12 años",
      "13 a 17 años",
      "18 a 64 años",
      "65 o más años"
    )))
  ) %>%
  select(indicador, edad, tipo, valor) %>%
  mutate(dimension = case_when(
    indicador == "Rezago educativo" ~ "Educación",
    indicador == "Vinculación educativa" ~ "Educación",
    indicador == "Rezago educativo" ~ "Educación",
    indicador == "Años de escolarización" ~ "Educación",
    indicador == "Hacinamiento" ~ "Condiciones habitacionales",
    indicador == "Tenencia insegura" ~ "Condiciones habitacionales",
    indicador == "Materialidad y problemas" ~ "Condiciones habitacionales",
    indicador == "Internet" ~ "Servicios básicos del hogar",
    indicador == "Calefacción" ~ "Servicios básicos del hogar",
    indicador == "Saneamiento" ~ "Servicios básicos del hogar",
    indicador == "Pensiones" ~ "Protección Social",
    indicador == "Seguridad social menores" ~ "Protección Social",
    indicador == "Cuidados" ~ "Protección Social",
    indicador == "Informalidad" ~ "Empleo",
    indicador == "Desempleo" ~ "Empleo",
    indicador == "Subempleo" ~ "Empleo"
  ))

plot_privaciones <- function(data, titulo, caption = NULL) {
  ggplot(data, aes(x = valor, y = indicador, fill = edad)) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.8, preserve = "single")) +
    scale_fill_manual(
      values = c(
        "Menores de 6 años" = "#eaac8b",
        "6 a 12 años" = "#e56b6f",
        "13 a 17 años" = "#b56576",
        "18 a 64 años" = "#6d597a",
        "65 o más años" = "#355070"
      ),
      breaks = c(
        "Menores de 6 años",
        "6 a 12 años",
        "13 a 17 años",
        "18 a 64 años",
        "65 o más años"
      )
    ) +
    facet_grid(
      rows = vars(dimension),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    labs(
      title = titulo,
      x = "Porcentaje de personas con privación",
      y = NULL,
      fill = "Tramo de edad",
      caption = caption
    ) +
    theme_ipsum_rc(base_size = 14) +
    theme(
      strip.placement = "outside",
      plot.caption = ggtext::element_markdown(size = 12),
      strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
      panel.spacing.y = unit(1, "lines"),
      axis.text.y = element_text(size = 10),
      strip.background = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

# Filtrar por tipo
privaciones_cens <- privaciones_limpio %>% filter(tipo == "Censurada")
privaciones_unc  <- privaciones_limpio %>% filter(tipo == "No censurada")

# Graficar
plot_privaciones(
  privaciones_cens,
  titulo = "Privaciones censuradas por indicador y tramo de edad (2024)",
  caption = "Miden la proporción total de personas que sufren privación en un indicador determinado, sin importar si son pobres multidimensionales o no.<br>Reflejan la incidencia bruta de la privación en la población general.<br><br>Fuente: Elaboración propia con base en ECH 2024 (INE) | @paubgood"
)

ggsave(here("output", "privaciones_censuradas.jpg"), dpi = 300, width = 12, height = 7)


plot_privaciones(privaciones_unc, 
                 titulo = "Privaciones no censuradas por indicador y tramo de edad (2024)",
                 caption = "Miden la proporción de personas multidimensionalmente pobres que además están privadas en ese indicador.<br> Solo consideran a los individuos que ya han sido clasificados como pobres multidimensionales (es decir, que superan el umbral de privaciones establecido en el IPM).<br><br>Fuente: Elaboración propia con base en ECH 2024 (INE) | @paubgood")

ggsave(here("output", "privaciones_no_censuradas.jpg"), dpi = 300, width = 12, height = 7)

