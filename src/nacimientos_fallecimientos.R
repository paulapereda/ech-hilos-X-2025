pacman::p_load(hrbrthemes, tidyverse, here)

nacimientos_muertes_uy <- tibble(
  anio = 2015:2024,
  nacidos_vivos = c(48926, 47058, 43036, 40139, 37472, 35874, 34603, 32301, 31385, 29899),
  fallecidos = c(32967, 34273, 33173, 34128, 34807, 32638, 41168, 39322, 34678, 35956)
) %>%
  pivot_longer(nacidos_vivos:fallecidos, names_to = "categoria", values_to = "valor") %>%
  mutate(
    categoria = recode(categoria,
                       "nacidos_vivos" = "Cantidad de nacidos vivos",
                       "fallecidos" = "Cantidad de fallecidos"),
    anio_label = ifelse(anio == 2024, "2024*", as.character(anio)),
    label = format(valor, big.mark = ".", decimal.mark = ",")
  )

ggplot(nacimientos_muertes_uy, aes(x = anio, y = valor, color = categoria)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(
    aes(label = label),
    vjust = -1.5,
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Cantidad de nacidos vivos" = "#b56576",
      "Cantidad de fallecidos" = "#6d597a"
    )
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", accuracy = 1),
                     limits = c(NA, 50000)) +
  scale_x_continuous(
    breaks = 2015:2024,
    labels = ifelse(2015:2024 == 2024, "2024*", as.character(2015:2024))
  ) +
  labs(
    title = "Evolución de los nacimientos y las muertes en Uruguay (2015 a 2024*)",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "(*) Los datos 2024 son preliminares<br>Fuente: Elaboración propia en base a Estadísticas Vitales, MSP | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.caption = ggtext::element_markdown(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave(here("output", "nacimientos_fallecimientos.jpg"), dpi = 300, width = 10, height = 7)


fecundidad_adolescente <- tibble(
  anio = 2004:2024,
  tasa = c(
    58.9, 59.89, 61.07, 59.57, 61.09, 59.42, 59.91, 57.27, 59.89, 60.94,
    58.15, 55.61, 50.31, 41.65, 35.8, 31.58, 28, 25.99, 22.32, 20.94, 20.12
  )
)

ggplot(fecundidad_adolescente, aes(x = anio, y = tasa)) +
  geom_line(color = "#b56576", size = 1) +
  geom_point(color = "#b56576", size = 2) +
  geom_text(aes(label = tasa), vjust = -1, size = 3.5, fontface = "bold", color = "grey20") +
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  scale_y_continuous(limits = c(0, 65)) +
  labs(
    title = "Cantidad de nacimientos de madres adolescentes por cada 1.000 mujeres de entre 15 y 19 años (2004 a 2024*)",
    x = NULL,
    y = "Tasa de fecundidad en adolescentes",
    caption = " * Los datos 2024 son preliminares\nFuente: MSP-INE | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 14) +
  theme(
    plot.caption = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

ggsave(here("output", "fecundidad_adolescente.jpg"), dpi = 300, width = 12, height = 7)

