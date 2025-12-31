pacman::p_load(hrbrthemes, tidyverse, here)

pobreza_multidimensional <- tibble(
  tramo_edad = c(
    "Menores de 6 años",
    "6 a 12 años",
    "13 a 17 años",
    "18 a 64 años",
    "65 o más años"
  ),
  incidencia = c(31.4, 27.5, 27.0, 18.4, 6.2) / 100,
  intensidad = c(35.0, 34.2, 34.9, 33.4, 30.9) / 100
) %>%
  pivot_longer(incidencia:intensidad, names_to = "pobreza", values_to = "valor") %>%
  mutate(
    tramo_edad = factor(tramo_edad, levels = c(
      "Menores de 6 años",
      "6 a 12 años",
      "13 a 17 años",
      "18 a 64 años",
      "65 o más años"
    )),
    label = paste0(round(valor * 100, 1), "%", sep = "")
  )

ggplot(pobreza_multidimensional, aes(x = tramo_edad, y = valor, fill = pobreza)) +
  geom_bar(stat = "identity", position = position_dodge(width = .7)) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("incidencia" = "#b56576", "intensidad" = "#6d597a"),
    labels = c("incidencia" = "Incidencia", "intensidad" = "Intensidad")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Pobreza multidimensional por tramo de edad (2023)",
    caption = "Fuente: INE, Encuesta Continua de Hogares | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.caption = ggtext::element_markdown(size = 12),
    panel.grid.major.y = element_blank()
  )

ggsave(here("output", "pobreza_multidimensional.jpg"), dpi = 300, width = 10, height = 7)
