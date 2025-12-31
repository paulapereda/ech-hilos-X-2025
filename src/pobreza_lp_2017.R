pacman::p_load(hrbrthemes, tidyverse, here)

pobreza_edad <- tibble(
  grupo_edad = c("Total", "Menores de 6 años", "6 a 12 años", "13 a 17 años", 
                 "18 a 64 años", "65 o más años"),
  estimacion = c(17.3, 32.2, 28.1, 27.5, 15.4, 6.3)
) %>% 
  mutate(grupo_edad = factor(grupo_edad, levels = c("Menores de 6 años", "6 a 12 años", "13 a 17 años", 
                                                    "18 a 64 años", "65 o más años", "Total")))

pobreza_edad %>%
  ggplot(aes(x = grupo_edad, y = estimacion)) +
  geom_bar(stat = "identity", fill = "#b56576", width = .6) +
  geom_text(aes(label = paste0(estimacion, "%")), 
            vjust = -0.5, 
            size = 6,
            color = "#333333",
            fontface = "bold",
            family = "Roboto Condensed") +
  labs(
    title = "Incidencia de la pobreza en personas según grupos de edades",
    subtitle = "Total país. 2024",
    x = "Grupo de edad",
    y = "Incidencia de la pobreza (%)",
    caption = "Fuente: Elaboración propia con base en ECH 2024 (INE) | @paubgood"
  ) +
  ylim(0, max(pobreza_edad$estimacion) + 5) +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave(here("output", "pobreza_2024.jpg"), dpi = 300, width = 12, height = 8)

pobreza <- tibble(
  año = 2006:2024,
  metodologia_2006 = c(32.5, 29.6, 24.2, 21.0, 18.5, 13.7, 12.4, 11.5, 9.7,
                       9.7, 9.4, 7.9, 8.1, 8.8, 11.6, 10.6, 9.9, 10.1, 8.3),
  metodologia_2017 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       18.1, 17.9, 15.5, 16.4, 17.3, 23.3, 21.1, 20.1, 19.7, 17.3)
)

pobreza_long <- pobreza %>%
  pivot_longer(cols = starts_with("metodologia"),
               names_to = "metodologia",
               values_to = "valor") %>%
  mutate(metodologia = recode(metodologia,
                              "metodologia_2006" = "Metodología 2006",
                              "metodologia_2017" = "Metodología 2017"))
ggplot(pobreza_long, aes(x = factor(año), y = valor, fill = metodologia)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(valor, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3,
            na.rm = TRUE,
            color = "#333333",
            fontface = "bold",
            family = "Roboto Condensed") +
  labs(
    x = "Año",
    y = "Incidencia de la pobreza (%)",
    fill = "Metodología",
    title = "Incidencia de la pobreza en personas (2006–2024)",
    caption = "Fuente: Elaboración propia con base en ECH (INE) | @paubgood"
  ) +
  scale_fill_manual(
    values = c("Metodología 2006" = "#6d597a", "Metodología 2017" = "#b56576")
  ) +
  theme_ipsum_rc(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25),
    plot.caption = element_text(size = 14),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

ggsave(here("output", "pobreza_2006_2017.jpg"), dpi = 300, width = 12, height = 8)
