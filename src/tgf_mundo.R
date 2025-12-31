pacman::p_load(rnaturalearthdata, rnaturalearth, tidyverse, here, sf)

# Cargar el mapa mundial con códigos ISO3
world <- ne_countries(scale = "medium", returnclass = "sf")

# Cargo datos
tgf_data <- read_csv(here("data", "fertility-rate-world.csv"))

# Unión por código ISO3
world_tgf <- world %>%
  left_join(tgf_data, by = c("iso_a3" = "iso3")) %>%
  filter(admin != "Antarctica")

ggplot(world_tgf) +
  geom_sf(aes(fill = tgf), color = "white", size = .1) +
  scale_fill_gradientn(
    colours = c("#eaac8b", "#e99c84", "#e88c7d", "#e56b6f", "#cd6873", "#b56576", "#915f78", "#6d597a", "#515575", "#355070"),
    name = "Tasa global de fecundidad (2023)",
    na.value = "grey90"
  ) +
  labs(
    title = "Tasa Global de Fecundidad por país, 2023",
    caption = "Fuente: estimaciones propias en base a Naciones Unidas (2023) | @paubgood"
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

ggsave(here("output", "tgf_mundial.jpg"), dpi = 300, width = 10, height = 7)
