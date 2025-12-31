pacman::p_load(tidyverse, readxl, srvyr, here)

df <- read_csv(here("data", "ECH_implantacion_2023.csv")) %>% 
  mutate(
    tramo_edad = case_when(
      e27 < 6 ~ "Menores de 6 años",
      e27 >= 6 & e27 <= 12 ~ "6 a 12 años",
      e27 >= 13 & e27 <= 17 ~ "13 a 17 años",
      e27 >= 18 & e27 <= 64 ~ "18 a 64 años",
      e27 >= 65 ~ "65 o más años",
      TRUE ~ NA_character_
    ),
    tramo_edad2 = case_when(
      e27 < 18 ~ "Menores de 18 años",
      e27 >= 18 & e27 <= 64 ~ "18 a 64 años",
      e27 >= 65 ~ "65 o más años",
      TRUE ~ NA_character_
    )
  )

pesos_boost <- read_xlsx(here("data", "pesos replicados Bootstrap anual 2023.xlsx")) 

df <- df %>% 
  left_join(pesos_boost)

design_ech <- df %>% 
  as_survey_rep(type = "bootstrap", weights = W_ANO, repweights = starts_with("wr"))

pobreza_edad <- design_ech %>% 
  group_by(tramo_edad) %>% 
  summarise(pobreza = survey_mean(pobre06)) %>% 
  as_tibble() %>% 
  select(- pobreza_se) %>% 
  mutate(
    tramo_edad = factor(tramo_edad, levels = c(
      "Menores de 6 años",
      "6 a 12 años",
      "13 a 17 años",
      "18 a 64 años",
      "65 o más años"
    )),
    label = paste0(round(pobreza*100, 1), "%", sep = ""))

ggplot(pobreza_edad, aes(x = tramo_edad, y = pobreza)) +
  geom_bar(stat = "identity", fill = "#b56576", width = 0.7) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = NULL,
    title = "Pobreza multidimensional por tramo de edad (2023)",
    caption = "Fuente: INE, Encuesta Continua de Hogares | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 14) +
  theme(
    legend.position = "none",
    plot.caption = ggtext::element_markdown(size = 12),
    panel.grid.major.y = element_blank()
  )
  
ggsave(here("output", "pobreza_monetaria.jpg"), dpi = 300, width = 10, height = 7)

pobreza_edad_2 <- design_ech %>% 
  group_by(tramo_edad2) %>% 
  summarise(pobreza = survey_mean(pobre06)) %>% 
  as_tibble() %>% 
  select(- pobreza_se) %>% 
  mutate(label = paste0(round(pobreza*100, 1), "%", sep = ""))

# tramo_edad2        pobreza  label
# Menores de 18 años   0.186  18.6%
# 18 a 64 años        0.0869   8.7% 
# 65 o más años       0.0220   2.2% 

