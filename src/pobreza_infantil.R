pacman::p_load(hrbrthemes, tidyverse, readxl, srvyr, here)

df <- read_csv(here("data", "ECH_implantacion_2024.csv")) %>% 
  mutate(
    tramo_edad = case_when(
      e27 < 6 ~ "Menores de 6",
      e27 >= 6 & e27 <= 12 ~ "6 a 12",
      e27 >= 13 & e27 <= 17 ~ "13 a 17",
      e27 >= 18 & e27 <= 64 ~ "18 a 64",
      e27 >= 65 ~ "65 y más",
      TRUE ~ NA_character_
    ),
    menores_18 = ifelse(e27 <= 17, 1, 0)
  )

pesos_boost <- read_xlsx(here("data", "pesos_replicados_ANUAL-2024_terceros.xlsx")) 

df <- df %>% 
  left_join(pesos_boost)

design_ech <- df %>% 
  as_survey_rep(type = "bootstrap", weights = W_ANO, repweights = starts_with("wr"))

# Calcular pobreza total - LP 2006
pobreza_total <- design_ech %>% 
  summarise(pobreza = survey_mean(pobre06)) %>% 
  as_tibble() %>% 
  select(- pobreza_se)%>% 
  mutate(
    tramo_edad = "Total",
    label = paste0(round(pobreza * 100, 1), "%")
  )

# Calcular pobreza por tramos de edad - LP 2006
pobreza_edad <- design_ech %>% 
  group_by(tramo_edad) %>% 
  summarise(pobreza = survey_mean(pobre06)) %>% 
  as_tibble() %>% 
  select(- pobreza_se) %>% 
  mutate(
    label = paste0(round(pobreza * 100, 1), "%")
  )

# Unir ambos y ordenar niveles - LP 2006
pobreza_edad_06 <- pobreza_edad %>% 
  bind_rows(pobreza_total) %>% 
  mutate(
    tramo_edad = factor(tramo_edad, levels = c(
      "Menores de 6",
      "6 a 12",
      "13 a 17",
      "18 a 64",
      "65 y más",
      "Total"
    )),
    metodologia = "Metodología 2006"
  )

# Calcular pobreza total - LP 2017
pobreza_total <- design_ech %>% 
  summarise(pobreza = survey_mean(pobre17)) %>% 
  as_tibble() %>% 
  select(- pobreza_se)%>% 
  mutate(
    tramo_edad = "Total",
    label = paste0(round(pobreza * 100, 1), "%")
  )

# Calcular pobreza por tramos de edad - LP 2017
pobreza_edad <- design_ech %>% 
  group_by(tramo_edad) %>% 
  summarise(pobreza = survey_mean(pobre17)) %>% 
  as_tibble() %>% 
  select(- pobreza_se) %>% 
  mutate(
    label = paste0(round(pobreza * 100, 1), "%")
  )

# Unir ambos y ordenar niveles - LP 2017
pobreza_edad_17 <- pobreza_edad %>% 
  bind_rows(pobreza_total) %>% 
  mutate(
    tramo_edad = factor(tramo_edad, levels = c(
      "Menores de 6",
      "6 a 12",
      "13 a 17",
      "18 a 64",
      "65 y más",
      "Total"
    )),
    metodologia = "Metodología 2017"
  )

pobreza_edad <- pobreza_edad_06 %>% 
  bind_rows(pobreza_edad_17)
  
ggplot(pobreza_edad, aes(x = tramo_edad, y = pobreza, fill = metodologia)) +
  geom_bar(stat = "identity",  position = position_dodge(width = .90), width = .8) +
  geom_text(aes(label = label),
            position = position_dodge(width = .9),
            vjust = -.5,
            size = 4.5,
            na.rm = TRUE,
            color = "#333333",
            fontface = "bold",
            family = "Roboto Condensed") +
  scale_fill_manual(values = c("Metodología 2006" = "#6d597a", "Metodología 2017" = "#b56576")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL, 
    y = "Incidencia de la pobreza (%)",
    title = "Incidencia de la pobreza en personas, según grupos de edades",
    subtitle = "Total país, 2024",
    caption = "Fuente: Elaboración propia con base en ECH (INE) | @paubgood"
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

