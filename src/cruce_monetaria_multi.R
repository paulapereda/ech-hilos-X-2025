pacman::p_load(hrbrthemes, tidyverse, ggtext, readxl, srvyr, here)

# df <- read_csv(here("data", "ECH_implantacion_2024.csv")) %>% 
#   mutate(
#     tramo_edad = case_when(
#       e27 < 6 ~ "Menores de 6",
#       e27 >= 6 & e27 <= 12 ~ "6 a 12",
#       e27 >= 13 & e27 <= 17 ~ "13 a 17",
#       e27 >= 18 & e27 <= 64 ~ "18 a 64",
#       e27 >= 65 ~ "65 y más",
#       TRUE ~ NA_character_
#     ),
#     menores_18 = ifelse(e27 <= 17, 1, 0)
#   )
# 
# pesos_boost <- read_xlsx(here("data", "pesos_replicados_ANUAL-2024_terceros.xlsx")) 
# 
# ech_24_con_pesos <- df
# 
# df <- df %>% 
#   left_join(pesos_boost)
# 
# ech_24_con_pesos <- df %>% 
#   write_rds(here("data", "ech_24_con_pesos.rds"))

df <- read_rds(here("data", "ech_24_con_pesos.rds"))

design_ech <- df %>% 
  as_survey_rep(type = "bootstrap", weights = W_ANO, repweights = starts_with("wr"))

estado_civil <- design_ech %>%
  filter(menores_18 == 0) %>%
  group_by(e36, nom_dpto) %>%
  summarise(
    estado_civil = survey_total(na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = estado_civil / sum(estado_civil) * 100
  ) 


estado_civil <- estado_civil %>% 
  mutate(estado_civil = case_when(
    1
    Separado/a de unión libre anterior
    2
    Divorciado/a
    3
    Casado/a (incluye separado/a y aún no se divorció)
    4
    Viudo/a de casamiento
    6
    Viudo/a de unión libre
    5
    Soltero/a (nunca se casó ni vivió en unión libre)
  ))

  tabla_doble <- design_ech %>%
  filter(menores_18 == 1) %>%
  group_by(pobre17, pobre_multi) %>%
  summarise(
    n_menores = survey_total(na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = n_menores / sum(n_menores) * 100
  ) 

pobres <- tabla_doble %>% 
  select(- contains("n_menores")) %>% 
  pivot_wider(names_from = pobre_multi, values_from = porcentaje)

# Definimos los porcentajes
datos <- tribble(
  ~grupo,                        ~porcentaje,
  "Ambas pobrezas",                       17,
  "Solo pobreza monetaria",               12,
  "Solo pobreza multidimensional",        11,
  "Sin pobreza",                          60
) %>% 
  mutate(grupo = factor(grupo, levels = c("Ambas pobrezas",
                                          "Solo pobreza monetaria",
                                          "Solo pobreza multidimensional",
                                          "Sin pobreza")))

# Expandir los porcentajes a 100 puntos
waffle_data <- datos %>%
  uncount(porcentaje) %>%
  mutate(id = row_number(),
         fila = (id - 1) %/% 10,
         columna = (id - 1) %% 10)

ggplot(waffle_data, aes(x = columna, y = -fila, fill = grupo)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(
    values = c(
      "Ambas pobrezas" = "#461220",
      "Solo pobreza monetaria" = "#8c2f39",
      "Solo pobreza multidimensional" = "#b23a48",
      "Sin pobreza" = "#fcb9b2"
    )
  ) +
  coord_equal() +
  labs(
    title = "No es solo falta de ingresos: la pobreza infantil también es multidimensional",
    subtitle = "Cada cuadrado representa 1% del total",
    fill = NULL,
    caption = "La pobreza multidimensional refleja las múltiples privaciones de la población<br>en términos de educación, condiciones habitacionales, servicios básicos del<br>hogar, protección social y empleo.<br><br>Fuente: INE, Encuesta Continua de Hogares (2024) | @paubgood"
  ) +
  theme_ipsum_rc(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_markdown(size = 16)
    )

ggsave(here("output", "pobreza_multi_monetaria.jpg"), dpi = 300, width = 12, height = 8)
