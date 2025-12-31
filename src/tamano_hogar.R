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

design_ech %>%
  filter(menores_18 == 1) %>%
  group_by(pobre17) %>%
  summarise(
    n_menores = survey_total(na.rm = TRUE),
    .groups = "drop"
  )

# Paso 1: contar cantidad de menores por hogar y quedarte con la pobreza del hogar
hogares <- df %>%
  group_by(ID) %>%
  summarise(
    pobre17 = first(pobre17),
    n_menores = sum(menores_18 == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Paso 2: convertir a objeto de encuesta de hogares
hogares_svy <- hogares %>%
  left_join(
    df %>% select(ID, W_ANO, starts_with("wr")) %>% distinct(),
    by = "ID"
  ) %>%
  as_survey_rep(
    type = "bootstrap",
    weights = W_ANO,
    repweights = starts_with("wr")
  )

# Paso 3: tabular cantidad de hogares por pobreza y número de menores
resumen <- hogares_svy %>%
  group_by(pobre17, n_menores) %>%
  summarise(
    n_hogares = survey_total(),
    .groups = "drop"
  )

# Paso 4 (opcional): calcular proporciones dentro de cada grupo de pobreza
resumen <- resumen %>%
  group_by(pobre17) %>%
  mutate(
    porcentaje = n_hogares / sum(n_hogares) * 100
  ) %>%
  ungroup()

resumen











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