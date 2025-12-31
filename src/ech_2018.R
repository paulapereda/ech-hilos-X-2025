df_2018 <- haven::read_sav(here("data", "HyP_2017_Terceros.sav")) %>%
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
  
df_2018 %>% 
 srvyr::as_survey_design(ids = 1, weights = pesoano) %>%
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
