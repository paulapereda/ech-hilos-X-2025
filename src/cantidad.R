pacman::p_load(hrbrthemes, tidyverse, here)

personas <- readRDS("~/Library/CloudStorage/Dropbox/R/censo-uy-2023/data/02_final/personas.rds")

data <- personas %>%
  filter(perna01_tramo %in% c(
    "0-4",
    "5-9",
    "10-14",
    "15-19"
  )) %>%
  group_by(perna01_tramo) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(perna01_tramo = factor(perna01_tramo, levels = c(
    "0-4",
    "5-9",
    "10-14",
    "15-19"
  )),
  prop = n/3499451,
  label = paste0(round(prop*100, 1), "%", sep = ""))
  
  