H_GLU <- read_csv("IMPORT/01__COVARIABLES__/DEBIT/H_GLU_ANNEES.csv") %>% dplyr::select("Date (TU)", "Valeur (en m)") %>% rename(DATE = "Date (TU)", H_instant = "Valeur (en m)")

H_EYR <- read_csv("IMPORT/01__COVARIABLES__/DEBIT/H_EYR_ANNEES.csv") %>% dplyr::select("Date (TU)", "Valeur (en m)") %>% rename(DATE = "Date (TU)", H_instant = "Valeur (en m)")

summary(H_GLU)
summary(H_EYR)

str(H_GLU)
str(H_EYR)

library(tidyverse)
library(lubridate)
library(patchwork)

# ============================
# Préparation Eyrieux
# ============================

EYR <- H_EYR %>%
  mutate(date = as_date(DATE),
    annee = year(date),
    mois = month(date)) %>%
  filter(annee %in% c(2020, 2024, 2026),
    mois %in% 3:8) %>%
  mutate(jour_mois = as.Date(format(date, "2024-%m-%d")))

# ============================
# Préparation Gluèyre
# ============================

GLU <- H_GLU %>%
  mutate(date = as_date(DATE),
    annee = year(date),
    mois = month(date)) %>%
  filter(annee %in% c(2020, 2024, 2026),
    mois %in% 3:8) %>%
  mutate(jour_mois = as.Date(format(date, "2024-%m-%d")))

palette_annees <- c(
  "2020" = "#8AA29E",
  "2024" = "#3D5A80",
  "2026" = "#C9552F"
)

reperes_mois <- as.Date(c(
  "2024-03-01",
  "2024-04-01",
  "2024-05-01",
  "2024-06-01",
  "2024-07-01",
  "2024-08-01"
))

labels_mois <- c(
  "Mars",
  "Avr.",
  "Mai",
  "Juin",
  "Juil.",
  "Août"
)

p_H_EYR <- ggplot(EYR,
                  aes(x = jour_mois,
                      y = H_instant,
                      color = factor(annee))) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette_annees) +
  scale_x_date(
    breaks = reperes_mois,
    labels = labels_mois
  ) +
  labs(x = NULL,
    y = "Hauteur d'eau (m)",
    color = "Année"
  ) +
  theme_rapport

p_H_GLU <- ggplot(GLU,
                  aes(x = jour_mois,
                      y = H_instant,
                      color = factor(annee))) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = palette_annees) +
  scale_x_date(
    breaks = reperes_mois,
    labels = labels_mois) +
  labs(
    x = NULL,
    y = "Hauteur d'eau (m)",
    color = "Année"
  ) +
  theme_rapport

p_H_EYR + p_H_GLU
