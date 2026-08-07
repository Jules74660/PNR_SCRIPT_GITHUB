
# pourcentage de sites avec 0, avec 1, avec 0 à 5, de 5 à 10 et +10 sonneurs adultes

data %>% group_by(Nbr_adulte) %>% summarise(n = n()) %>%
  mutate(pourcentage = n / sum(n) * 100) %>% view()

# 84.5 % de sites non occupés 
# 15.5 % de sites occupés

# essayer de relier, suintement dans la colonne remarque à la présence de sonneurs 
data %>% group_by(str_detect(Remarques., "Suintement")) %>% summarise(n = n()) %>% view()

# essayer de relier, suintement dans la colonne remarque à la présence de Nbr_adulte
data %>% group_by(str_detect(Remarques., "Suintement")) %>% summarise(Nbr_adulte = sum(Nbr_adulte)) %>% view()

# 48 sonneurs comptés en contexte de suintement


# Rajouter des : dans la colonne heure 
data$HEURE <- str_replace(data$HEURE, "(\\d{2})(\\d{2})", "\\1:\\2")

# voir s'il y a un patern dans la journée, normalement il y a 10 jours donc on devrait avoir un graphique avec le nombre de sonneurs adultes en Y et les heures de la journée en X avec les différentes courbes pour chaque journée

data %>% filter(Nbr_adulte > 0) %>% ggplot(aes(x = HEURE, y = Nbr_adulte, group = DATE)) +
  geom_line() +
  geom_point() +
  theme_minimal()

# faire une moyenne de sonneurs comptés par heure donc une catégorie de 9h à 10h et de suite et ensuite faire un graphique avec les catégories en X et le nombre moyen de sonneurs comptés en Y (courbe), et rajouter le nombre d'observations par catégorie

n_total <- data %>%
  mutate(HEURE_CAT = case_when(
    HEURE >= "09:00" & HEURE < "10:00" ~ "09-10",
    HEURE >= "10:00" & HEURE < "11:00" ~ "10-11",
    HEURE >= "11:00" & HEURE < "12:00" ~ "11-12",
    HEURE >= "12:00" & HEURE < "13:00" ~ "12-13",
    HEURE >= "13:00" & HEURE < "14:00" ~ "13-14",
    HEURE >= "14:00" & HEURE < "15:00" ~ "14-15",
    HEURE >= "15:00" & HEURE < "16:00" ~ "15-16",
    HEURE >= "16:00" & HEURE < "17:00" ~ "16-17",
    TRUE ~ NA_character_
  )) %>%
  group_by(HEURE_CAT) %>%
  summarise(n_total = n())

# Données du graphique après filtre
data %>%
  filter(Nbr_adulte > 0) %>%
  mutate(HEURE_CAT = case_when(
    HEURE >= "09:00" & HEURE < "10:00" ~ "09-10",
    HEURE >= "10:00" & HEURE < "11:00" ~ "10-11",
    HEURE >= "11:00" & HEURE < "12:00" ~ "11-12",
    HEURE >= "12:00" & HEURE < "13:00" ~ "12-13",
    HEURE >= "13:00" & HEURE < "14:00" ~ "13-14",
    HEURE >= "14:00" & HEURE < "15:00" ~ "14-15",
    HEURE >= "15:00" & HEURE < "16:00" ~ "15-16",
    HEURE >= "16:00" & HEURE < "17:00" ~ "16-17",
    TRUE ~ NA_character_
  )) %>%
  group_by(HEURE_CAT) %>%
  summarise(
    moyenne_sonneurs = mean(Nbr_adulte)) %>%
  st_drop_geometry(moyenne_sonneurs) %>%
  left_join(n_total, by = "HEURE_CAT") %>%
  ggplot(aes(x = HEURE_CAT, y = moyenne_sonneurs)) +
  geom_line(group = 1) +
  geom_point() +
  geom_text(aes(label = paste0("n=", n_total)),
            vjust = -1) +
  theme_minimal()
