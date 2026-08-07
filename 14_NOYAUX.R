
data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp") %>% st_transform(crs = 2154) 

# noyau 1 de 2020 
data %>%
  filter(ID_SITE > 373, ID_SITE < 445) %>%
  summarise(EFF_MAX = sum(EFF_MAX, na.rm = TRUE))

# noyau 2 de 2020 

data %>%
  filter(ID_SITE > 447, ID_SITE < 495) %>%
  summarise(EFF_MAX = sum(EFF_MAX, na.rm = TRUE))

# noyau 3 de 2020 

data %>%
  filter(ID_SITE > 533, ID_SITE < 553) %>%
  summarise(EFF_MAX = sum(EFF_MAX, na.rm = TRUE))

# noyau 4 de 2020 

data %>%
  filter(ID_SITE > 558, ID_SITE < 587) %>%
  summarise(EFF_MAX = sum(EFF_MAX, na.rm = TRUE))

DUCA <- sf::st_read("IMPORT/2012/DONNEES_SONNEURS_Ducasse_2012.shp") %>% st_transform(crs = 2154) 

# extrait num par ce sinon charactere
DUCA2 <- DUCA %>%
  st_drop_geometry() %>%
  mutate(num = as.numeric(gsub("eyr", "", Site)))

# noyau 1 de 2020 
DUCA2 %>%
  filter(num >= 4, num <= 6) %>%
  summarise(EFF_MAX = sum(Effectif.m, na.rm = TRUE))

# noyau 2 de 2020 
DUCA2 %>%
  filter(num >= 8, num <= 13) %>%
  summarise(EFF_MAX = sum(Effectif.m, na.rm = TRUE))

# noyau 3 de 2020 
DUCA2 %>%
  filter(num >= 18, num <= 20) %>%
  summarise(EFF_MAX = sum(Effectif.m, na.rm = TRUE))

# noyau 4 de 2020 
DUCA2 %>%
  filter(num >= 21, num <= 25) %>%
  summarise(EFF_MAX = sum(Effectif.m, na.rm = TRUE))

ROESGLU <- ROES %>% filter(RIVIERE == "Glueyre")
ROESEYR <- ROES %>% filter(RIVIERE == "Eyrieux")
sum(ROESGLU$EFF_MAX, na.rm = TRUE)
sum(ROESEYR$EFF_MAX, na.rm = TRUE)

# graphique des noyaux 
JEGO <- sf::st_read("IMPORT/2020/DONNEES_SONNEURS_Jego_2020.shp") %>%
  st_transform(crs = 2154)

dataglu <- data %>%
  mutate(EFF_MAX = as.numeric(EFF_MAX)) %>%
  filter(RIVIERE == "Glueyre")

JEGO <- JEGO %>%
  mutate(Eff_max = as.numeric(Eff_max)) %>%
  filter(NOM_MILIEU == "Glueyre")

# --- Ordre des sites (référence 2026) ---
ordre_sites <- dataglu$ID_SITE

dataglu <- dataglu %>%
  mutate(ID_SITE = factor(ID_SITE, levels = ordre_sites))

JEGO <- JEGO %>%
  mutate(Site = factor(Site, levels = ordre_sites))

dataglu <- dataglu %>%
  mutate(ID_num = as.numeric(as.character(ID_SITE)))

pas_affichage <- 10

labels_allegees <- ifelse(
  seq_along(levels(dataglu$ID_SITE)) %% pas_affichage == 0,
  levels(dataglu$ID_SITE),"")

ggplot() +
  geom_col(
    data = dataglu,
    aes(x = ID_SITE, y = EFF_MAX, fill = "2026 (ROES)"),
    color = "grey30",
    width = 0.85) +
  geom_col(
    data = JEGO %>% filter(Site %in% commun),
    aes(x = Site, y = Eff_max, fill = "2020 (JEGO)"),
    width = 0.6) +
  scale_fill_manual(
    name = "Campagne",
    values = c("2026 (ROES)" = "grey75", "2020 (JEGO)" = "green4")) +
  scale_x_discrete(labels = labels_allegees) +
  labs(x = "Sites de la Gluèyre (aval → amont)",
    y = "Effectif maximal observé") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
    axis.title = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic"))

# eyrieux : 

dataeyr <- data %>%
  mutate(EFF_MAX = as.numeric(EFF_MAX)) %>%
  filter(RIVIERE == "Eyrieux")

JEGO <- sf::st_read("IMPORT/2020/DONNEES_SONNEURS_Jego_2020.shp") %>%
  st_transform(crs = 2154)

JEGO <- JEGO %>%
  mutate(Eff_max = as.numeric(Eff_max)) %>%
  filter(NOM_MILIEU == "Eyrieux")

commun <- intersect(as.character(dataeyr$ID_SITE), as.character(JEGO$Site))

# --- Ordre des sites (référence 2026) ---
ordre_sites <- dataeyr$ID_SITE

dataeyr <- dataeyr %>%
  mutate(ID_SITE = factor(ID_SITE, levels = ordre_sites))

JEGO <- JEGO %>%
  mutate(Site = factor(Site, levels = ordre_sites))

JEGO <- JEGO %>%
  mutate(ID_num = as.numeric(as.character(Site)))

dataeyr <- dataeyr %>%
  mutate(ID_num = as.numeric(as.character(ID_SITE)))

pas_affichage <- 5

labels_allegees <- ifelse(
  seq_along(levels(dataeyr$ID_SITE)) %% pas_affichage == 0,
  levels(dataeyr$ID_SITE),"")

ggplot() +
  geom_col(
    data = dataeyr,
    aes(x = ID_SITE, y = EFF_MAX, fill = "2026 (ROES)"),
    color = "grey30",
    width = 0.85) +
  geom_col(
    data = JEGO %>% filter(Site %in% commun),
    aes(x = Site, y = Eff_max, fill = "2020 (JEGO)"),
    width = 0.6) +
  scale_fill_manual(
    name = "Campagne",
    values = c("2026 (ROES)" = "grey75", "2020 (JEGO)" = "green4")) +
  scale_x_discrete(labels = labels_allegees) +
  labs(x = "Sites de l'Eyrieux (aval → amont)",
       y = "Effectif maximal observé") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
        axis.title = element_text(size = 12),
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 9, face = "italic"))

dataeyr <- dataeyr %>%
  mutate(noyau = case_when(
      ID_num >= 371.1 & ID_num <= 442.2 ~ "Noyau 1",
      ID_num >= 447   & ID_num <= 497   ~ "Noyau 2",
      ID_num >= 498   & ID_num <= 532 ~ "inter",
      ID_num >= 533   & ID_num <= 551.1 ~ "Noyau 3",
      ID_num >= 557.1 & ID_num <= 587.2 ~ "Noyau 4",
      TRUE ~ "Hors noyau"))

resultats_noyaux <- dataeyr %>%
  group_by(noyau) %>%
  summarise(
    n_sites = n(),
    pct_sites = 100 * n() / nrow(dataeyr),
    effectif_total = sum(EFF_MAX, na.rm = TRUE),
    pct_effectif = 100 * sum(EFF_MAX, na.rm = TRUE) / sum(dataeyr$EFF_MAX, na.rm = TRUE)) %>%
  arrange(desc(pct_effectif))

JEGO <- JEGO %>%
  mutate(noyau = case_when(
    ID_num >= 372 & ID_num <= 445 ~ "Noyau 1",
    ID_num >= 447   & ID_num <= 495 ~ "Noyau 2",
    ID_num >= 496   & ID_num <= 531 ~ "inter",
    ID_num >= 533   & ID_num <= 553 ~ "Noyau 3",
    ID_num >= 558 & ID_num <= 587 ~ "Noyau 4",
    TRUE ~ "Hors noyau"))

resultats_noyaux <- JEGO %>%
  group_by(noyau) %>%
  summarise(
    n_sites = n(),
    pct_sites = 100 * n() / nrow(JEGO),
    effectif_total = sum(Eff_max, na.rm = TRUE),
    pct_effectif = 100 * sum(Eff_max, na.rm = TRUE) / sum(JEGO$Eff_max, na.rm = TRUE)) %>%
  arrange(desc(pct_effectif))

# pourcentage d'occupation par site 

PEIG <- read.csv(file = "IMPORT/2024/DONNEES_SONNEURS_2024_CP.csv") %>%
  mutate(X = as.numeric(gsub(",", ".", X)),Y = as.numeric(gsub(",", ".", Y))) %>% 
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

JEGO <- sf::st_read("IMPORT/2020/DONNEES_SITES_Jego_2020.shp") %>%
  st_transform(crs = 2154)

DUCA <- sf::st_read("IMPORT/2012/DONNEES_SONNEURS_Ducasse_2012.shp") %>% st_transform(crs = 2154) 

calc_occupation_noyaux <- function(df, id_col, eff_col, breaks) {
  df <- df %>%
    mutate(
      id_num = as.numeric(as.character(.data[[id_col]])),
      eff = as.numeric(.data[[eff_col]])
    )
  
  # Attribution du noyau selon les bornes fournies (data.frame avec noyau/min/max)
  df$noyau <- "Hors noyau"
  for (i in seq_len(nrow(breaks))) {
    idx <- df$id_num >= breaks$min[i] & df$id_num <= breaks$max[i]
    df$noyau[idx] <- breaks$noyau[i]
  }
  
  df %>%
    group_by(noyau) %>%
    summarise(
      n_sites_total = n(),
      n_sites_occupes = sum(eff > 0, na.rm = TRUE),
      taux_occupation = 100 * n_sites_occupes / n_sites_total,
      effectif_total = sum(eff, na.rm = TRUE)
    ) %>%
    arrange(match(noyau, breaks$noyau))
}

breaks_2020 <- tribble(
  ~noyau,     ~min, ~max,
  "Noyau 1",  373,  445,
  "Noyau 2",  447,  495,
  "Noyau 3",  533,  553,
  "Noyau 4",  558,  587)

res_2020 <- calc_occupation_noyaux(JEGO, "Site", "Eff_max", breaks_2020)
res_2020

PEIG <- PEIG %>%
  st_drop_geometry() %>%
  mutate(
    Site = site,
    OCC2024 = pmax(presence_P1, presence_P2)) %>%
  select(Site, OCC2024)

res_2024 <- calc_occupation_noyaux(PEIG, "Site", "OCC2024", breaks_2020)
res_2024

data <- data %>%
  st_drop_geometry() %>%
  select(
    Site = ID_SITE,
    EFF2026 = EFF_MAX)

res_2026 <- calc_occupation_noyaux(data, "Site", "EFF2026", breaks_2020)
res_2026

# regarder le nombre de sonneurs par passage sur les noyau pour 2020 et 2026 


JEGO <- JEGO %>%
  mutate(ID_num = as.numeric(as.character(Site)))

JEGO <- JEGO %>%
  mutate(noyau = case_when(
    ID_num >= 373 & ID_num <= 445 ~ "Noyau 1",
    ID_num >= 447   & ID_num <= 495 ~ "Noyau 2",
    ID_num >= 533   & ID_num <= 553 ~ "Noyau 3",
    ID_num >= 558 & ID_num <= 587 ~ "Noyau 4",
    TRUE ~ "Hors noyau"))

resultats_noyaux <- JEGO %>%
  group_by(noyau) %>%
  summarise(
    n_sites = n(),
    pct_sites = 100 * n() / nrow(JEGO),
    effectif_total = sum(Eff_max, na.rm = TRUE),
    effectif_passage_P1 = sum(Eff_tot_P1, na.rm = TRUE), 
    effectif_passage_P2 = sum(Eff_tot_P2, na.rm = TRUE),
    pct_effectif = 100 * sum(Eff_max, na.rm = TRUE) / sum(JEGO$Eff_max, na.rm = TRUE)) %>%
  arrange(desc(pct_effectif))


data <- data %>%
  mutate(noyau = case_when(
    ID_SITE >= 373 & ID_SITE <= 445 ~ "Noyau 1",
    ID_SITE >= 447   & ID_SITE <= 495 ~ "Noyau 2",
    ID_SITE >= 533   & ID_SITE <= 553 ~ "Noyau 3",
    ID_SITE >= 558 & ID_SITE <= 587 ~ "Noyau 4",
    TRUE ~ "Hors noyau"))

dataglu <- data %>% filter(RIVIERE == "Glueyre")

dataglu <- dataglu %>%
  mutate(noyau_glu = case_when(
    ID_SITE >= 14 & ID_SITE <= 34 ~ "Noyau 1",
    ID_SITE >= 118   & ID_SITE <= 175 ~ "Noyau 2",
    ID_SITE >= 239   & ID_SITE <= 258 ~ "Noyau 3",
    TRUE ~ "Hors noyau"))

resultats_noyaux <- dataglu %>%
  group_by(noyau_glu) %>%
  summarise(
    n_sites = n(),
    pct_sites = 100 * n() / nrow(JEGO),
    effectif_total = sum(EFF_MAX, na.rm = TRUE),
    effectif_passage_P1 = sum(SONN_P1, na.rm = TRUE), 
    effectif_passage_P2 = sum(SONN_P2, na.rm = TRUE),
    effectif_passage_P3 = sum(SONN_P3, na.rm = TRUE),
    pct_effectif = 100 * sum(EFF_MAX, na.rm = TRUE) / sum(dataglu$EFF_MAX, na.rm = TRUE)) %>%
  arrange(desc(pct_effectif))

JEGO <- JEGO %>%
  mutate(ID_SITE = as.numeric(as.character(Site)))

JEGO <- JEGO %>%
  mutate(noyau_glu = case_when(
    ID_SITE >= 14 & ID_SITE <= 34 ~ "Noyau 1",
    ID_SITE >= 118   & ID_SITE <= 175 ~ "Noyau 2",
    ID_SITE >= 235   & ID_SITE <= 258 ~ "Noyau 3",
    TRUE ~ "Hors noyau"))

resultats_noyaux_jeg <- JEGO %>%
  group_by(noyau_glu) %>%
  summarise(
    n_sites = n(),
    pct_sites = 100 * n() / nrow(JEGO),
    effectif_total = sum(Eff_max, na.rm = TRUE),
    effectif_passage_P1 = sum(Eff_tot_P1, na.rm = TRUE), 
    effectif_passage_P2 = sum(Eff_tot_P2, na.rm = TRUE),
    pct_effectif = 100 * sum(Eff_max, na.rm = TRUE) / sum(JEGO$Eff_max, na.rm = TRUE)) %>%
  arrange(desc(pct_effectif))


breaks_2020_glu <- tribble(
  ~noyau,     ~min, ~max,
  "Noyau 1",  14,  34,
  "Noyau 2",  118,  175,
  "Noyau 3",  235,  258, 
  TRUE ~ "Hors noyau")

# gluèyre 

calc_occupation_noyaux <- function(df, id_col, eff_col, breaks) {
  df <- df %>%
    mutate(
      id_num = as.numeric(as.character(.data[[id_col]])),
      eff = as.numeric(.data[[eff_col]])
    )
  
  # Attribution du noyau selon les bornes fournies (data.frame avec noyau/min/max)
  df$noyau <- "Hors noyau"
  for (i in seq_len(nrow(breaks))) {
    idx <- df$id_num >= breaks$min[i] & df$id_num <= breaks$max[i]
    df$noyau[idx] <- breaks$noyau[i]
  }
  
  df %>%
    group_by(noyau) %>%
    summarise(
      n_sites_total = n(),
      n_sites_occupes = sum(eff > 0, na.rm = TRUE),
      taux_occupation = 100 * n_sites_occupes / n_sites_total,
      effectif_total = sum(eff, na.rm = TRUE)
    ) %>%
    arrange(match(noyau, breaks$noyau))
}

breaks_2020_glu <- tribble(
  ~noyau,     ~min, ~max,
  "Noyau 1",  14,  34,
  "Noyau 2",  118,  175,
  "Noyau 3",  235,  258)

JEGO <- JEGO %>%
  mutate(noyau = case_when(
    ID_SITE >= 14 & ID_SITE <= 34 ~ "Noyau 1",
    ID_SITE >= 118   & ID_SITE <= 175 ~ "Noyau 2",
    ID_SITE >= 235   & ID_SITE <= 258 ~ "Noyau 3",
    TRUE ~ "Hors noyau"))


res_2020 <- calc_occupation_noyaux(JEGO, "Site", "Eff_max", breaks_2020_glu)
res_2020

PEIG <- read.csv(file = "IMPORT/2024/DONNEES_SONNEURS_2024_CP.csv") %>%
  mutate(X = as.numeric(gsub(",", ".", X)),Y = as.numeric(gsub(",", ".", Y))) %>% 
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

PEIG <- PEIG %>%
  st_drop_geometry() %>%
  mutate(
    Site = site, 
    OCC2024 = pmax(presence_P1, presence_P2)) %>%
  select(Site, OCC2024)

res_2024 <- calc_occupation_noyaux(PEIG, "Site", "OCC2024", breaks_2020_glu)
res_2024

data <- data %>%
  st_drop_geometry() %>%
  select(
    Site = ID_SITE,
    EFF2026 = EFF_MAX)

res_2026 <- calc_occupation_noyaux(data, "Site", "EFF2026", breaks_2020_glu)
res_2026
