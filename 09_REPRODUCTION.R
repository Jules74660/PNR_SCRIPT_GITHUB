
################################################################################
################## Script 08 : Indices de repro #######################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)

pacman::p_load(readr, ggplot2, tidyverse, dplyr, gt, patchwork, sf, viridis, shadowtext, grid,png, ggtext,ggspatial, prettymapr, leaflet, terra, data.table, stringr)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp") 

reprod <- data %>% select(ID_SITE, DATE_P1, DATE_P2, DATE_P3, Ind_rep_P1, Ind_rep_P2, Ind_rep_P3, EFF_MAX, SONN_P1,SONN_P2, SONN_P3, RIVIERE)

str(reprod)
# en gros il y a 5 modalités : J, P, T, A et entre des + si il y en a plusieurs sur un sit, l'objectif est d'homogénéiser ces modalités dans les 3 colonnes avec un ordre précis, A+T+P+J. Lorsque dans chcune des trois colonnes Ind_rep_P1, Ind_rep_P2, Ind_rep_P3 ces colonnes seront organisés, une autre colonne devra etre crée avec les indices de repro des 3 passages donc pas mettre 3 fois A ou J mais jsute mettre la cominaison max qu'il peut avoir sur les 3 passages

# Fonction pour remettre dans l'ordre A + T + P + J
ordre_rep <- function(x) {
  if (is.na(x) | x == "") return(NA_character_)
  modalites <- unlist(str_split(x, "\\+"))
  ordre <- c("A", "T", "P", "J")
  modalites <- ordre[ordre %in% modalites]
  paste(modalites, collapse = "+")}

# Fonction pour obtenir l'indice maximal entre plusieurs passages
max_rep <- function(...) {
  x <- c(...)
  # supprimer NA
  x <- x[!is.na(x) & x != ""]
  if(length(x) == 0) return(NA_character_)
  # récupérer toutes les modalités présentes
  modalites <- unique(unlist(str_split(x, "\\+")))
  ordre <- c("A", "T", "P", "J")
  # garder celles qui existent et respecter l'ordre
  modalites <- ordre[ordre %in% modalites]
  paste(modalites, collapse = "+")}


reprod <- reprod %>%
  mutate(
    Ind_rep_P1 = sapply(Ind_rep_P1, ordre_rep),
    Ind_rep_P2 = sapply(Ind_rep_P2, ordre_rep),
    Ind_rep_P3 = sapply(Ind_rep_P3, ordre_rep)) %>%
  rowwise() %>%
  mutate(
    Ind_rep_global = max_rep(
      Ind_rep_P1,
      Ind_rep_P2,
      Ind_rep_P3)) %>%
  ungroup()

reprod <- reprod %>%
  mutate(Eff_indice = ifelse(EFF_MAX == 0, 0, 1))

reprod %>% st_write("IMPORT/INDIC_REPRO.shp", delete_dsn = TRUE)

stats_repro_riviere <- purrr::map_dfr(1:3, function(i){
  
  indice <- paste0("Ind_rep_P", i)
  sonn <- paste0("SONN_P", i)
  
  reprod %>%
    st_drop_geometry() %>%   # important si reprod est encore un objet sf
    group_by(RIVIERE) %>%
    summarise(
      Sites_avec_indice_repro = sum(!is.na(.data[[indice]]) & .data[[indice]] != ""),
      
      Sites_avec_adultes_sur_sites_repro = sum(
        !is.na(.data[[indice]]) & .data[[indice]] != "" &
          .data[[sonn]] > 0,
        na.rm = TRUE
      ),
      
      Nombre_total_adultes_sur_sites_repro = sum(
        ifelse(
          !is.na(.data[[indice]]) & .data[[indice]] != "",
          .data[[sonn]],
          0
        ),
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    mutate(Passage = paste0("P", i))
  
}) %>%
  select(Passage, RIVIERE, everything())

stats_repro_riviere %>% view()

# Sites occupés
sites_occupes <- reprod %>%
  filter(EFF_MAX > 0)

# Pourcentage de sites occupés avec un indice de reproduction
pourcentage_repro <- sites_occupes %>%
  summarise(Sites_occupes = n(),
    Sites_avec_indice = sum(!is.na(Ind_rep_global) & Ind_rep_global != ""),
    Pourcentage = round(100 * Sites_avec_indice / Sites_occupes, 1))

pourcentage_repro

pourcentage_repro_riviere <- reprod %>%
  filter(EFF_MAX > 0) %>%
  group_by(RIVIERE) %>%
  summarise(
    Sites_occupes = n(),
    Sites_avec_indice = sum(!is.na(Ind_rep_global) & Ind_rep_global != ""),
    Pourcentage = round(100 * Sites_avec_indice / Sites_occupes, 1),
    .groups = "drop")

pourcentage_repro_riviere

# Pourcentage de Têtards, de Ponte, de Juvéniles et d'Amplexus sur le sites (pas les combinaisons)

library(purrr)

stats_passage <- map_dfr(1:3, function(i){
  
  sonn <- paste0("SONN_P", i)
  indice <- paste0("Ind_rep_P", i)
  
  reprod %>%
    st_drop_geometry() %>%
    group_by(RIVIERE) %>%
    summarise(
      Sites_occupes = sum(.data[[sonn]] > 0, na.rm = TRUE),
      
      Amplexus = sum(stringr::str_detect(coalesce(.data[[indice]], ""), "\\bA\\b")),
      Pontes   = sum(stringr::str_detect(coalesce(.data[[indice]], ""), "\\bP\\b")),
      Tetards  = sum(stringr::str_detect(coalesce(.data[[indice]], ""), "\\bT\\b")),
      Juveniles= sum(stringr::str_detect(coalesce(.data[[indice]], ""), "\\bJ\\b")),
      
      .groups = "drop"
    ) %>%
    mutate(Passage = paste0("P", i))
}) %>%
  select(Passage, RIVIERE, everything())

stats_passage

# JEGO : 

JEGO <- sf::st_read("IMPORT/2020/DONNEES_SITES_Jego_2020.shp")
JEGO <- JEGO %>% select(NOM_MILIEU, ID,Date_P1 ,Date_P2, Indices__1, Indices__2, Eff_tot_P1, Eff_tot_P2)
str(JEGO)
summary(JEGO)

JEGO <- JEGO %>%
  mutate(Ind_rep_global = case_when(
      Indices__1 != "Aucuns" | Indices__2 == "Oui" ~ "Oui",
      TRUE ~ NA_character_),
    Eff_max = pmax(Eff_tot_P1, Eff_tot_P2, na.rm = TRUE))

stats_2020 <- JEGO %>%
  #filter(NOM_MILIEU == "Eyrieux") %>%
  st_drop_geometry() %>%
  summarise(Sites_occupes = sum(Eff_max > 0),
    Sites_avec_IR = sum(Eff_max > 0 & !is.na(Ind_rep_global)),
    Pourcentage = round(100 * Sites_avec_IR / Sites_occupes, 1),
    
    Nb_individus = sum(Eff_max),
    
    Nb_individus_sites_IR = sum(
      ifelse(!is.na(Ind_rep_global), Eff_max, 0)))

stats_2020

# 2024
PEIG <- read.csv(file = "IMPORT/2024/DONNEES_SONNEURS_2024_CP.csv") %>%
  mutate(X = as.numeric(gsub(",", ".", X)),Y = as.numeric(gsub(",", ".", Y))) %>% 
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

str(PEIG)
summary(PEIG)

PEIG <- PEIG %>%
  mutate(Reprod_global = ifelse(Reprod_P1 == 1 | Reprod_P2 == 1, 1, 0),
    Presence_global = ifelse(presence_P1 == 1 | presence_P2 == 1, 1, 0))

stats_2024 <- PEIG %>%
  st_drop_geometry() %>%
  group_by(ce) %>%
  summarise(
    Sites_occupes = sum(Presence_global == 1),
    Sites_avec_IR = sum(Presence_global == 1 & Reprod_global == 1),
    Pourcentage = round(100 * Sites_avec_IR / Sites_occupes, 1),
    .groups = "drop")

stats_2024
