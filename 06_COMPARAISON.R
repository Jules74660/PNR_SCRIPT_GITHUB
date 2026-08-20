# ce script a pour objectif de rassembler l'ensemble des données pour pouvoir les rendre comparables

################################################################################
############## Script 06 : couplage données pour comparaison ###################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)

pacman::p_load(readr, ggplot2, tidyverse, dplyr, gt, patchwork, sf, viridis, shadowtext, grid,png, ggtext,ggspatial, prettymapr, leaflet, terra, data.table)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

#### Données de base de 2026 : ROES ####

ROES <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp") %>% st_transform(crs = 2154) 

str(ROES)
summary(ROES)

#### Données de 2007 : Boitier ####

BOIT <-  sf::st_read("IMPORT/2007/DONNEES_SONNEURS_Boitier_2007.shp") %>% st_transform(crs = 2154) 

#BOIT <- BOIT %>% filter(!st_is_empty(geometry))
#ggplot(BOIT) +
#  annotation_map_tile(type = "osm", zoomin = 0) +
#  geom_sf(aes(color = Secteur), size = 3) +
#  theme_minimal()

# très peu de données et que sur la Gluèyre
# analyse à part 

#### Données de 2012 : Ducasse ####

DUCA <- sf::st_read("IMPORT/2012/DONNEES_SONNEURS_Ducasse_2012.shp") %>% st_transform(crs = 2154) 

str(DUCA)
summary(DUCA)
#### Données de 2012 : Jego ####

JEGO <-  sf::st_read("IMPORT/2020/DONNEES_SONNEURS_Jego_2020.shp")  %>% st_transform(crs = 2154) 
summary(JEGO)
#### Données de 2024 : Peigné ####

PEIG <- read.csv(file = "IMPORT/2024/DONNEES_SONNEURS_2024_CP.csv") %>%
  mutate(X = as.numeric(gsub(",", ".", X)),Y = as.numeric(gsub(",", ".", Y))) %>% 
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE)

summary(PEIG)

# Association des sites avec effectifs des sonneurs sur les différents passages au jeu de données 2026 (ROES)

#Les suivis de 2020 (JEGO) et de 2024 (PEIG) reposent sur les mêmes sites, identifiés par une numérotation commune, ce qui permet une comparaison directe. En revanche, les nouveaux sites prospectés en 2026 ne peuvent donc pas être inclus dans cette comparaison. En 2012, l’unité d’échantillonnage était le « patch », défini comme un ensemble d’au moins trois vasques distantes de moins de 100m. Les identifiants des sites actuels ne peuvent donc pas être comparés tel quel, sans harmoniser l’unité d'échantillonnage. Pour harmoniser les unités, une zone tampon de 100m de rayon a été créée autour de chaque point GPS de 2012, étant donné que les limites des patchs ne sont plus disponibles. Tous les sites de 2026 inclus dans cette zone ont été regroupés et assimilés au patch correspondant. Lorsque plusieurs patchs étaient proches, ils ont été regroupés afin de limiter les confusions et les doubles comptages (tableau des correspondances en annexe). De cette façon, les données de 2012 (DUCA) sont spatialement et fondamentalement comparables aux données de 2020, 2024 et 2026. 

# //// ÉTAPE 1 : BUFFER 100M AUTOUR DES POINTS DUCA (2012) /// ####

DUCA_buff <- st_buffer(DUCA, dist = 100)
ROES_patch <- st_join(
  ROES,
  DUCA_buff["Site"],
  join = st_within,
  left = TRUE)

ROES_patch_sum <- ROES_patch %>%
  filter(!is.na(Site)) %>%
  st_drop_geometry() %>%
  group_by(Site) %>%
  summarise(
    EFF_MAX_2026 = sum(EFF_MAX, na.rm = TRUE),
    n_sites = n())

Comparaison_2012_2026 <-
  DUCA %>%
  st_drop_geometry() %>%
  select(
    Site,
    Effectif.m,
    Estimation,
    Nb.session
  ) %>%
  left_join(ROES_patch_sum, by = "Site")

# comparer l'estimation de l'abondance sur ces sites lorsque le modèle aura fonctionné

Comparaison_2012_2026 <- Comparaison_2012_2026 %>%
  mutate(
    Occ2012 = Effectif.m > 0,
    Occ2026 = EFF_MAX_2026 > 0
  )

Comparaison_2012_2026 %>%
  summarise(
    Occupation2012 = 100 * mean(Occ2012, na.rm = TRUE),
    Occupation2026 = 100 * mean(Occ2026, na.rm = TRUE))

# JEGO et PEIG
str(JEGO)
summary(JEGO)

str(PEIG)
summary(PEIG)

ROES_comp <- ROES %>%
  st_drop_geometry() %>%
  select(
    Site = ID_SITE,
    EFF2026 = EFF_MAX)

JEGO_comp <- JEGO %>%
  st_drop_geometry() %>%
  mutate(
    Site = as.numeric(Site),
    EFF2020 = as.numeric(Eff_max)
  ) %>%
  select(Site, EFF2020)

PEIG_comp <- PEIG %>%
  st_drop_geometry() %>%
  mutate(
    Site = site,
    OCC2024 = pmax(presence_P1, presence_P2)
  ) %>%
  select(Site, OCC2024)

Comparaison_2020_2024_2026 <-
  full_join(JEGO_comp, PEIG_comp, by = "Site") %>%
  full_join(ROES_comp, by = "Site")

JEGO_comp %>% count(Site) %>% filter(n > 1)
PEIG_comp %>% count(Site) %>% filter(n > 1)
ROES_comp %>% count(Site) %>% filter(n > 1)

JEGO_comp <- JEGO %>%
  st_drop_geometry() %>%
  mutate(
    Site = as.numeric(Site),
    EFF2020 = as.numeric(Eff_max)
  ) %>%
  group_by(Site) %>%
  summarise(EFF2020 = max(EFF2020, na.rm = TRUE), .groups = "drop")

PEIG_comp <- PEIG %>%
  st_drop_geometry() %>%
  mutate(
    Site = site,
    OCC2024 = pmax(presence_P1, presence_P2)
  ) %>%
  group_by(Site) %>%
  summarise(OCC2024 = max(OCC2024, na.rm = TRUE), .groups = "drop")

ROES_comp <- ROES %>%
  st_drop_geometry() %>%
  transmute(
    Site = ID_SITE,
    EFF2026 = EFF_MAX) %>%
  group_by(Site) %>%
  summarise(EFF2026 = max(EFF2026, na.rm = TRUE), .groups = "drop")

Comparaison_2020_2024_2026 <-
  full_join(JEGO_comp, PEIG_comp, by = "Site") %>%
  full_join(ROES_comp, by = "Site")

Comparaison_2020_2024_2026 <-
  JEGO_comp %>%
  inner_join(PEIG_comp, by = "Site") %>%
  inner_join(ROES_comp, by = "Site")

# nombre de mares au fil des années 

PEIG %>% group_by(ce) %>%
  summarise(Passage_1 = sum(nb_mares_P1, na.rm = TRUE),
    Passage_2 = sum(nb_mares_P2, na.rm = TRUE))

PEIG %>% filter(ce == "Eyrieux") %>% summarise(sum(nb_mares_P1, na.rm = TRUE))
PEIG %>% filter(ce == "Eyrieux") %>% summarise(sum(nb_mares_P2, na.rm = TRUE))

PEIG %>% filter(ce == "Glueyre") %>% summarise(sum(nb_mares_P1, na.rm = TRUE))
PEIG %>% filter(ce == "Glueyre") %>% summarise(sum(nb_mares_P2, na.rm = TRUE))

JEGOsite <-  sf::st_read("IMPORT/2020/DONNEES_SITES_Jego_2020.shp")  %>% st_transform(crs = 2154) 
JEGOsite %>% filter(NOM_MILIEU == "Glueyre") %>% summarise(sum(Nvasques_m, na.rm = TRUE))
JEGOsite %>% filter(NOM_MILIEU == "Glueyre") %>% summarise(sum(Nvasques_1, na.rm = TRUE))

# perte moyenne de mares par site du P1 au P2 et du P2 au P3 sur l'Eyrieux et la Gluèyre
ROES %>% filter(RIVIERE == "Eyrieux") %>%
  summarise(P1_P2 = mean(NBR_MAR_P2 - NBR_MAR_P1, na.rm = TRUE),
    P2_P3 = mean(NBR_MAR_P3 - NBR_MAR_P2, na.rm = TRUE))

ROES %>% filter(RIVIERE == "Glueyre") %>%
  summarise(P1_P2 = mean(NBR_MAR_P2 - NBR_MAR_P1, na.rm = TRUE),
            P2_P3 = mean(NBR_MAR_P3 - NBR_MAR_P2, na.rm = TRUE))


ROES %>%
  group_by(RIVIERE) %>%
  summarise(
    Moy_P1 = mean(NBR_MAR_P1, na.rm = TRUE),
    Moy_P2 = mean(NBR_MAR_P2, na.rm = TRUE),
    Moy_P3 = mean(NBR_MAR_P3, na.rm = TRUE),
    .groups = "drop")

ROES %>%
  group_by(RIVIERE) %>%
  summarise(n_sites = n(),
    P1 = 100 * mean(SONN_P1 > 0, na.rm = TRUE),
    P2 = 100 * mean(SONN_P2 > 0, na.rm = TRUE),
    P3 = 100 * mean(SONN_P3 > 0, na.rm = TRUE))

ROES %>%
  group_by(RIVIERE) %>%
  summarise(
    n_sites = n(),
    P1 = 100 * mean(SONN_P1 > 0, na.rm = TRUE),
    P2 = 100 * mean(SONN_P2 > 0, na.rm = TRUE),
    P3 = 100 * mean(SONN_P3 > 0, na.rm = TRUE),
    Global = 100 * mean(
      (SONN_P1 > 0 | SONN_P2 > 0 | SONN_P3 > 0),
      na.rm = TRUE))

library(dplyr)


ROES %>%
  rowwise() %>%
  mutate(max_site = max(c(SONN_P1, SONN_P2, SONN_P3), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(max_site > 0) %>%  # ne garder que les sites occupés
  group_by(RIVIERE) %>%
  summarise(
    n_sites_occupes = n(),
    min_max_site = min(max_site, na.rm = TRUE),
    max_max_site = max(max_site, na.rm = TRUE),
    moyenne_par_site_occupe = mean(max_site, na.rm = TRUE),
    sd_par_site_occupe = sd(max_site, na.rm = TRUE))
