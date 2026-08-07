# ce script a pour objectif d'importer et de formater les données de terrain afin de les analyser dans les prochains scripts 

################################################################################
#################### Script 01 : Import and formatting #########################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)
pacman::p_load(tidyverse, readxl, ggplot2, readr, leaflet, sf, stringr, zoo)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

#data <- sf::st_read("IMPORT/DONNEES_SONNEURS_2026_JR.shp") %>%
#  mutate(across(c(ID_SITE, L_site,
#      Nbr_adulte, Nbr_subadu,NBR_MARES, 
#      NBR_MAR_P2, NBR_MAR_P3,
#      Nbr_adu_P2, Nbr_sub_P2,
#      Nbr_adu_P3, Nbr_sub_P3),as.numeric)) %>% 
# rename(NBR_MAR_P1 = NBR_MARES, Nbr_adu_P1 = Nbr_adulte, Nbr_sub_P1 = Nbr_subadu, #Remarq_P1 = Remarques, Faune_P1 = Faune, Ind_rep_P1 = Indic_rep, OBS_P1 = OBS, #HABITAT_P1 = HABITAT, DATE_P1 = DATE, HEURE_P1 = HEURE) %>% 
#  select(ID_SITE, RIVE, HABITAT_P1, L_site, Ensol, 
#         DATE_P1, DATE_P2, DATE_P3, 
#         HEURE_P1, HEURE_P2, HEURE_P3, 
#         OBS_P1, OBS_P2, OBS_P3,
#         HABITAT_P1, HABITAT_P2, HABITAT_P3,
#         NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3,
#         Nbr_adu_P1, Nbr_adu_P2, Nbr_adu_P3,
#         Nbr_sub_P1, Nbr_sub_P2, Nbr_sub_P3,
#        Ind_rep_P1, Ind_rep_P2, Ind_rep_P3, 
#         Faune_P1, Faune_P2, Faune_P3, 
#         Remarq_P1,Remarq_P2, Remarq_P3, 
#         geometry)

str(data)

# Export data en shp 
#data %>% st_write("EXPORT/DONNEES_SONNEURS_2026_JR.shp", delete_layer = TRUE)

# ///  IMPORT DU JEU DE DONNNEES FORMATÉ /// ####

data <- sf::st_read("IMPORT/DONNEES_SONNEURS_2026_JR.shp")

# ///  VERIFICATION sur la SAISI DES DONNÉES /// ####

############################## ID_SITE ######################################### 

data %>% group_by(ID_SITE) %>% summarise(n = n()) %>% filter(n > 1)
# chaque site est unique : pas de doublon 

# 1 : SITES MANQUANTS au premier passage sur la base du renseignement de l'heure, du type d'habitat, du nombre de mares et de sonneurs adultes ainsi que de l'observateur

data %>% filter(is.na(HEURE_P1),is.na(HABITAT_P1),is.na(NBR_MAR_P1),is.na(OBS_P1), is.na(Nbr_adu_P1)) %>% select(ID_SITE, HABITAT_P1, NBR_MAR_P1, OBS_P1, Nbr_adu_P1)
# good 

# 2 : SITES MANQUANTS au deuxième passage sur la base du renseignement de l'heure, du type d'habitat, du nombre de mares et de sonneurs adultes ainsi que de l'observateur

data %>% filter(is.na(HEURE_P2),is.na(HABITAT_P2),is.na(NBR_MAR_P2),is.na(OBS_P2), is.na(Nbr_adu_P2)) %>% select(ID_SITE,HABITAT_P2, NBR_MAR_P2, OBS_P2, Nbr_adu_P2)
# pas good

# 3 : SITES MANQUANTS au troisième passage sur la base du renseignement de l'heure, du type d'habitat, du nombre de mares et de sonneurs adultes ainsi que de l'observateur

data %>% filter(is.na(HEURE_P3),is.na(HABITAT_P3),is.na(NBR_MAR_P3),is.na(OBS_P3), is.na(Nbr_adu_P3)) %>% select(ID_SITE, HABITAT_P3, NBR_MAR_P3, OBS_P3, Nbr_adu_P3)
# pas good

############################## DATES ######################################## 

# Inférer les dates pour le deuxième et troisième passage ainsi que pour les oublies de saisi pour le premier et le deuxième ou les dates ont été saisies à chaque site 

str(data)

data <- data %>% fill(
    DATE_P1,
    DATE_P2,
    DATE_P3,
    .direction = "downup") # downup pour dire que c'est dans l'ordre des sites

################################# HEURES ####################################### 

# Inférer les heures pour les oublies de saisis se basant sur l'horaire du site d'avant et de celui d'après en prenant le milieu

# toutes les fautes de saisi (différent de 4 chiffres sont mis en NA)

#library(stringr)

data <- data %>% arrange(ID_SITE) # dans l'odre des sites

# format heure : "^\\d{2}:\\d{2}$"
#library(zoo)

data <- data %>%
  mutate(
    HEURE_P1 = case_when(
      str_detect(HEURE_P1, "^\\d{4}$") ~
        paste0(substr(HEURE_P1, 1, 2), ":", substr(HEURE_P1, 3, 4)),
      str_detect(HEURE_P1, "^\\d{2}:\\d{2}$") ~ HEURE_P1,
      TRUE ~ NA_character_), 
    HEURE_P2 = case_when(
      str_detect(HEURE_P2, "^\\d{4}$") ~
        paste0(substr(HEURE_P2, 1, 2), ":", substr(HEURE_P2, 3, 4)),
      str_detect(HEURE_P2, "^\\d{2}:\\d{2}$") ~ HEURE_P2,
      TRUE ~ NA_character_),
    HEURE_P3 = case_when(
      str_detect(HEURE_P3, "^\\d{4}$") ~
        paste0(substr(HEURE_P3, 1, 2), ":", substr(HEURE_P3, 3, 4)),
      str_detect(HEURE_P3, "^\\d{2}:\\d{2}$") ~ HEURE_P3,
      TRUE ~ NA_character_))

# verif des heures manquantes 
data %>% is.na() %>% colSums()

# fonction pour convertir "HH:MM" en minutes depuis minuit
heure_to_min <- function(x) {
  h <- as.numeric(substr(x, 1, 2))
  m <- as.numeric(substr(x, 4, 5))
  h * 60 + m}

# fonction pour reconvertir minutes en "HH:MM"
min_to_heure <- function(x) {
  x <- round(x)
  h <- floor(x / 60) %% 24
  m <- round(x %% 60)
  sprintf("%02d:%02d", h, m)}

data <- data %>%
  arrange(ID_SITE) %>%
  mutate(
    HEURE_P1_min = heure_to_min(HEURE_P1),
    HEURE_P2_min = heure_to_min(HEURE_P2),
    HEURE_P3_min = heure_to_min(HEURE_P3),
    HEURE_P1_min = na.approx(HEURE_P1_min, na.rm = FALSE, rule = 2),
    HEURE_P2_min = na.approx(HEURE_P2_min, na.rm = FALSE, rule = 2),
    HEURE_P3_min = na.approx(HEURE_P3_min, na.rm = FALSE, rule = 2),
    HEURE_P1 = ifelse(is.na(HEURE_P1), min_to_heure(HEURE_P1_min), HEURE_P1),
    HEURE_P2 = ifelse(is.na(HEURE_P2), min_to_heure(HEURE_P2_min), HEURE_P2),
    HEURE_P3 = ifelse(is.na(HEURE_P3), min_to_heure(HEURE_P3_min), HEURE_P3)) %>%
  select(-HEURE_P1_min, -HEURE_P2_min, -HEURE_P3_min)

# verification
data %>% select(HEURE_P1, HEURE_P2, HEURE_P3) %>% is.na() %>% colSums()
# c'est good

######################### TRAITEMENT DES ZEROS ################################# 

# MAUVAISE IDÉE tant pour le nombre de mares que de sonneurs : inflation de zéros
# remplacer tout les NA de la colonne NBR_MARES par 0
#data$NBR_MAR_P1[is.na(data$NBR_MAR_P1)] <- 0
#data$NBR_MAR_P2[is.na(data$NBR_MAR_P2)] <- 0
#data$NBR_MAR_P3[is.na(data$NBR_MAR_P3)] <- 0  

# Transformation des 0 dans les colonnes Nbr_adu_P1 P2 P3 et Nbr_sub_P1 P2 P3 en NA

data <- data %>% mutate(across(c(Nbr_adu_P1, Nbr_adu_P2, Nbr_adu_P3, Nbr_sub_P1, Nbr_sub_P2, Nbr_sub_P3), ~ na_if(., 0)))

############################# AJOUT RIVIERE #################################### 

data <- data %>%
  mutate(RIVIERE = case_when(
    ID_SITE >= 2.00 & ID_SITE <= 315.50 ~ "Glueyre",
    ID_SITE >= 317.01 & ID_SITE <= 590.00 ~ "Eyrieux",
    TRUE ~ NA_character_))

########################### FAUNE (Pelophylax) ################################# 

# Transformer dans FAUNE_P1, FAUNE_P2 et FAUNE_P3 les Pelophylax en P 

data <- data %>%
  mutate(across(c(Faune_P1, Faune_P2, Faune_P3), ~ ifelse(. == "Pelophylax", "P", .)))

############################# TYPE HABITAT ##################################### 

# Pour les oublies de saisis sur le type d'habitat, mettre un C quand plus de mares que 1 et M lorsque le nombre de mares est = à 1 pour le NBR_MAR_P1 

data <- data %>%
  mutate(HABITAT_P1 = case_when(
    is.na(HABITAT_P1) & NBR_MAR_P1 > 1 ~ "C",
    is.na(HABITAT_P1) & NBR_MAR_P1 == 1 ~ "M",
    TRUE ~ HABITAT_P1))

######################### Effectif maximum compté ############################## 

# Créer une colonne EFF_MAX pour avoir les effectifs maximumus comptés pour chaque site 
data <- data %>%
mutate(
  EFF_MAX = pmax(
    rowSums(cbind(Nbr_adu_P1, Nbr_sub_P1), na.rm = TRUE),
    rowSums(cbind(Nbr_adu_P2, Nbr_sub_P2), na.rm = TRUE),
    rowSums(cbind(Nbr_adu_P3, Nbr_sub_P3), na.rm = TRUE),
    na.rm = TRUE),
  EFF_MAX = ifelse(is.infinite(EFF_MAX), NA_real_, EFF_MAX))

######################### Effectif maximum compté ############################## 

# Export data en shp 

#data %>% st_write("EXPORT/DONNEES_SONNEURS_2026_JR.shp", delete_layer = TRUE)

######################### Sonneur TOT par passage ############################## 


#data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp")
#data <- data %>%
#  mutate(SONNEURS_P1 = coalesce(Nbr_adu_P1, 0) + coalesce(Nbr_sub_P1, 0), 
#        SONNEURS_P2 = coalesce(Nbr_adu_P2, 0) + coalesce(Nbr_sub_P2, 0), 
#         SONNEURS_P3 = coalesce(Nbr_adu_P3, 0) + coalesce(Nbr_sub_P3, 0))
#data <- data %>%
#  rename(SONN_P1 = SONNEURS_P1,
#        SONN_P2 = SONNEURS_P2,
#         SONN_P3 = SONNEURS_P3)
#
# data %>% st_write("EXPORT/DONNEES_SONNEURS_2026_JR.shp", delete_dsn = TRUE)
#
# data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp")
