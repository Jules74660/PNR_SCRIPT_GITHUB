# ce script a pour objectif de faire de rassembler les covariables dans l'objectif de faire tourner dans le script prochain les modèles d'estimation de l'abondance relative ainsi que de l'occupation

################################################################################
################## Script 02 : Statistiques descriptives #######################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)

pacman::p_load(readr, ggplot2, tidyverse, dplyr, gt, patchwork, sf, viridis, shadowtext, grid,png, ggtext,ggspatial, prettymapr, leaflet, terra, data.table)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp")

data <- data %>% select(ID_SITE, RIVIERE, RIVE, L_site, Ensol, DATE_P1, DATE_P2, DATE_P3, HEURE_P1, HEURE_P2, HEURE_P3, OBS_P1, OBS_P2, OBS_P3,
                        HABITAT_P1, HABITAT_P2, HABITAT_P3,
                        NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3,
                        Nbr_adu_P1, Nbr_adu_P2, Nbr_adu_P3,
                        Nbr_sub_P1, Nbr_sub_P2, Nbr_sub_P3, 
                        SONN_P1, SONN_P2, SONN_P3,
                        EFF_MAX, MAR_MAX)

# SET de la projection pour toutes les couches #

data <- data %>% st_transform(crs = 2154)

# VERIF toutes les dates de P1, P2 et P3
# P1 
summary(data$DATE_P1)
str(data$DATE_P1)
which(format(data$DATE_P1, "%Y") == "2027")
data$DATE_P1[75]
data$DATE_P1[75] <- data$DATE_P1[75] - years(1)
# P2
str(data$DATE_P2)
which(format(data$DATE_P2, "%Y") == "2027") # pas de 2027
# P3
str(data$DATE_P3)
which(format(data$DATE_3, "%Y") == "2027") # pas de 2027

# verif 

summary(data$DATE_P1)
summary(data$DATE_P2) # couille paté
summary(data$DATE_P3)

# P2 
which(format(data$DATE_P2, "%Y") == "0026")
data$DATE_P2[which(year(data$DATE_P2) < 2020)]
data$DATE_P2[704]
year(data$DATE_P2[704]) <- 2026
data$DATE_P2[704] 

summary(data$DATE_P1)
summary(data$DATE_P2) # plus de couille paté
summary(data$DATE_P3)
# c'est good

# /// COVARIABLES /// ####

# Altitude
# Densité de vasques (car . ..)
# Nombre maximum de vasques par site 
# Densité du réseau hydrographique 
# Distance à l'ouvrage
# Densité du réseau routier 
# Surface de forêts 
# Surface de zones urbanisées 
# Température 
# Pelophylax 
# Observateur
# Rive 

########################### ___ Altitude ___#####################################

# Données issues de la BD ALTI
# Format asc

#library(terra)

# Liste des fichiers sous format asc
fichiers <- list.files("IMPORT/01__COVARIABLES__/BD_ALTI/BDALTIV2/1_DONNEES_LIVRAISON_2023-01-00224/BDALTIV2_MNT_25M_ASC_LAMB93_IGN69_D007", pattern = "\\.asc$", full.names = TRUE)

# Transforamtion en raster 
rasters <- lapply(fichiers, rast)

# Transformation en un seul fichier
raster_un <- do.call(mosaic, rasters)
merged_raster <- do.call(merge, rasters)
plot(merged_raster) # verif (c'est good)

# Bonne projection 
crs(merged_raster) <- "EPSG:2154"

# Sortie en un seul fichier tif pour plus facile à manipuler
writeRaster(merged_raster, "IMPORT/01__COVARIABLES__/BD_ALTI/mnt_ardeche.tif", overwrite = TRUE)

ALTI <- rast("IMPORT/01__COVARIABLES__/BD_ALTI/mnt_ardeche.tif")

# Association de l'altitude à chacun des points du jeu de données principal
data$ALTI <- terra::extract(ALTI, vect(data))[, 2]

save(data, file = "SAUVEGARDES/data_ALTI.RData")
load(file = "SAUVEGARDES/data_ALTI.RData") # .. Sauvegarde intermédiaire N°1 .. ####

anyNA(data$ALTI) # YE 

############### ___ Densité de mares dans un rayon de 50m ___ ##################

# basé sur le maximum de mares (MAR_MAX) et non pas une moyenne sur les 3 passages (choix méthodo à expliquer)

# créer un buffer de 50m autour des points
buffers <- st_buffer(data, dist = 50)

# Intégration de chaque point dans les buffers
intersections <- st_intersects(buffers, data)

# Pour chaque site sommmer les MAR_MAX dans le buffer

data$dens_mares_50m <- sapply(intersections, function(idx) {
  sum(data$MAR_MAX[idx], na.rm = TRUE)})

summary(data$dens_mares_50m)
max(data$dens_mares_50m)

################ ___ Nombre maximum de vasques par site  ___ ###################

# C'est MAR_MAX déjà

####################### ___ Distance à l'ouvrage  ___ ##########################

ouvrage <- sf::st_read("IMPORT/01__COVARIABLES__/ouvrage/ouvrages_suivi.shp")

# créer une géométrie car que colonne XL93 et YL93
ouvrage <- st_as_sf(ouvrage,coords = c("XL93", "YL93"),crs = 2154, remove = FALSE)

# Calculer la distance de chaque point à un ouvrage 
distance <- st_distance(data, ouvrage)

# Garder la distance miminale à un ouvrage pour chaque site
data$OUVRAGE <- apply(distance, 1, min)

######################## ___ Surface de forêts   ___ ###########################

foret <- sf::st_read("IMPORT/01__COVARIABLES__/foret/forêt.shp")
foret <- foret %>% st_transform(crs = 2154)

# 100m
buffers_100m <- st_buffer(data, dist = 100)
intersections_100m <- st_intersection(foret, buffers_100m)
intersections_100m$surface_m2 <- st_area(intersections_100m)

surface_foret_100m <- intersections_100m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(surface_foret_100m = sum(as.numeric(surface_m2), na.rm = TRUE))

data <- data %>% left_join(surface_foret_100m, by = "ID_SITE")
data$surface_foret_100m[is.na(data$surface_foret_100m)] <- 0

# 500m 
buffers_500m <- st_buffer(data, dist = 500)
intersections_500m <- st_intersection(foret, buffers_500m)
intersections_500m$surface_m2 <- st_area(intersections_500m)

surface_foret_500m <- intersections_500m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(surface_foret_500m = sum(as.numeric(surface_m2), na.rm = TRUE))

data <- data %>% left_join(surface_foret_500m, by = "ID_SITE")
data$surface_foret_500m[is.na(data$surface_foret_500m)] <- 0

summary(data$surface_foret_100m)
summary(data$surface_foret_500m)

save(data, file = "SAUVEGARDES/data_FORET.RData")
load(file = "SAUVEGARDES/data_FORET.RData") # .. Sauvegarde intermédiaire N°2 .. ####

#################### ___  Surface de zones urbanisées  ___ #####################

bati <- sf::st_read("IMPORT/01__COVARIABLES__/bati/bâti.shp")
bati <- bati %>% st_transform(crs = 2154)

# 100m
buffers_100m <- st_buffer(data, dist = 100)
intersections_100m <- st_intersection(bati, buffers_100m)
intersections_100m$surface_m2 <- st_area(intersections_100m)

surface_bati_100m <- intersections_100m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(surface_bati_100m = sum(as.numeric(surface_m2), na.rm = TRUE))

data <- data %>% left_join(surface_bati_100m, by = "ID_SITE")
data$surface_bati_100m[is.na(data$surface_bati_100m)] <- 0

# 500m 
buffers_500m <- st_buffer(data, dist = 500)
intersections_500m <- st_intersection(bati, buffers_500m)
intersections_500m$surface_m2 <- st_area(intersections_500m)

surface_bati_500m <- intersections_500m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(surface_bati_500m = sum(as.numeric(surface_m2), na.rm = TRUE))

data <- data %>% left_join(surface_bati_500m, by = "ID_SITE")
data$surface_bati_500m[is.na(data$surface_bati_500m)] <- 0

summary(data$surface_bati_100m)
summary(data$surface_bati_500m)

save(data, file = "SAUVEGARDES/data_BATI.RData")
load(file = "SAUVEGARDES/data_BATI.RData") # .. Sauvegarde intermédiaire N°3 .. ####

################# ___ Densité du réseau hydrographique  ___ ####################

# Calcul de la densité du réseau hydrographique dans un rayon de 100 et 500 m autour de chaque site

RH <- sf::st_read("IMPORT/01__COVARIABLES__/BDTOPO/1_DONNEES_LIVRAISON_2026-06-00412/BDT_3-5_SHP_LAMB93_D007_ED2026-06-15/HYDROGRAPHIE/COURS_D_EAU.shp")

RH <- RH %>% filter(TOPONYME != "la Glueyre" & TOPONYME != "l'Eyrieux") 

RH <- RH %>% st_transform(crs = 2154) # bonne proj
RH <- st_make_valid(RH)

# Recadrer à l'emprise de ton étude pour éfficacité calcul
emprise <- st_buffer(st_union(data), dist = 600)
RH <- st_intersection(RH, emprise)

# ---- 100m ----
buffers_100m <- st_buffer(data, dist = 100)
intersections_100m <- st_intersection(RH, buffers_100m)
intersections_100m$longueur_m <- st_length(intersections_100m)

longueur_hydro_100m <- intersections_100m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(longueur_hydro_100m = sum(as.numeric(longueur_m), na.rm = TRUE))

data <- data %>% left_join(longueur_hydro_100m, by = "ID_SITE")
data$longueur_hydro_100m[is.na(data$longueur_hydro_100m)] <- 0

# m/km²
data$dens_hydro_100m <- (data$longueur_hydro_100m / (pi * 100^2)) * 1e6

# ---- 500m ----
buffers_500m <- st_buffer(data, dist = 500)
intersections_500m <- st_intersection(RH, buffers_500m)
intersections_500m$longueur_m <- st_length(intersections_500m)

longueur_hydro_500m <- intersections_500m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(longueur_hydro_500m = sum(as.numeric(longueur_m), na.rm = TRUE))

data <- data %>% left_join(longueur_hydro_500m, by = "ID_SITE")
data$longueur_hydro_500m[is.na(data$longueur_hydro_500m)] <- 0

data$dens_hydro_500m <- (data$longueur_hydro_500m / (pi * 500^2))* 1e6

# Vérification
summary(data$dens_hydro_100m)
summary(data$dens_hydro_500m)

##################### ___  Densité du réseau routier  ___ ######################

ROUTE <- sf::st_read("IMPORT/01__COVARIABLES__/routes/routes_suivi.shp")
ROUTE <- ROUTE %>% st_transform(crs = 2154) # bonne proj
ROUTE <- st_make_valid(ROUTE)

# Recadrer à l'emprise de ton étude pour éfficacité calcul
emprise <- st_buffer(st_union(data), dist = 600)
ROUTE <- st_intersection(ROUTE, emprise)

# ---- 100m ----
buffers_100m <- st_buffer(data, dist = 100)
intersections_100m <- st_intersection(ROUTE, buffers_100m)
intersections_100m$longueur_m <- st_length(intersections_100m)

longueur_route_100m <- intersections_100m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(longueur_route_100m = sum(as.numeric(longueur_m), na.rm = TRUE))

data <- data %>% left_join(longueur_route_100m, by = "ID_SITE")
data$longueur_route_100m[is.na(data$longueur_route_100m)] <- 0

# m/km²
data$dens_route_100m <- (data$longueur_route_100m / (pi * 100^2)) * 1e6

# ---- 500m ----
buffers_500m <- st_buffer(data, dist = 500)
intersections_500m <- st_intersection(ROUTE, buffers_500m)
intersections_500m$longueur_m <- st_length(intersections_500m)

longueur_route_500m <- intersections_500m %>%
  st_drop_geometry() %>%
  group_by(ID_SITE) %>%
  summarise(longueur_route_500m = sum(as.numeric(longueur_m), na.rm = TRUE))

data <- data %>% left_join(longueur_route_500m, by = "ID_SITE")
data$longueur_route_500m[is.na(data$longueur_route_500m)] <- 0

data$dens_route_500m <- (data$longueur_route_500m / (pi * 500^2))* 1e6

# Vérification
summary(data$dens_route_100m)
summary(data$dens_route_500m)

# Sélection des variables pour les modèles 

data <- data %>% select(ID_SITE, RIVIERE, RIVE, L_site, Ensol, DATE_P1, DATE_P2, DATE_P3, HEURE_P1, HEURE_P2, HEURE_P3, OBS_P1, OBS_P2, OBS_P3,
                        HABITAT_P1, HABITAT_P2, HABITAT_P3,
                        NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3,
                        Nbr_adu_P1, Nbr_adu_P2, Nbr_adu_P3,
                        Nbr_sub_P1, Nbr_sub_P2, Nbr_sub_P3,
                        SONN_P1, SONN_P2, SONN_P3,
                        EFF_MAX, MAR_MAX,
                        ALTI,
                        dens_mares_50m,
                        OUVRAGE,
                        surface_foret_100m,surface_foret_500m,
                        surface_bati_100m,surface_bati_500m,
                        dens_hydro_100m,dens_hydro_500m,
                        dens_route_100m,dens_route_500m)

# Renommer certaines colonnes

data <- data %>% rename(MAR50 = dens_mares_50m, F100 = surface_foret_100m, F500 = surface_foret_500m, B100 = surface_bati_100m, B500 = surface_bati_500m, DH100 = dens_hydro_100m, DH500 = dens_hydro_500m, DR100 = dens_route_100m, DR500 = dens_route_500m)

save(data, file = "SAUVEGARDES/data_covs.RData")
load(file = "SAUVEGARDES/data_covs.RData") # .. Sauvegarde intermédiaire N°4 .. ####


# /// Données environnementales /// ####

#################### ___  Débit de la rivière ___ #####################

H_GLU <- read_csv("IMPORT/01__COVARIABLES__/DEBIT/H_Glueyre.csv") %>% dplyr::select("Date (TU)", "Valeur (en m)") %>% rename(DATE = "Date (TU)", H_instant = "Valeur (en m)")

Q_GLU <- read_csv("IMPORT/01__COVARIABLES__/DEBIT/Q_Glueyre.csv") %>% dplyr::select("Date (TU)", "Valeur (en m³/s)") %>% rename(DATE = "Date (TU)", Q_instant = "Valeur (en m³/s)")

H_EYR <- read_csv("IMPORT/01__COVARIABLES__/DEBIT/H_Eyrieux.csv") %>% dplyr::select("Date (TU)", "Valeur (en m)") %>% rename(DATE = "Date (TU)", H_instant = "Valeur (en m)")

ggplot() + 
  geom_line(data = H_GLU, aes(x = DATE, y = H_instant)) + 
  geom_point(data = H_GLU, aes(x = DATE, y = H_instant))

ggplot() + 
  geom_line(data = H_EYR, aes(x = DATE, y = H_instant)) + 
  geom_point(data = H_EYR, aes(x = DATE, y = H_instant))
# c'est good

ggplot() + 
  geom_line(data = H_GLU, aes(x = DATE, y = H_instant), color = "red") + 
  geom_line(data = Q_GLU, aes(x = DATE, y = Q_instant))

# regarder la corrélation entre débit et hauteur d'eau avec des moyennes d'heure ####

H_horaire <- H_GLU %>%
  mutate(DATE_HEURE = floor_date(DATE, unit = "hour")) %>%
  group_by(DATE_HEURE) %>%
  summarise(H_moy = mean(H_instant, na.rm = TRUE), .groups = "drop")

Q_horaire <- Q_GLU %>%
  mutate(DATE_HEURE = floor_date(DATE, unit = "hour")) %>%
  group_by(DATE_HEURE) %>%
  summarise(Q_moy = mean(Q_instant, na.rm = TRUE), .groups = "drop")

data_HQ_horaire <- inner_join(H_horaire, Q_horaire, by = "DATE_HEURE")

shapiro.test(data_HQ_horaire$H_moy)
shapiro.test(data_HQ_horaire$Q_moy)
# pas normal

cor.test(data_HQ_horaire$H_moy, data_HQ_horaire$Q_moy, method = "spearman") # non paramétrique
plot(data_HQ_horaire$H_moy, data_HQ_horaire$Q_moy)

# séparation de la date et de l'heure ####

# Gluèyre
H_GLU <- H_GLU %>%
  mutate(DATETIME = DATE,
    DATE = as.Date(DATETIME),
    HEURE = format(DATETIME, "%H:%M:%S")) %>% select(-DATETIME)

# Eyrieux
H_EYR <- H_EYR %>%
  mutate(DATETIME = DATE,
         DATE = as.Date(DATETIME),
         HEURE = format(DATETIME, "%H:%M:%S")) %>% select(-DATETIME)

# PAS DE TEMPS ####
# Regarder les pas de temps entre les mesures pour la Gluèyre et aussi combien de mesures pas jour en moyenne

# calcul des minutes
H_GLU <- H_GLU %>%
  arrange(DATE, HEURE) %>%
  mutate(.tmp_datetime = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), diff_temps = difftime(.tmp_datetime, lag(.tmp_datetime), units = "mins")) %>%
  select(-.tmp_datetime)

# Nombre de mesures par jour
mesures_par_jour_GLU <- H_GLU %>%
  group_by(DATE) %>%
  summarise(n_mesures = n())

summary(mesures_par_jour_GLU$n_mesures)
# 1 min, 21 max et 4.912 en moyenne

# Distribution des pas de temps
summary(as.numeric(H_GLU$diff_temps))

# calcul des minutes
H_EYR <- H_EYR %>%
  arrange(DATE, HEURE) %>%
  mutate(.tmp_datetime = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), diff_temps = difftime(.tmp_datetime, lag(.tmp_datetime), units = "mins")) %>%
  select(-.tmp_datetime)

# Nombre de mesures par jour
mesures_par_jour_EYR <- H_EYR %>%
  group_by(DATE) %>%
  summarise(n_mesures = n())

summary(mesures_par_jour_EYR$n_mesures)
# 1 min, 21 max et 4.912 en moyenne

# Distribution des pas de temps
summary(as.numeric(H_EYR$diff_temps))

# DEBIT COUPLÉ #### 

# --- Préparation des datetime pour H_GLU et H_EYR ---
H_GLU <- H_GLU %>%
  mutate(DATETIME = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  arrange(DATETIME) %>%
  distinct(DATETIME, .keep_all = TRUE)

H_EYR <- H_EYR %>%
  mutate(DATETIME = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  arrange(DATETIME) %>%
  distinct(DATETIME, .keep_all = TRUE)

# --- Datetime des observations pour chaque période ---
mes_obs <- data %>%
  mutate(
    DATETIME_P1 = as.POSIXct(paste(DATE_P1, HEURE_P1), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    DATETIME_P2 = as.POSIXct(paste(DATE_P2, HEURE_P2), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    DATETIME_P3 = as.POSIXct(paste(DATE_P3, HEURE_P3), format = "%Y-%m-%d %H:%M", tz = "UTC")
  )

# ============ INTERPOLATION GLUEYRE ============

mes_obs$H_P1 <- NA_real_
mes_obs$H_P2 <- NA_real_
mes_obs$H_P3 <- NA_real_

glu <- mes_obs$RIVIERE == "Glueyre"
eyr <- mes_obs$RIVIERE == "Eyrieux"

mes_obs$H_P1[glu] <- approx(
  x = H_GLU$DATETIME,
  y = H_GLU$H_instant,
  xout = mes_obs$DATETIME_P1[glu],
  rule = 2
)$y

mes_obs$H_P1[eyr] <- approx(
  x = H_EYR$DATETIME,
  y = H_EYR$H_instant,
  xout = mes_obs$DATETIME_P1[eyr],
  rule = 2
)$y

mes_obs$H_P2[glu] <- approx(H_GLU$DATETIME, H_GLU$H_instant,
                            xout = mes_obs$DATETIME_P2[glu],
                            rule = 2)$y

mes_obs$H_P2[eyr] <- approx(H_EYR$DATETIME, H_EYR$H_instant,
                            xout = mes_obs$DATETIME_P2[eyr],
                            rule = 2)$y

mes_obs$H_P3[glu] <- approx(H_GLU$DATETIME, H_GLU$H_instant,
                            xout = mes_obs$DATETIME_P3[glu],
                            rule = 2)$y

mes_obs$H_P3[eyr] <- approx(H_EYR$DATETIME, H_EYR$H_instant,
                            xout = mes_obs$DATETIME_P3[eyr],
                            rule = 2)$y

# --- Nettoyage : suppression uniquement des colonnes temporaires DATETIME ---

data <- mes_obs %>%
  select(-DATETIME_P1, -DATETIME_P2, -DATETIME_P3)

save(data, file = "SAUVEGARDES/data_Q.RData")
load(file = "SAUVEGARDES/data_Q.RData") # .. Sauvegarde intermédiaire N°5 .. ####

data %>% filter(RIVIERE == "Glueyre") %>% summary()
H_GLU %>% filter(DATE >= as.Date("2026-04-09") & DATE <= as.Date("2026-04-27")) %>% ggplot() + geom_line(aes(x = DATE, y = H_instant))

ggplot() + geom_line(data = data, aes(x = DATE_P1, y = H_P1))
summary(data$H_P1)

################################################################################
############################ DONNEES METEO FRANCE ##############################
################################################################################

METEO_FRANCE <- read_delim("IMPORT/01__COVARIABLES__/METEO_FRANCE/H_07_latest-2025-2026.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% dplyr::select(NUM_POSTE, NOM_USUEL, LAT, LON, ALTI, AAAAMMJJHH, RR1, FF, T, U) %>% dplyr::filter(NUM_POSTE %in% c("07064001", "07096001", "07286002"))

# 07096001 Gluiras 
# 07064001 Cheylard
# 07286002 Saint-Pierreville

# convertir en texte
d <- as.character(METEO_FRANCE$AAAAMMJJHH)
METEO_FRANCE$DATE <- as.Date(
  substr(d, 1, 8),
  format = "%Y%m%d")

METEO_FRANCE$HEURE <- paste0(
  substr(d, 9, 10),
  ":00")

# prendre que les dates de terrain 

METEO_FRANCE <- METEO_FRANCE %>% filter(DATE >= as.Date("2026-04-01") & DATE <= as.Date("2026-06-30"))

# RR1 : quantité de précipitation tombée en 1 heure (mm)
# FF : force du vent moyenné sur 10 mn, mesurée à 10 m (m/s)
# T : température de l'air sous abris (°C)
# U : humidité relative de l'air (%)

GLU <- METEO_FRANCE %>% filter(NUM_POSTE == "07096001")
CHE <- METEO_FRANCE %>% filter(NUM_POSTE == "07064001")
SPV <- METEO_FRANCE %>% filter(NUM_POSTE == "07286002")


summary(GLU$U)
summary(GLU$FF)

summary(CHE$RR1)
summary(CHE$T)

summary(SPV$RR1)
summary(SPV$T)

# graphiques ####
ggplot(SPV, aes(x = DATE, y = T)) +
  geom_smooth(color = "blue") +
  theme_minimal()

ggplot(CHE, aes(x = DATE, y = U)) +
  geom_smooth(color = "blue") +
  theme_minimal()

ggplot(CHE, aes(x = DATE, y = FF)) +
  geom_smooth(color = "blue") +
  theme_minimal()

ggplot(SPV, aes(x = DATE, y = RR1)) +
  geom_smooth(color = "blue") +
  theme_minimal()

b <- ggplot() +
  geom_smooth(data = GLU, aes(x = DATE, y = RR1), color = "blue") +
  geom_smooth(data = SPV, aes(x = DATE, y = RR1), color = "pink") +
  geom_smooth(data = CHE, aes(x = DATE, y = RR1), color = "darkgreen") +
  labs(title = "PLUVIOMÉTRIE (MM)", 
       y = "Pluviométrie (mm)") + 
  theme_minimal()

a <- ggplot() +
  geom_smooth(data = CHE, aes(x = DATE, y = T), color = "darkgreen") + 
  geom_smooth(data = GLU, aes(x = DATE, y = T), color = "blue") +
  geom_smooth(data = SPV, aes(x = DATE, y = T), color = "pink") +
  labs(title = "TEMPÉRATURE DE L'AIR (°C)", 
       y = "Température (°C)") + 
  theme_minimal()

a / b

ggplot() + 
  geom_boxplot(data = data, aes(x = RIVE, y = EFF_MAX))

data %>% filter(RIVE == "G", RIVIERE == "Eyrieux") %>% summarise(n())
data %>% filter(RIVE == "D", RIVIERE == "Eyrieux") %>% summarise(n())

# Résumé ####

# La station de Gluiras (07096001) contient les variables, vent (FF), température de l'air (T), pluviométrie (RR1) et humidité relative de l'air. 

# En revanche la station du Cheylard (07064001) ne contient que les variables de température de l'air (T) et pluviométrie (RR1)

# La station de Saint-Pierreville (07286002) ne contient aussi que les variables de température de l'air (T) et pluviométrie (RR1)

# La station de Beauvène (07030001) ne compte que la variable précipitation (RR1)

######### ___  Humidité relative et force du vent pour l'Eyrieux et Gluèyre  ___ ###########
# données de GLU
# données horaires donc associer la valeur correspondantes à la date et l'heure des observations de data

data <- data %>%
  mutate(
    DATETIME_P1 = as.POSIXct(paste(DATE_P1, HEURE_P1), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    DATETIME_P2 = as.POSIXct(paste(DATE_P2, HEURE_P2), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    DATETIME_P3 = as.POSIXct(paste(DATE_P3, HEURE_P3), format = "%Y-%m-%d %H:%M", tz = "UTC")
  )


GLU <- GLU %>%
  mutate(DATETIME = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  arrange(DATETIME)

GLU <- GLU %>% arrange(DATETIME)

interp_var <- function(target_datetime, station_data, var) {
  approx(
    x = as.numeric(station_data$DATETIME),
    y = station_data[[var]],
    xout = as.numeric(target_datetime),
    rule = 2  # extrapole avec la valeur la plus proche si hors plage, évite les NA en bord
  )$y
}

data <- data %>%
  mutate(
    U_P1  = sapply(DATETIME_P1, interp_var, station_data = GLU, var = "U"),
    FF_P1 = sapply(DATETIME_P1, interp_var, station_data = GLU, var = "FF"),
    U_P2  = sapply(DATETIME_P2, interp_var, station_data = GLU, var = "U"),
    FF_P2 = sapply(DATETIME_P2, interp_var, station_data = GLU, var = "FF"),
    U_P3  = sapply(DATETIME_P3, interp_var, station_data = GLU, var = "U"),
    FF_P3 = sapply(DATETIME_P3, interp_var, station_data = GLU, var = "FF"))

# Vérification
sum(is.na(data$FF_P1)); sum(is.na(data$FF_P2)); sum(is.na(data$FF_P3))

ggplot() + 
  geom_line(data= GLU, aes(x = DATE, y = U)) + 
  geom_point(data= data, aes(x = DATE_P1, y = U_P1), color = "red") + 
  geom_point(data= data, aes(x = DATE_P2, y = U_P2), color = "red") + 
  geom_point(data= data, aes(x = DATE_P3, y = U_P3), color = "red")
  
ggplot() + 
  geom_line(data= GLU, aes(x = DATE, y = FF)) + 
  geom_point(data= data, aes(x = DATE_P1, y = FF_P1), color = "red") + 
  geom_point(data= data, aes(x = DATE_P2, y = FF_P2), color = "red") + 
  geom_point(data= data, aes(x = DATE_P3, y = FF_P3), color = "red") 

# Température de l'air ####
------------------------------ #### GLUEYRE #### -------------------------------

SPV <- SPV %>%
  mutate(DATETIME = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  arrange(DATETIME)

data_glu <- data %>% filter(RIVIERE == "Glueyre")

# données de SPV
# à fréquence horaire

data_glu <- data_glu %>%
  mutate(
    T_P1 = sapply(DATETIME_P1, interp_var, station_data = SPV, var = "T"),
    T_P2 = sapply(DATETIME_P2, interp_var, station_data = SPV, var = "T"),
    T_P3 = sapply(DATETIME_P3, interp_var, station_data = SPV, var = "T"))


------------------------------- #### EYRIEUX #### ------------------------------

CHE <- CHE %>%
  mutate(DATETIME = as.POSIXct(paste(DATE, HEURE), format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  arrange(DATETIME)

data_eyr <- data %>% filter(RIVIERE == "Eyrieux")

# données de CHE
data_eyr <- data_eyr %>%
  mutate(
    T_P1 = sapply(DATETIME_P1, interp_var, station_data = CHE, var = "T"),
    T_P2 = sapply(DATETIME_P2, interp_var, station_data = CHE, var = "T"),
    T_P3 = sapply(DATETIME_P3, interp_var, station_data = CHE, var = "T"))

# rassemblement des colonnes
data <- bind_rows(data_glu, data_eyr)

# Vérif
ggplot() + 
  geom_line(data= SPV, aes(x = DATE, y = T)) + 
  geom_point(data= data, aes(x = DATE_P1, y = T_P1), color = "red") + 
  geom_point(data= data, aes(x = DATE_P2, y = T_P2), color = "red") + 
  geom_point(data= data, aes(x = DATE_P3, y = T_P3), color = "red")

ggplot() + 
  geom_line(data= CHE, aes(x = DATE, y = T)) + 
  geom_point(data= data, aes(x = DATE_P1, y = T_P1), color = "red") + 
  geom_point(data= data, aes(x = DATE_P2, y = T_P2), color = "red") + 
  geom_point(data= data, aes(x = DATE_P3, y = T_P3), color = "red")

data %>% st_drop_geometry() %>% group_by(RIVIERE) %>% 
  summarise(T_P1_moy = mean(T_P1, na.rm = TRUE), n = n())

save(data, file = "SAUVEGARDES/data_METEO.RData")
load(file = "SAUVEGARDES/data_METEO.RData") # .. Sauvegarde intermédiaire N°6 .. ####

#################### ___  Pluviométrie de la Gluèyre ___ #####################
# données de SPV

SPV <- as.data.table(SPV)
setkey(SPV, DATETIME)
CHE <- as.data.table(CHE)
setkey(CHE, DATETIME)

# Fonction de cumul de pluie sur les 24h précédant chaque DATETIME cible
cumul_pluie_24h <- function(datetimes, station_dt) {
  sapply(datetimes, DH100function(t) {
    if (is.na(t)) return(NA_real_)
    sum(station_dt[DATETIME >= (t - 24*3600) & DATETIME <= t, RR1], na.rm = TRUE)
  })
}

# ---- Glueyre <- SPV ----
data_glu[, `:=`(
  RR1_P1 = cumul_pluie_24h(DATETIME_P1, SPV),
  RR1_P2 = cumul_pluie_24h(DATETIME_P2, SPV),
  RR1_P3 = cumul_pluie_24h(DATETIME_P3, SPV))]

# ---- Eyrieux <- CHE ----
data_eyr[, `:=`(
  RR1_P1 = cumul_pluie_24h(DATETIME_P1, CHE),
  RR1_P2 = cumul_pluie_24h(DATETIME_P2, CHE),
  RR1_P3 = cumul_pluie_24h(DATETIME_P3, CHE))]

# Recombiner dans l'ordre d'origine
data <- rbindlist(list(data_glu, data_eyr))
setorder(data, ROW_ID)
data[, ROW_ID := NULL]

# Reconstituer l'objet sf
data <- st_sf(data, geometry = geom_data)

summary(data$RR1_P1)
summary(data$RR1_P2)
summary(data$RR1_P3)

# Variance et nombre de valeurs non-nulles
sd(data$RR1_P1, na.rm = TRUE)
sum(data$RR1_P1 > 0, na.rm = TRUE)
sum(data$RR1_P2 > 0, na.rm = TRUE)
sum(data$RR1_P3 > 0, na.rm = TRUE)

hist(data$RR1_P1)

data <- data %>% select(-DATETIME_P1, -DATETIME_P2,-DATETIME_P3, -RR1_24h_P1, -RR1_24h_P2,-RR1_24h_P3)

# SAUVEGARDE finale 
save(data, file = "SAUVEGARDES/data_VF.RData")
load(file = "SAUVEGARDES/data_VF.RData") # .. Sauvegarde finale N°7 .. ####

data %>% select(ID_SITE,DATETIME_P1, DATETIME_P2,DATETIME_P3, RR1_P1, RR1_P2, RR1_P3) %>% vie
