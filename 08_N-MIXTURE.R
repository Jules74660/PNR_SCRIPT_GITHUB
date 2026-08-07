# ce script a pour objectif de faire de rassembler les covariables dans l'objectif de faire tourner dans le script prochain les modèles d'estimation de l'abondance relative ainsi que de l'occupation


################################################################################
########################## Préparation des données #############################
################################################################################


# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)

pacman::p_load(readr, ggplot2, tidyverse, dplyr, unmarked, AICcmodavg, car, corrplot)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

load("SAUVEGARDES/data_VF.RData")

data <- data %>% select(ID_SITE, RIVIERE, SONN_P1, SONN_P2, SONN_P3, ALTI, MAR_MAX, MAR50, OUVRAGE, F100,F500, B100, B500, DH100, DH500, DR100, DR500, RIVE, OBS_P1, OBS_P2, OBS_P3, DATE_P1, DATE_P2, DATE_P3, HEURE_P1, HEURE_P2, HEURE_P3, NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3, FF_P1,FF_P2, FF_P3, U_P1,U_P2, U_P3, T_P1,T_P2, T_P3, H_P1, H_P2, H_P3, RR1_P1, RR1_P2, RR1_P3)

table(dataglu$OBS_P1)
table(dataglu$OBS_P2)
table(dataglu$OBS_P3)
# Ou toutes les occasions combinées
table(unlist(GLU_obs_covs$OBS))

data <- data %>%
  mutate(
    OBS_P1 = case_when(
      OBS_P1 == "À" ~ "A",
      OBS_P1 %in% c("1136", "1525") ~ "J",
      is.na(OBS_P1) ~ "J",
      TRUE ~ OBS_P1
    ),
    OBS_P2 = case_when(
      OBS_P2 == "À" ~ "A",
      OBS_P2 %in% c("1136", "1525") ~ "J",
      is.na(OBS_P2) ~ "A",
      TRUE ~ OBS_P2
    ),
    OBS_P3 = case_when(
      OBS_P3 == "À" ~ "A",
      OBS_P3 %in% c("1136", "1525") ~ "J",
      is.na(OBS_P3) ~ "A",
      TRUE ~ OBS_P3
    )
  )

################################################################################
######################## Standardisation des covs  #############################
################################################################################

# Le jour julien correspond au jour de l'année (1 à 365/366)
data$DATE_P1 <- as.numeric(format(data$DATE_P1, "%j"))
data$DATE_P2 <- as.numeric(format(data$DATE_P2, "%j"))
data$DATE_P3 <- as.numeric(format(data$DATE_P3, "%j"))
summary(data$DATE_P1)

data <- data %>%
  mutate(HEURE_P1 = lubridate::hour(hm(HEURE_P1)) + lubridate::minute(lubridate::hm(HEURE_P1)) / 60,
    HEURE_P2 = lubridate::hour(hm(HEURE_P2)) + lubridate::minute(lubridate::hm(HEURE_P2)) / 60,
    HEURE_P3 = lubridate::hour(hm(HEURE_P3)) + lubridate::minute(lubridate::hm(HEURE_P3)) / 60)


variables_site <- c("ALTI", "MAR_MAX", "MAR50", "OUVRAGE", "F100", "F500", 
                    "B100", "B500", "DH100", "DH500", "DR100", "DR500")

# Pour chaque variable, proportion à la valeur minimale (souvent = 0 avant standardisation)
for (v in variables_site) {
  valeur_min <- min(data[[v]], na.rm = TRUE)
  prop_min <- mean(data[[v]] == valeur_min, na.rm = TRUE)
  cat(v, ":", round(prop_min * 100, 1), "% des sites à la valeur minimale\n")
}

# on écarte batiment car trop de zéros
# on écarte DH100 car trop de zéros en on garde DH500

standardiser <- function(x) {
  z <- as.numeric(scale(x))
  z[is.na(z)] <- 0
  z}

vars_a_standardiser <- c("ALTI", "OUVRAGE", "F100", "F500", "DH500", "DR100", "DR500")
data <- data %>% select(-c(B100, B500, DH100))
data <- data %>% 
  st_drop_geometry() %>%
  mutate(across(all_of(vars_a_standardiser), standardiser))

sapply(data[vars_a_standardiser], mean, na.rm = TRUE)
sapply(data[vars_a_standardiser], sd, na.rm = TRUE)

# COVARIABLES DE SITE 

# ALTI : Altitude
# MAR_MAX : Nombre max de mares (habitat potentiel)
# MAR50 : Nombre de mares dans un rayon de 50m
# OUVRAGE : Distance à l'ouvrage le plus proche
# F100, F500 : Surface de forêt (100m/500m)
# B100, B500 : Surface urbanisée/bâti (100m/500m)
# DH100, DH500 : Densité hydrographique (100m/500m)
# DR100, DR500 : Densité routière (100m/500m)
# RIVE : Rive gauche/droite

# COVARIABLES D'OBSERVATION

# OBS_P1/P2/P3 : Observateur → effet potentiel sur la détection
# DATE_P1/P2/P3 : Date du passage → peut affecter la détection (phénologie, météo)
# HEURE_P1/P2/P3 : Heure du passage → idem 
# NBR_MAR_P1/P2/P3 : Nombre de mares en eau 
# T_P1/P2/P3  : Température de l'air
# U_P1/P2/P3  : Humidité relative
# FF_P1/P2/P3  : Force du vent 
# FF_P1/P2/P3  : Hauteur d'eau
# RR1_P1/P2/P3  : Cumul des précipitations des 24 heures précédents 

################################################################################
############################### Choix du modèle ################################
################################################################################

sum(rowSums(y_glu) == 0)
nrow(y_glu)

sum(rowSums(y_eyr) == 0)
nrow(y_eyr)

table(y_glu)
mean(y_glu, na.rm = TRUE)
var(y_glu, na.rm = TRUE)
# surdispersion des données 
# surdispersion 

mod_P   <- pcount(~1 ~1, data = umf_glu, mixture = "P")
mod_NB  <- pcount(~1 ~1, data = umf_glu, mixture = "NB")
mod_ZIP <- pcount(~1 ~1, data = umf_glu, mixture = "ZIP")

aictab(list(P = mod_P, NB = mod_NB, ZIP = mod_ZIP))

gof_NB <- Nmix.gof.test(mod_NB, nsim = 1000)
gof_NB

?Nmix.gof.test

# test avec toutes les covariables (non corrélés basé sur les tests effectués avant)

mod_NB_full_glu <- pcount(
  ~ OBS + HEURE + DATE + RR1 + FF + NBR_MAR
  ~ MAR_MAX + F100 + B100 + DH100 + OUVRAGE, 
  data = umf_glu, mixture = "NB")

mod_NB_full_glu@opt$convergence
summary(mod_NB_full_glu)

aictab(list(NBfull = mod_NB_full_glu, NB = mod_NB))

# eyrieux 

mod_P_eyr   <- pcount(~1 ~1, data = umf_eyr, mixture = "P")
mod_NB_eyr  <- pcount(~1 ~1, data = umf_eyr, mixture = "NB")
mod_ZIP_eyr <- pcount(~1 ~1, data = umf_eyr, mixture = "ZIP")

aictab(list(P = mod_P_eyr, NB = mod_NB_eyr, ZIP = mod_ZIP_eyr))

gof_NB <- Nmix.gof.test(mod_NB_eyr, nsim = 1000)
gof_NB

mod_NB_full_eyr <- pcount(
  ~ OBS + DATE + HEURE + U + NBR_MAR
  ~ MAR_MAX + F500 + DR100 + DH100 + OUVRAGE + RIVE, 
  data = umf_eyr, mixture = "NB")

gof_NB_eyr <- Nmix.gof.test(mod_NB_full_eyr, nsim = 1000)
gof_NB_eyr

################################################################################
###################### Sélection des variables corrélés ########################
################################################################################

dataglu <- data %>% filter(RIVIERE == "Glueyre")
nrow(dataglu)

id_a_retirer <- dataglu$ID_SITE[509]
id_a_retirer
dataglu <- dataglu %>% filter(ID_SITE != id_a_retirer)
################################ GLUEYRE #########################

GLU_site_covs <- dataglu %>% st_drop_geometry() %>% dplyr::select(ALTI, MAR_MAX, MAR50, OUVRAGE, F500, DH500, DR100, DR500, RIVE)

GLU_obs_covs <- list(
  OBS     = dataglu %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataglu %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataglu %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataglu %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  TEMP    = dataglu %>% st_drop_geometry() %>% dplyr::select(T_P1, T_P2, T_P3),
  U       = dataglu %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3),
  FF      = dataglu %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataglu %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3), 
  H_EAU   = dataglu %>% st_drop_geometry() %>% dplyr::select(H_P1, H_P2, H_P3))

y_glu <- dataglu %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()
umf_glu <- unmarkedFramePCount(y = y_glu, siteCovs = GLU_site_covs, obsCovs = GLU_obs_covs)

# Covariables de site
cor_site <- GLU_site_covs %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_site, method = "number", type = "upper", tl.cex = 0.7)
# id les paires avec le > 0.4
which(abs(cor_site) > 0.4 & abs(cor_site) < 1, arr.ind = TRUE)
round(cor_site, 2)

vif(GLU_site_covs %>% select(where(is.numeric)))

# pas corrélés mais test 
mod_DR100 <- pcount(~1 ~ DR100, data = umf_glu, mixture = "ZIP")
mod_DR500 <- pcount(~1 ~ DR500, data = umf_glu, mixture = "ZIP")
aictab(list(mod_DR100, mod_DR500), modnames = c("DR100", "DR500"))
# je garde DR500 avec le delta de 6.07
# j'enlève de l'analyse DR100, DH500, B500, F500 

GLU_site_covs <- GLU_site_covs %>% select(-c(DR500))
vif(GLU_site_covs %>% select(where(is.numeric)))
cor_site <- GLU_site_covs %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_site, method = "number", type = "upper", tl.cex = 0.7)
round(cor_site, 2)

# Covariables d'observation
obs_long <- dataglu %>%
  st_drop_geometry() %>%
  select(ID_SITE, 
         TEMP_P1 = T_P1, TEMP_P2 = T_P2, TEMP_P3 = T_P3,
         RR1_P1, RR1_P2, RR1_P3,
         FF_P1, FF_P2, FF_P3,
         U_P1, U_P2, U_P3,
         HEURE_P1, HEURE_P2, HEURE_P3,
         DATE_P1, DATE_P2, DATE_P3,
         NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3,
         H_P1, H_P2, H_P3) %>%
  pivot_longer(-ID_SITE, 
               names_to = c(".value", "PASSAGE"), 
               names_pattern = "(.*)_P(\\d)") 

cor_obs <- obs_long %>% select(-ID_SITE, -PASSAGE) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_obs, method = "number", type = "upper", tl.cex = 0.8)
# id
which(abs(cor_obs) > 0.4 & abs(cor_obs) < 1, arr.ind = TRUE)
dim(cor_obs)
colnames(cor_obs)
diag(cor_obs)
round(cor_obs, 2)

GLU_obs_covs <- list(OBS     = dataglu %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataglu %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataglu %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataglu %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  TEMP    = dataglu %>% st_drop_geometry() %>% dplyr::select(T_P1, T_P2, T_P3),
  U       = dataglu %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3),
  FF      = dataglu %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataglu %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

GLU_obs_covs_2 <- data.frame(
  HEURE = unlist(GLU_obs_covs$HEURE),
  DATE  = unlist(GLU_obs_covs$DATE),
  TEMP  = unlist(GLU_obs_covs$TEMP),
  U     = unlist(GLU_obs_covs$U),
  FF    = unlist(GLU_obs_covs$FF),
  RR1   = unlist(GLU_obs_covs$RR1),
  NBR_MAR = unlist(GLU_obs_covs$NBR_MAR))
vif(GLU_obs_covs_2 %>% select(where(is.numeric)))

# H_EAU écarté en raison du VIF élevé
# REFAIRE DES LISTES
GLU_obs_covs <- list(
  OBS     = dataglu %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataglu %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataglu %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataglu %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  TEMP    = dataglu %>% st_drop_geometry() %>% dplyr::select(T_P1, T_P2, T_P3),
  U       = dataglu %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3),
  FF      = dataglu %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataglu %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

# final
summary(GLU_obs_covs)
summary(GLU_site_covs)

y_glu <- dataglu %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()

umf_glu <- unmarkedFramePCount(y = y_glu, siteCovs = GLU_site_covs, obsCovs = GLU_obs_covs)
summary(umf_glu)
str(umf_glu)

################################ Eyrieux ##########################

dataeyr <- data %>% filter(RIVIERE == "Eyrieux")

EYR_site_covs <- dataeyr %>% st_drop_geometry() %>% dplyr::select(ALTI, MAR_MAX, MAR50, OUVRAGE, F500, DH500, DR100, DR500, RIVE)

EYR_obs_covs <- list(
  OBS     = dataeyr %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataeyr %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataeyr %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataeyr %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  U       = dataeyr %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3),
  FF      = dataeyr %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataeyr %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

y_eyr <- dataeyr %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()
umf_eyr <- unmarkedFramePCount(y = y_eyr, siteCovs = EYR_site_covs, obsCovs = EYR_obs_covs)

# Covariables de site
cor_site <- EYR_site_covs %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_site, method = "number", type = "upper", tl.cex = 0.7)
# id les paires avec le > 0.4
which(abs(cor_site) > 0.4 & abs(cor_site) < 1, arr.ind = TRUE)
round(cor_site, 2)

# pas corrélés mais test 
mod_DR100 <- pcount(~1 ~ DR100, data = umf_eyr, mixture = "ZIP", K = 150)
mod_DR500 <- pcount(~1 ~ DR500, data = umf_eyr, mixture = "ZIP", K = 150)
aictab(list(mod_DR100, mod_DR500), modnames = c("DR100", "DR500"))
densityPlot(EYR_site_covs$DR100)
densityPlot(EYR_site_covs$DR500)
# je garde DR100

vif(EYR_site_covs %>% select(where(is.numeric)))
EYR_site_covs <- EYR_site_covs %>% select(-c(DR100))
vif(EYR_site_covs %>% select(where(is.numeric)))

# Covariables d'observation
obs_long <- dataeyr %>%
  st_drop_geometry() %>%
  select(ID_SITE, 
         TEMP_P1 = T_P1, TEMP_P2 = T_P2, TEMP_P3 = T_P3,
         RR1_P1, RR1_P2, RR1_P3,
         FF_P1, FF_P2, FF_P3,
         U_P1, U_P2, U_P3,
         HEURE_P1, HEURE_P2, HEURE_P3,
         DATE_P1, DATE_P2, DATE_P3,
         NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3) %>%
  pivot_longer(-ID_SITE, 
               names_to = c(".value", "PASSAGE"), 
               names_pattern = "(.*)_P(\\d)") 

cor_obs <- obs_long %>% select(-ID_SITE, -PASSAGE) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_obs, method = "number", type = "upper", tl.cex = 0.8)
# id
which(abs(cor_obs) > 0.4 & abs(cor_obs) < 1, arr.ind = TRUE)
round(cor_obs, 2)

#EYR_obs_covs2 <- data.frame(
#  HEURE = unlist(EYR_obs_covs$HEURE),
#  DATE  = unlist(EYR_obs_covs$DATE),
#  TEMP  = unlist(EYR_obs_covs$TEMP),
#  U     = unlist(EYR_obs_covs$U),
#  FF    = unlist(EYR_obs_covs$FF),
#  RR1   = unlist(EYR_obs_covs$RR1),
#  NBR_MAR = unlist(EYR_obs_covs$NBR_MAR))

# REFAIRE DES LISTES
EYR_obs_covs <- list(
  OBS     = dataeyr %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataeyr %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataeyr %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataeyr %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  U       = dataeyr %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3),
  FF      = dataeyr %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataeyr %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

EYR_obs_covs
EYR_site_covs

################################################################################
####################### Création des modèles N-Mixture #########################
################################################################################

summary(GLU_obs_covs) # OBS, DATE, HEURE, NBR_MAR, TEMP, U, FF, RR1
summary(GLU_site_covs) # ALTI, MAR_MAX, MAR50, OUVRAGE, F500, DH500, DR100, RIVE
summary(EYR_obs_covs) # OBS, DATE, HEURE, NBR_MAR, U, FF, RR1
summary(EYR_site_covs) # ALTI, MAR_MAX, MAR50, OUVRAGE, F500, DH500, DR500, RIVE

y_glu <- dataglu %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()

umf_glu <- unmarkedFramePCount(y = y_glu, siteCovs = GLU_site_covs, obsCovs = GLU_obs_covs)
summary(umf_glu)
str(umf_glu)

y_eyr <- dataeyr %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()

umf_eyr <- unmarkedFramePCount(y = y_eyr, siteCovs = EYR_site_covs, obsCovs = EYR_obs_covs)
summary(umf_eyr)
str(umf_eyr)

################################################################################
########################## Intégration des covs  ###############################
################################################################################

################################ Gluèyre ##########################
# Détection 
mod_det_null   <- pcount(~1 ~1, data = umf_glu, mixture = "ZIP")
mod_det_obs    <- pcount(~OBS ~1, data = umf_glu, mixture = "ZIP")
mod_det_heure  <- pcount(~HEURE ~1, data = umf_glu, mixture = "ZIP")
mod_det_date   <- pcount(~DATE ~1, data = umf_glu, mixture = "ZIP")
mod_det_temp   <- pcount(~TEMP ~1, data = umf_glu, mixture = "ZIP")
mod_det_u      <- pcount(~U ~1, data = umf_glu, mixture = "ZIP")
mod_det_ff     <- pcount(~FF ~1, data = umf_glu, mixture = "ZIP")
mod_det_rr1    <- pcount(~RR1 ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar <- pcount(~NBR_MAR ~1, data = umf_glu, mixture = "ZIP")
mod_det_full   <- pcount(~OBS + HEURE + DATE + TEMP + U + FF + RR1 + NBR_MAR ~1, 
                         data = umf_glu, mixture = "ZIP")

aictab(list(mod_det_null, mod_det_obs, mod_det_heure, mod_det_date, mod_det_temp,
            mod_det_u, mod_det_ff, mod_det_rr1, mod_det_nbrmar, mod_det_full),
       modnames = c("null","OBS","HEURE","DATE","TEMP","U","FF","RR1","NBR_MAR","full"))
# meilleur c'est NBR_MAR
# HEURE

mod_det_nbrmar_heure <- pcount(~NBR_MAR + HEURE ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar_obs   <- pcount(~NBR_MAR + OBS ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar_U   <- pcount(~NBR_MAR + U ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar_DATE   <- pcount(~NBR_MAR + DATE ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar_RR1   <- pcount(~NBR_MAR + RR1 ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar_TEMP   <- pcount(~NBR_MAR + TEMP ~1, data = umf_glu, mixture = "ZIP")
mod_det_nbrmar_FF   <- pcount(~NBR_MAR + FF ~1, data = umf_glu, mixture = "ZIP")

DETECT_GLU <- aictab(list(mod_det_null, mod_det_obs, mod_det_heure, mod_det_date, mod_det_temp, mod_det_u, mod_det_ff, mod_det_rr1, mod_det_nbrmar, mod_det_full,mod_det_nbrmar_heure, mod_det_nbrmar_obs, mod_det_nbrmar_U, mod_det_nbrmar_DATE,mod_det_nbrmar_RR1, mod_det_nbrmar_TEMP,mod_det_nbrmar_FF ),modnames = c("null","OBS","HEURE","DATE","TEMP","U","FF","RR1","NBR_MAR","full","NBR_MAR+HEURE","NBR_MAR+OBS","NBR_MAR+U", "NBR_MAR+DATE","NBR_MAR+RR1","NBR_MAR+TEMP","NBR_MAR+FF"))

# on retient NBR_MAR et humidité relative (U)
pred_det_U <- predict(mod_det_nbrmar_U, type = "det")
pred_det_HEURE <- predict(mod_det_nbrmar_heure, type = "det")
pred_det_FULL <- predict(mod_det_full, type = "det")
summary(pred_det_U$Predicted)
summary(pred_det_HEURE$Predicted)
summary(pred_det_FULL$Predicted)
# test ajustement gof 
Nmix.gof.test(mod_det_nbrmar_U, nsim = 1000)

# ALTI, MAR_MAX, MAR50, OUVRAGE, F500, DH500, DR100, RIVE
# ABONDANCE 
mod_abond_null    <- pcount(~NBR_MAR + U ~1, data = umf_glu, mixture = "ZIP")
mod_abond_marmax  <- pcount(~NBR_MAR + U ~MAR_MAX, data = umf_glu, mixture = "ZIP")
mod_abond_mar50   <- pcount(~NBR_MAR + U ~MAR50, data = umf_glu, mixture = "ZIP")
mod_abond_ouvrage <- pcount(~NBR_MAR + U ~OUVRAGE, data = umf_glu, mixture = "ZIP")
mod_abond_alti    <- pcount(~NBR_MAR + U ~ALTI, data = umf_glu, mixture = "ZIP")
mod_abond_f500    <- pcount(~NBR_MAR + U ~F500, data = umf_glu, mixture = "ZIP")
mod_abond_dh500   <- pcount(~NBR_MAR + U ~DH500, data = umf_glu, mixture = "ZIP")
mod_abond_dr100   <- pcount(~NBR_MAR + U ~DR100, data = umf_glu, mixture = "ZIP")
mod_abond_rive    <- pcount(~NBR_MAR + U ~RIVE, data = umf_glu, mixture = "ZIP")
mod_abond_full    <- pcount(~NBR_MAR + U ~ALTI + MAR_MAX + MAR50 + OUVRAGE + F500 + DH500 + DR100 + RIVE,data = umf_glu, mixture = "ZIP")

aictab(list(mod_abond_null, mod_abond_marmax, mod_abond_mar50, mod_abond_ouvrage, mod_abond_alti, mod_abond_dh100, mod_abond_dr100, mod_abond_rive, mod_abond_full),
       modnames = c("null","MAR_MAX","MAR50","OUVRAGE","ALTI","DH100","DR100","RIVE","full"))

summary(mod_abond_full)
# MAR-MAX meilleure variable
# exclusion B100 car grosse intervalle
mod1 <- pcount(~NBR_MAR + U ~MAR_MAX + OUVRAGE, data = umf_glu, mixture = "ZIP")
mod2 <- pcount(~NBR_MAR + U ~MAR_MAX + ALTI, data = umf_glu, mixture = "ZIP")
mod3 <- pcount(~NBR_MAR + U ~MAR_MAX + F500, data = umf_glu, mixture = "ZIP")
mod4 <- pcount(~NBR_MAR + U ~MAR_MAX + DH500, data = umf_glu, mixture = "ZIP")
mod5 <- pcount(~NBR_MAR + U ~MAR_MAX + DR100, data = umf_glu, mixture = "ZIP")
mod6 <- pcount(~NBR_MAR + U ~MAR_MAX + RIVE, data = umf_glu, mixture = "ZIP")

mod_50_1 <- pcount(~NBR_MAR + U ~ MAR50 + OUVRAGE, data = umf_glu, mixture = "ZIP")
mod_50_2 <- pcount(~NBR_MAR + U ~MAR50 + ALTI, data = umf_glu, mixture = "ZIP")
mod_50_3 <- pcount(~NBR_MAR + U ~MAR50 + F500, data = umf_glu, mixture = "ZIP")
mod_50_4 <- pcount(~NBR_MAR + U ~MAR50 + DH500, data = umf_glu, mixture = "ZIP")
mod_50_5 <- pcount(~NBR_MAR + U ~MAR50 + DR100, data = umf_glu, mixture = "ZIP")
mod_50_6 <- pcount(~NBR_MAR + U ~MAR50 + RIVE, data = umf_glu, mixture = "ZIP")
# on garde Mar_max dans tous les cas 

aictab(list(mod_abond_null, mod_abond_marmax, mod_abond_mar50, mod_abond_ouvrage, mod_abond_alti, mod_abond_f100, mod_abond_b100, mod_abond_dh100, mod_abond_dr100, mod_abond_rive, mod_abond_full, mod1, mod2, mod3, mod4, mod5, mod6,mod_50_1, mod_50_2, mod_50_3, mod_50_4, mod_50_5, mod_50_6),modnames = c("null","MAR_MAX","MAR50","OUVRAGE","ALTI","F100","B100","DH100","DR100","RIVE","full","MAR_MAX+OUVRAGE","MAR_MAX+ALTI","MAR_MAX+F500","MAR_MAX+DH500","MAR_MAX+DR100","MAR_MAX+RIVE","MAR50+OUVRAGE","MAR50+ALTI","MAR50+F500","MAR50+DH500","MAR50+DR100","MAR50+RIVE"))

# et le meilleur c'est MAR_MAX+DH500 donc mod4

mod4_ALTI <- pcount(~NBR_MAR + HEURE ~ MAR_MAX + DH500 + ALTI,data = umf_glu,mixture="ZIP")

mod4_DR100 <- pcount(~NBR_MAR + HEURE ~ MAR_MAX + DH500 + DR100,data = umf_glu,mixture="ZIP")

mod4_F500 <- pcount(~NBR_MAR + HEURE ~ MAR_MAX + DH500 + F500,data = umf_glu,mixture="ZIP")

mod4_OUVRAGE <- pcount(~NBR_MAR + HEURE ~ MAR_MAX + DH500 + OUVRAGE,data = umf_glu,mixture="ZIP")

mod4_RIVE <- pcount(~NBR_MAR + HEURE ~ MAR_MAX + DH500 + RIVE,data = umf_glu,mixture="ZIP")

ABOND_GLU <- aictab(list(mod_abond_null, mod_abond_marmax, mod_abond_mar50, mod_abond_ouvrage, mod_abond_alti, mod_abond_f100, mod_abond_b100, mod_abond_dh100, mod_abond_dr100, mod_abond_rive, mod_abond_full, mod1, mod2, mod3, mod4, mod5, mod6,mod_50_1, mod_50_2, mod_50_3, mod_50_4, mod_50_5, mod_50_6, mod4_ALTI, mod4_DR100, mod4_F100,mod4_OUVRAGE, mod4_RIVE),modnames = c("null","MAR_MAX","MAR50","OUVRAGE","ALTI","F100","B100","DH100","DR100","RIVE","full","MAR_MAX+OUVRAGE","MAR_MAX+ALTI","MAR_MAX+F500","MAR_MAX+DH500","MAR_MAX+DR100","MAR_MAX+RIVE","MAR50+OUVRAGE","MAR50+ALTI","MAR50+F500","MAR50+DH500","MAR50+DR100","MAR50+RIVE","mod4_ALTI", "mod4_DR100"," mod4_F100","mod4_OUVRAGE", "mod4_RIVE"))

# mod4 est le meilleur et ajouter des covariables ne fait pas baisser l'AIC mais fait augmenter les paramètres

# récup mar_max brut
load("SAUVEGARDES/data_VF.RData")
data_brut <- data %>%
  filter(RIVIERE == "Glueyre")
MAR_MAX_brut <- data_brut$MAR_MAX
summary(MAR_MAX_brut)
# enlever la valeur du NA dans le jeu de données
GLU_site_covs$log_MAR_MAX <- scale(log(MAR_MAX_brut))

umf_glu <- unmarkedFramePCount(
  y = y_glu,
  siteCovs = GLU_site_covs,
  obsCovs = GLU_obs_covs)

# variantes 
mod4
mod4_log_MAR <- pcount(~NBR_MAR + HEURE ~ log_MAR_MAX + DH500,data = umf_glu,mixture = "ZIP")
aictab(list(mod4,mod_log_MAR), modnames = c("mod2normal", "mod_log_MAR"))
summary(mod4_log_MAR)
# log mar est bien meilleur
MOD_final <- mod4_log_MAR
summary(MOD_final)

AIC_GLU <-  aictab(list(mod_abond_null, mod_abond_marmax, mod_abond_mar50, mod_abond_ouvrage, mod_abond_alti, mod_abond_f100, mod_abond_b100, mod_abond_dh100, mod_abond_dr100, mod_abond_rive, mod_abond_full, mod1, mod2, mod3, mod4, mod5, mod6,mod_50_1, mod_50_2, mod_50_3, mod_50_4, mod_50_5, mod_50_6, mod4_ALTI, mod4_DR100, mod4_F100,mod4_OUVRAGE, mod4_RIVE,mod4_log_MAR),modnames = c("null","MAR_MAX","MAR50","OUVRAGE","ALTI","F100","B100","DH100","DR100","RIVE","full","MAR_MAX+OUVRAGE","MAR_MAX+ALTI","MAR_MAX+F500","MAR_MAX+DH500","MAR_MAX+DR100","MAR_MAX+RIVE","MAR50+OUVRAGE","MAR50+ALTI","MAR50+F500","MAR50+DH500","MAR50+DR100","MAR50+RIVE","mod4_ALTI", "mod4_DR100"," mod4_F100","mod4_OUVRAGE", "mod4_RIVE", "mod4_log_MAR"))

write_csv(AIC_GLU, "SAUVEGARDES/AIC_GLU.csv")

# refaire tourner 

# Identifier l'ID_SITE exact du site 509 (position dans le data.frame)
id_a_retirer <- dataglu$ID_SITE[509]
id_a_retirer

# Retirer ce site de dataglu
dataglu <- dataglu %>% filter(ID_SITE != id_a_retirer)

# Reconstruire tous les objets depuis ce dataglu nettoyé
GLU_site_covs <- dataglu %>% st_drop_geometry() %>%
  dplyr::select(ALTI, MAR_MAX, MAR50, OUVRAGE, F100, F500, DH500, DR100, DR500, RIVE)

GLU_obs_covs <- list(
  NBR_MAR = dataglu %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  U       = dataglu %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3)
  # + toutes les autres covariables d'observation que vous gardez
)

y_glu <- dataglu %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()

umf_glu <- unmarkedFramePCount(y = y_glu, siteCovs = GLU_site_covs, obsCovs = GLU_obs_covs)
nrow(umf_glu@y)  # doit être 700 maintenant

################################ Eyrieux ##########################

summary(EYR_obs_covs) # OBS, DATE, HEURE, NBR_MAR, U, FF, RR1

# Détection 
eyr_det_null   <- pcount(~1 ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_obs    <- pcount(~OBS ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_heure  <- pcount(~HEURE ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_date   <- pcount(~DATE ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_u      <- pcount(~U ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_ff     <- pcount(~FF ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_rr1    <- pcount(~RR1 ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_nbrmar <- pcount(~NBR_MAR ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_full   <- pcount(~OBS + HEURE + DATE + U + FF + RR1 + NBR_MAR ~1, 
                         data = umf_eyr, mixture = "ZIP")

aictab(list(eyr_det_null, eyr_det_obs, eyr_det_heure, eyr_det_date,
            eyr_det_u, eyr_det_ff, eyr_det_rr1, eyr_det_nbrmar, eyr_det_full),
       modnames = c("null","OBS","HEURE","DATE","U","FF","RR1","NBR_MAR","full"))

# meilleur c'est NBR_MAR
# choix entre RR1 et U car corrélés : RR1 plus parcimonieux pour l'instant 

eyr_det_nbrmar_heure <- pcount(~NBR_MAR + HEURE ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_nbrmar_obs   <- pcount(~NBR_MAR + OBS ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_nbrmar_U   <- pcount(~NBR_MAR + U ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_nbrmar_DATE   <- pcount(~NBR_MAR + DATE ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_nbrmar_RR1   <- pcount(~NBR_MAR + RR1 ~1, data = umf_eyr, mixture = "ZIP")
eyr_det_nbrmar_FF   <- pcount(~NBR_MAR + FF ~1, data = umf_eyr, mixture = "ZIP")

aictab(list(eyr_det_null, eyr_det_obs, eyr_det_heure, eyr_det_date, eyr_det_u, eyr_det_ff, eyr_det_rr1, eyr_det_nbrmar, eyr_det_full, eyr_det_nbrmar_heure, eyr_det_nbrmar_obs, eyr_det_nbrmar_U, eyr_det_nbrmar_DATE,eyr_det_nbrmar_RR1,eyr_det_nbrmar_FF ),modnames = c("null","OBS","HEURE","DATE","U","FF","RR1","NBR_MAR","full","NBR_MAR+HEURE","NBR_MAR+OBS","NBR_MAR+U", "NBR_MAR+DATE","NBR_MAR+RR1","NBR_MAR+FF")) 
# ici c'est U donc on ne garde que U

summary(eyr_det_full)
# j'enlève OBS sur l'Eyrieux car 1 passage tout seul 
# HEURE et DATE pas beaucoup de support
# U et RR1 pas trop mal 
# il reste NBR_MAR, U et RR1

eyr_RR1  <- pcount(~NBR_MAR + U ~1, data = umf_eyr, mixture = "ZIP")

DETECT_EYR <- aictab(list(eyr_det_null, eyr_det_obs, eyr_det_heure, eyr_det_date, eyr_det_u, eyr_det_ff, eyr_det_rr1, eyr_det_nbrmar, eyr_det_full, eyr_det_nbrmar_heure, eyr_det_nbrmar_obs, eyr_det_nbrmar_U, eyr_det_nbrmar_DATE,eyr_det_nbrmar_RR1,eyr_det_nbrmar_FF),modnames = c("null","OBS","HEURE","DATE","U","FF","RR1","NBR_MAR","full","NBR_MAR+HEURE","NBR_MAR+OBS","NBR_MAR+U", "NBR_MAR+DATE","NBR_MAR+RR1","NBR_MAR+FF"))

dim(getY(umf_eyr))
colSums(is.na(umf_eyr@obsCovs))
length(eyr_det_full@data@y)
length(eyr_OBS@data@y)

# EYR_RR1 est le meilleur : NBR_MAR + U + RR1 

ajusteyr <- Nmix.gof.test(eyr_RR1, nsim = 1000)

# ABONDANCE 
colnames(EYR_site_covs) # ALTI, MAR_MAX, MAR50, OUVRAGE, F500, DH500, DR500, RIVE

eyr_abond_null    <- pcount(~NBR_MAR + U  ~1, data = umf_eyr, mixture = "ZIP")
eyr_abond_marmax  <- pcount(~NBR_MAR + U ~MAR_MAX, data = umf_eyr, mixture = "ZIP")
eyr_abond_mar50   <- pcount(~NBR_MAR + U ~MAR50, data = umf_eyr, mixture = "ZIP")
eyr_abond_ouvrage <- pcount(~NBR_MAR + U ~OUVRAGE, data = umf_eyr, mixture = "ZIP")
eyr_abond_alti    <- pcount(~NBR_MAR + U ~ALTI, data = umf_eyr, mixture = "ZIP")
eyr_abond_f500    <- pcount(~NBR_MAR + U ~F500, data = umf_eyr, mixture = "ZIP")
eyr_abond_dh500   <- pcount(~NBR_MAR + U ~DH500, data = umf_eyr, mixture = "ZIP")
eyr_abond_dr500   <- pcount(~NBR_MAR + U ~DR500, data = umf_eyr, mixture = "ZIP")
eyr_abond_rive    <- pcount(~NBR_MAR + U ~RIVE, data = umf_eyr, mixture = "ZIP")
eyr_abond_full    <- pcount(~NBR_MAR + U ~ALTI + MAR_MAX + MAR50 + OUVRAGE + F500 + DH500 + DR500 + RIVE,data = umf_eyr, mixture = "ZIP")

aictab(list(eyr_abond_null, eyr_abond_marmax, eyr_abond_mar50, eyr_abond_ouvrage, eyr_abond_alti, eyr_abond_f500, eyr_abond_dh500, eyr_abond_dr500, eyr_abond_rive, eyr_abond_full),
       modnames = c("null","MAR_MAX","MAR50","OUVRAGE","ALTI","F500","DH500","DR500", "RIVE","full"))

summary(eyr_abond_full)
#, MAR_MAX, DH500, DR500, OUVRAGE

eyr_abond_marmax
mod1 <- pcount(~NBR_MAR + U  ~MAR_MAX + DH500, data = umf_eyr, mixture = "ZIP")
mod2 <- pcount(~NBR_MAR + U  ~MAR_MAX + DR500, data = umf_eyr, mixture = "ZIP")
mod3 <- pcount(~NBR_MAR + U  ~MAR_MAX + OUVRAGE, data = umf_eyr, mixture = "ZIP")
mod4 <- pcount(~NBR_MAR + U  ~MAR_MAX + ALTI, data = umf_eyr, mixture = "ZIP")
mod5 <- pcount(~NBR_MAR + U  ~MAR_MAX + DR500 + DH500 + ALTI + OUVRAGE, data = umf_eyr, mixture = "ZIP")

aictab(list(eyr_abond_marmax, mod1, mod2, mod3, mod4, mod5),modnames = c("mar_max","DH500","DR500","OUVRAGE", "ALTI", "FULL"))

# modèle 4 le plus parcimonieux après full donc je le garde

load("SAUVEGARDES/data_VF.RData")
data_brut <- data %>%
  filter(RIVIERE == "Eyrieux")
MAR_MAX_brut <- data_brut$MAR_MAX
summary(MAR_MAX_brut)
# enlever la valeur du NA dans le jeu de données
EYR_site_covs$log_MAR_MAX <- scale(log(MAR_MAX_brut))

umf_eyr <- unmarkedFramePCount(
  y = y_eyr,
  siteCovs = EYR_site_covs,
  obsCovs = EYR_obs_covs)

mod4log <- pcount(~NBR_MAR + U  ~log_MAR_MAX + ALTI, data = umf_eyr, mixture = "ZIP")

aictab(list(mod4, mod4log),modnames = c("mar_max","logmarmax"))
# meilleur est toujours mod4 

aictab(list(eyr_abond_null, eyr_abond_marmax, eyr_abond_mar50, eyr_abond_ouvrage, eyr_abond_alti, eyr_abond_f500, eyr_abond_dh500, eyr_abond_dr500, eyr_abond_rive, eyr_abond_full,mod1, mod2, mod3, mod4, mod5),modnames = c("null","MAR_MAX","MAR50","OUVRAGE","ALTI","F500","DH500","DR500", "RIVE","full","DH500","DR500","OUVRAGE", "ALTI", "FULL"))

ajusteyr <- Nmix.gof.test(mod4, nsim = 1000)
summary(mod4)

fl <- fitList(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)

#mod6 =  eyr_abond_marmax, mod7 = eyr_abond_mar50, mod8 = eyr_abond_ouvrage, mod9 = eyr_abond_alti, mod10 = eyr_abond_f500, mod11 = eyr_abond_dh500, mod12 = eyr_abond_dr500, mod13 = eyr_abond_rive, mod14 = eyr_abond_full)
modSel(fl)

ms_df <- as(modSel(fl), "data.frame")
c_hat <- 2.34

ms_df$logLik <- (2 * ms_df$nPars - ms_df$AIC) / 2
ms_df$QAIC <- -2 * ms_df$logLik / c_hat + 2 * ms_df$nPars
ms_df$deltaQAIC <- ms_df$QAIC - min(ms_df$QAIC)
ms_df$QAICwt <- exp(-0.5 * ms_df$deltaQAIC) / sum(exp(-0.5 * ms_df$deltaQAIC))

ms_df[order(ms_df$QAIC), c("nPars", "AIC", "QAIC", "deltaQAIC", "QAICwt")]

vif(EYR_site_covs %>% select(where(is.numeric)))

################################################################################
#################### Estimation de l'abondance relative  #######################
################################################################################

# gluèyre
y_glu <- dataglu %>% st_drop_geometry() %>% dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>% as.matrix()

umf_glu <- unmarkedFramePCount(y = y_glu, siteCovs = GLU_site_covs, obsCovs = GLU_obs_covs)

MOD_final_glu <- pcount(~NBR_MAR + U ~MAR_MAX + DH500, data = umf_glu, mixture = "ZIP")
summary(MOD_final_glu)
ajustglu <- Nmix.gof.test(MOD_final_glu, nsim = 1000)

re_final_glu <- ranef(MOD_final_glu)
N_estime_glu <- bup(re_final_glu, stat = "mean")

resultats_glu <- data.frame(
  ID_SITE = dataglu$ID_SITE,
  Abondance_estimee = N_estime_glu)

# Vérification critique
nrow(resultats_glu) == nrow(dataglu)

head(resultats_glu)
summary(N_estime_glu)
sum(N_estime_glu)

obs_max_glu <- apply(getY(umf_glu), 1, max, na.rm = TRUE)

plot(obs_max_glu, N_estime_glu,
     pch = 16, col = "steelblue",
     xlab = "Maximum observé par site",
     ylab = "Abondance estimée (N)",
     main = "Gluèyre : observé vs estimé")
abline(0, 1, col = "red", lty = 2)

# eyrieux
MOD_final_eyr <- pcount(~NBR_MAR + U  ~MAR_MAX + ALTI, data = umf_eyr, mixture = "ZIP")
summary(MOD_final_eyr)
ajusteyr <- Nmix.gof.test(mod4, nsim = 1000)

re_final_eyr <- ranef(MOD_final_eyr)
N_estime_eyr <- bup(re_final_eyr, stat = "mean")

resultats_eyr <- data.frame(
  ID_SITE = dataeyr$ID_SITE,
  Abondance_estimee = N_estime_eyr
)
nrow(resultats_eyr) == nrow(dataeyr)

summary(N_estime_eyr)
sum(N_estime_eyr)

obs_max_eyr <- apply(getY(umf_eyr), 1, max, na.rm = TRUE)
plot(obs_max_eyr, N_estime_eyr,
     pch = 16, col = "darkgreen",
     xlab = "Maximum observé par site",
     ylab = "Abondance estimée (N)",
     main = "Eyrieux : observé vs estimé")
abline(0, 1, col = "red", lty = 2)

# Eyrieux
se_eyr <- SE(MOD_final_eyr) * sqrt(2.63)
# Gluèyre  
se_glu <- SE(MOD_final_glu) * sqrt(3.73)
# Recalcul des z et p-values corrigés
z_eyr <- coef(MOD_final_eyr) / se_eyr
p_eyr <- 2 * pnorm(-abs(z_eyr))
z_glu <- coef(MOD_final_glu) / se_glu
p_glu <- 2 * pnorm(-abs(z_glu))
data.frame(Estimate = coef(MOD_final_eyr), SE_corrigee = se_eyr, z = z_eyr, p = p_eyr)
data.frame(Estimate = coef(MOD_final_glu), SE_corrigee = se_glu, z = z_glu, p = p_glu)

round(p_eyr, 9)
round(p_glu, 9)

# obtenir les intervalles de confiance : 
Ntotal_fn <- function(mod) {
  sum(bup(ranef(mod), stat = "mean"))}

# Eyrieux ####
pb_eyr <- parboot(MOD_final_eyr, statistic = Ntotal_fn, nsim = 1000)
range(pb_eyr@t.star)
quantile(pb_eyr@t.star, c(0.025, 0.975))

Pdet_fn <- function(mod) {
  mean(predict(mod, type = "det")$Predicted, na.rm = TRUE)
}
pb_det_eyr <- parboot(MOD_final_eyr, statistic = Pdet_fn, nsim = 1000)
# Valeur centrale (observée sur le modèle original, pas simulée)
Pdet_fn(MOD_final_eyr)

# Intervalle de confiance à 95%
quantile(pb_det_eyr@t.star, c(0.025, 0.975))


# proba détec 
stats_combines <- function(mod) {
  c(Ntotal = sum(bup(ranef(mod), stat = "mean")),
    Pdet = mean(predict(mod, type = "det")$Predicted, na.rm = TRUE))}

pb_eyr_combine <- parboot(MOD_final_eyr, statistic = stats_combines, nsim = 1000)
# Résultats
pb_eyr_combine@t0  # valeurs observées (Ntotal et Pdet)
apply(pb_eyr_combine@t.star, 2, quantile, probs = c(0.025, 0.975))  # IC 95% pour les deux

# Gluèyre ####

pb_glu_combine <- parboot(MOD_final_glu, statistic = stats_combines, nsim = 1000)
pb_glu_combine@t0 
apply(pb_glu_combine@t.star, 2, quantile, probs = c(0.025, 0.975))  # IC 95% pour les deux
summary(pb_glu_combine@t.star[, "Ntotal"])
hist(pb_glu_combine@t.star[, "Ntotal"], breaks = 50)
abline(v = pb_glu_combine@t0["Ntotal"], col = "red", lwd = 2)
