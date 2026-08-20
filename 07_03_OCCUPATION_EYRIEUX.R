# ce script a pour objectif d'obtenir la probabilité d'occupation des sites sur l'Eyrieux

pacman::p_load(readr, ggplot2, tidyverse, dplyr, unmarked, AICcmodavg, car, corrplot, sf)

################################################################################
########################## Préparation des données #############################
################################################################################

load("SAUVEGARDES/data_mod.RData")
dataeyr <- data %>% filter(RIVIERE == "Eyrieux")

dataeyr <- dataeyr %>%
  mutate(across(starts_with("NBR_MAR_P"), ~ ifelse(is.na(.), 0, .)))

# site covs
EYR_site_covs <- dataeyr %>% select(ALTI, MAR_MAX,MAR50, OUVRAGE, F100, F500, DH500, DR100, DR500, RIVE)

# Covariables de site
cor_site <- EYR_site_covs %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_site, method = "number", type = "upper", tl.cex = 0.7)
# id les paires avec le > 0.4
which(abs(cor_site) > 0.4 & abs(cor_site) < 1, arr.ind = TRUE)
round(cor_site, 2)
vif(EYR_site_covs %>% select(where(is.numeric)))

#MAR_MAX / MAR50 (r = 0,52)
# éviter de les mettre ensemble dans les modèles

EYR_site_covs <- dataeyr %>% select(ALTI, MAR_MAX,MAR50, OUVRAGE, F100, DH500, DR100, DR500, RIVE)

#obs covs 

EYR_obs_covs <- list(
  OBS     = dataeyr %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataeyr %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataeyr %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataeyr %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  TEMP    = dataeyr %>% st_drop_geometry() %>% dplyr::select(T_P1, T_P2, T_P3),
  U       = dataeyr %>% st_drop_geometry() %>% dplyr::select(U_P1, U_P2, U_P3),
  FF      = dataeyr %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataeyr %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3), 
  H_EAU   = dataeyr %>% st_drop_geometry() %>% dplyr::select(H_P1, H_P2, H_P3))

obs_long <- dataeyr %>%
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

EYR_obs_covs_2 <- data.frame(
  HEURE = unlist(EYR_obs_covs$HEURE),
  DATE  = unlist(EYR_obs_covs$DATE),
  TEMP  = unlist(EYR_obs_covs$TEMP),
  U     = unlist(EYR_obs_covs$U),
  FF    = unlist(EYR_obs_covs$FF),
  RR1   = unlist(EYR_obs_covs$RR1),
  NBR_MAR = unlist(EYR_obs_covs$NBR_MAR))

vif(EYR_obs_covs_2 %>% select(where(is.numeric)))
corrplot(cor_obs, order = "hclust", addrect = 3)
# TEMP, DATE U et H sont encore une fois corrélé ce qui nécessite de faire un choix qui va être la DATE

GLU_obs_covs <- list(
  OBS     = dataglu %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataglu %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataglu %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataglu %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  FF      = dataglu %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataglu %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

################################################################################
###################### Création des modèles d'occupation #######################
################################################################################

y.wt <- dataeyr %>% 
  st_drop_geometry() %>% 
  dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>%
  mutate(across(everything(), ~ as.numeric(. > 0))) %>%
  as.matrix()

umfeyr <- unmarkedFrameOccu(y=y.wt,
                            siteCovs= EYR_site_covs,
                            obsCovs=EYR_obs_covs)

umfeyr@obsCovs$NBR_MAR_log <- log1p(umfeyr@obsCovs$NBR_MAR)
umfeyr@siteCovs$MAR_MAXlog <- log1p(umfeyr@siteCovs$MAR_MAX)
umfeyr@siteCovs$MAR50log <- log1p(umfeyr@siteCovs$MAR50)

fm0 <- occu(formula = ~ 1 ~1, data = umfeyr)
predict(fm0,type="state")
predict(fm0,type="det")

obs.boot.fm0 <- AICcmodavg::mb.gof.test(fm0, nsim = 100, lot.hist=F)
# mauvais ajustement

packageVersion("AICcmodavg")
?Nmix.gof.test()
################################################################################
########################## Intégration des covs  ###############################
################################################################################

# détection
fm0 <- occu(formula = ~ 1 ~1, data = umfeyr)
fm1 <- occu(formula = ~OBS ~1, data = umfeyr)
fm2 <- occu(formula = ~HEURE ~1, data = umfeyr)
fm3 <- occu(formula = ~DATE ~1, data = umfeyr)
fm4 <- occu(formula = ~FF ~1, data = umfeyr)
fm5 <- occu(formula = ~NBR_MAR ~1, data = umfeyr)
fm6 <- occu(formula = ~RR1 ~1, data = umfeyr)
fm7 <- occu(formula = ~RR1 + NBR_MAR + FF + DATE + HEURE + OBS ~1, data = umfeyr)
fm8 <- occu(formula = ~NBR_MAR_log ~1, data = umfeyr)

obs.boot.fm7 <- AICcmodavg::mb.gof.test(fm7, nsim = 1000, lot.hist=F)

AIC_detect_eyr_occu <- aictab(list(fm0, fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8),
       modnames = c("null","OBS","HEURE","DATE","FF","NBR_MAR","RR1","full", "log"))

# NBR_MAR_log meilleur après le full

summary(fm7)

fm_nbrmar_obs    <- occu(~ NBR_MAR_log + OBS ~ 1, data = umfeyr)
fm_nbrmar_heure  <- occu(~ NBR_MAR_log + HEURE ~ 1, data = umfeyr)
fm_nbrmar_date   <- occu(~ NBR_MAR_log + DATE ~ 1, data = umfeyr)
fm_nbrmar_ff     <- occu(~ NBR_MAR_log + FF ~ 1, data = umfeyr)
fm_nbrmar_rr1    <- occu(~ NBR_MAR_log + RR1 ~ 1, data = umfeyr)

AIC_detect_eyr_occu_bi <- aictab(list(fm7, fm5, fm_nbrmar_obs, fm_nbrmar_heure, fm_nbrmar_date, fm_nbrmar_ff, fm_nbrmar_rr1), modnames = c("full", "nbr_mar","NBR_MAR+obs","NBR_MAR+heure", "NBR_MAR+date", "NBR_MAR+ff", "NBR_MAR+rr1"))

# le support de détection est NBR_MAR_log + RR1

########################## occupation ###############################

fm7_marmax <- occu(formula = ~ NBR_MAR_log + RR1 ~ MAR_MAX, data = umfeyr)

obs.boot.fm7marmax <- AICcmodavg::mb.gof.test(fm7_marmax, nsim = 1000, lot.hist=F)

fm7_full <- occu(formula = ~RR1 + NBR_MAR + FF + DATE + HEURE + OBS ~ MAR_MAX + ALTI + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfeyr)

obs.boot.fm7full <- AICcmodavg::mb.gof.test(fm7_full, nsim = 1000, lot.hist=F)

fm_full_RR1_MAR <- occu(formula = ~RR1 + NBR_MAR_log~ MAR_MAX + ALTI + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfeyr)

obs.boot.fm_full_RR1_MAR <- AICcmodavg::mb.gof.test(fm_full_RR1_MAR, nsim = 1000, lot.hist=F)
# C-HAT DE 2.62

fm_occ_null    <- occu(~RR1 + NBR_MAR_log~ 1, data = umfeyr)
fm_occ_alti    <- occu(~RR1 + NBR_MAR_log~ ALTI, data = umfeyr)
fm_occ_marmax  <- occu(~RR1 + NBR_MAR_log~ MAR_MAX, data = umfeyr)
fm_occ_marmaxlog <- occu(~RR1 + NBR_MAR_log~ MAR_MAXlog, data = umfeyr)
fm_occ_mar50   <- occu(~RR1 + NBR_MAR_log~ MAR50, data = umfeyr)
fm_occ_ouvrage <- occu(~RR1 + NBR_MAR_log~ OUVRAGE, data = umfeyr)
fm_occ_f100    <- occu(~RR1 + NBR_MAR_log~ F100, data = umfeyr)
fm_occ_dh500   <- occu(~RR1 + NBR_MAR_log~ DH500, data = umfeyr)
fm_occ_dr100   <- occu(~RR1 + NBR_MAR_log~ DR100, data = umfeyr)
fm_occ_dr500   <- occu(~RR1 + NBR_MAR_log~ DR500, data = umfeyr)
fm_occ_rive    <- occu(~RR1 + NBR_MAR_log~ RIVE, data = umfeyr)
fm_full_RR1_MAR <- occu(formula = ~RR1 + NBR_MAR_log~ MAR_MAX + ALTI + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfeyr)

summary(fm_full_RR1_MAR)

AIC_occu_eyr <- aictab(list(fm_occ_null, fm_occ_alti, fm_occ_marmax, fm_occ_marmaxlog, fm_occ_mar50, fm_occ_ouvrage, fm_occ_f100, fm_occ_dh500, fm_occ_dr100, fm_occ_dr500, fm_occ_rive, fm_full_RR1_MAR), modnames = c("null", "ALTI", "MAR_MAX", "MAR_MAX_log", "MAR50", "OUVRAGE", "F100", "DH500", "DR100", "DR500", "RIVE","full"), c.hat =2.62)

# alti est retenu 

fm_bi_MARMAX <- occu(formula = ~RR1 + NBR_MAR_log ~ ALTI + MAR_MAX, data = umfeyr)
fm_bi_MARMAXlog <- occu(formula = ~RR1 + NBR_MAR_log ~ ALTI + MAR_MAXlog, data = umfeyr)
fm_bi_ouvrage <- occu(formula = ~RR1 + NBR_MAR_log~ ALTI + OUVRAGE, data = umfeyr)
fm_bi_F100 <- occu(formula = ~RR1 + NBR_MAR_log~ ALTI + F100, data = umfeyr)
fm_bi_MAR50 <- occu(formula = ~RR1 + NBR_MAR_log~ ALTI + MAR50, data = umfeyr)
fm_bi_MAR50log <- occu(formula = ~RR1 + NBR_MAR_log~ ALTI + MAR50log, data = umfeyr)
fm_bi_DH500 <- occu(formula = ~RR1 + NBR_MAR_log~ ALTI + DH500, data = umfeyr)
fm_bi_DR100 <- occu(formula = ~RR1 + NBR_MAR_log~ ALTI + DR100, data = umfeyr)
fm_bi_DR500 <- occu(formula = ~RR1 + NBR_MAR~ ALTI + DR500, data = umfeyr)
fm_bi_RIVE <- occu(formula = ~RR1 + NBR_MAR~ ALTI + RIVE, data = umfeyr)

AIC_occu_eyr_bi <- aictab(list(fm_bi_MARMAX,fm_bi_MARMAXlog, fm_bi_ouvrage, fm_bi_F100, fm_bi_MAR50,fm_bi_MAR50log, fm_bi_DH500, fm_bi_DR100, fm_bi_DR500, fm_bi_RIVE, fm_full_RR1_MAR), modnames = c("ALTI + MAR_MAX", "ALTI + MAR_MAXlog", "ALTI + OUVRAGE", "ALTI + F100", "ALTI + MAR50","ALTI + MAR50log" ,"ALTI + DH500", "ALTI + DR100", "ALTI + DR500", "ALTI + RIVE", "full"), c.hat =2.62)

fm_bi_MARMAXlogFDH500 <- occu(formula = ~RR1 + NBR_MAR_log ~ ALTI + MAR_MAXlog + DH500, data = umfeyr)
fm_bi_MARMAXlogOUV <- occu(formula = ~RR1 + NBR_MAR_log ~ ALTI + MAR_MAXlog + OUVRAGE, data = umfeyr)
fm_bi_MARMAXlogDR100 <- occu(formula = ~RR1 + NBR_MAR_log ~ ALTI + MAR_MAXlog + DR100, data = umfeyr)
fm_bi_MARMAXlogF100 <- occu(formula = ~RR1 + NBR_MAR_log ~ ALTI + MAR_MAXlog + F100, data = umfeyr)

AIC_occu_eyr_tri <- aictab(list(fm_bi_MARMAXlog, fm_bi_MARMAXlogFDH500,fm_bi_MARMAXlogOUV, fm_bi_MARMAXlogDR100, fm_bi_MARMAXlogF100, fm_full_RR1_MAR), modnames = c("ALTI + MAR_MAXlog","ALTI + MAR_MAXlog + DH500", "ALTI + MAR_MAXlog + OUVRAGE", "ALTI + MAR_MAXlog + DR100", "ALTI + MAR_MAXlog + F100", "full"), c.hat =2.62)

# MAR50 et MAR_MAX pareil 

# alti et MAR50 et MAR_MAX C'EST BIEN 

fm_MARMAX_log <- occu(~RR1 + NBR_MAR_log ~ MAR_MAX_log + ALTI,
                      data = umfeyr)

fm_MAR50_log <- occu(~RR1 + NBR_MAR_log ~ MAR_MAX_log + ALTI,
                      data = umfeyr)

aictab(list(fm_bi_MARMAX, fm_MARMAX_log, fm_occ_full),modnames = c("MAR_MAX linéaire", "log(MAR_MAX + 1)", "full"), c.hat = 2.72)

mod_FINAL_occu_EYR <- fm_bi_MARMAXlog

obs.boot.fm_final_eyr <- AICcmodavg::mb.gof.test(mod_FINAL_occu_EYR, nsim = 1000, lot.hist=F)

summary(mod_FINAL_occu_EYR)

################################################################################
################# CALCUL de l'occupation et de la détection ####################
################################################################################

# occupation

predict(mod_FINAL_occu_EYR, 'state')
pred_occ <- predict(mod_FINAL_occu_EYR, type = "state")

set.seed(123)

B <- 2000

mean_psi <- numeric(B)

for (i in 1:B) {
  # simulation des coefficients du modèle
  beta <- MASS::mvrnorm(
    1,
    mu = coef(mod_FINAL_occu_EYR, type = "state"),
    Sigma = vcov(mod_FINAL_occu_EYR, type = "state"))
  X <- model.matrix(~ ALTI + MAR_MAX_log, data = as.data.frame(siteCovs(umfeyr)))
  
  psi <- plogis(X %*% beta)
  
  mean_psi[i] <- mean(psi)
}

mean_psi_obs <- mean(pred_occ$Predicted, na.rm = TRUE)

IC_psi <- quantile(mean_psi, c(0.025, 0.975), na.rm = TRUE)

mean_psi_obs
IC_psi

# détection 

set.seed(123)

B <- 2000

mean_p <- numeric(B)

for (i in 1:B) {
  
  beta <- MASS::mvrnorm(1,
                        mu = coef(mod_FINAL_occu_EYR, type = "det"),
                        Sigma = vcov(mod_FINAL_occu_EYR, type = "det"))
  
  X <- model.matrix(
    ~ RR1 + NBR_MAR_log,
    data = as.data.frame(obsCovs(umfeyr)))
  
  p <- plogis(X %*% beta)
  
  mean_p[i] <- mean(p)
}

mean_p_obs <- mean(predict(mod_FINAL_occu_EYR, type = "det")$Predicted,
  na.rm = TRUE)

IC_p <- quantile(
  mean_p,
  c(0.025, 0.975),
  na.rm = TRUE)

mean_p_obs
IC_p


# export de la table d'aic 
AIC_detect_eyr_occu
AIC_detect_eyr_occu_bi
AIC_occu_eyr
AIC_occu_eyr_bi
AIC_occu_eyr_tri

liste_aic <- list(
  "Structure détection univariée" = AIC_detect_eyr_occu, 
  "Structure détection bivariée" = AIC_detect_eyr_occu_bi,
  "Structure détection univariée" = AIC_occu_eyr,
  "Structure abondance bivariée" = AIC_occu_eyr_bi,
  "Structure abondance trivariée" = AIC_occu_eyr_tri)

tableau_complet <- imap_dfr(liste_aic, ~ as.data.frame(.x) %>% 
                              mutate(Modele_set = .y)) %>%
  mutate(AICc_final    = ifelse(is.na(c_hat), AICc, QAICc),
         Delta_final   = ifelse(is.na(c_hat), Delta_AICc, Delta_QAICc),
         Poids_final   = ifelse(is.na(c_hat), AICcWt, QAICcWt),
         Critere       = ifelse(is.na(c_hat), "AICc", "QAICc")) %>%
  select(Modele_set, Modnames, K, Critere, AICc_final, Delta_final, Poids_final, c_hat) %>%
  mutate(across(c(AICc_final, Delta_final, Poids_final), ~ round(.x, 2)))

write.csv(tableau_complet, "annexe_AIC_OCCUPATION_eyr.csv", row.names = FALSE)

