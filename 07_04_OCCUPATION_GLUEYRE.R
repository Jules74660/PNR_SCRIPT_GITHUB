# ce script a pour objectif d'obtenir la probabilité d'occupation des sites sur la Gluèyre

pacman::p_load(readr, ggplot2, tidyverse, dplyr, unmarked, AICcmodavg, car, corrplot)

################################################################################
########################## Préparation des données #############################
################################################################################

load("SAUVEGARDES/data_mod.RData")

dataglu <- data %>% filter(RIVIERE == "Glueyre")
id_a_retirer <- dataglu$ID_SITE[509]
id_a_retirer
dataglu <- dataglu %>% filter(ID_SITE != id_a_retirer)

dataglu <- dataglu %>%
  mutate(across(starts_with("NBR_MAR_P"), ~ ifelse(is.na(.), 0, .)))

GLU_site_covs <- dataglu %>% select(ALTI, MAR_MAX,MAR50, OUVRAGE, F100, F500, DH500, DR100, DR500, RIVE)

# Covariables de site
cor_site <- GLU_site_covs %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_site, method = "number", type = "upper", tl.cex = 0.7)
# id les paires avec le > 0.4
which(abs(cor_site) > 0.4 & abs(cor_site) < 1, arr.ind = TRUE)
round(cor_site, 2)
vif(GLU_site_covs %>% select(where(is.numeric)))

GLU_site_covs <- dataglu %>% select(ALTI, MAR_MAX,MAR50, OUVRAGE, F100, DH500, DR100, DR500, RIVE)
# on enlève F500 car corrélé à F100 et par pertinence écologique au regard des capacités de déplacement du sonneur 

# pas de soucis sur les données de sites 
#OUVRAGE / DR500 (r = -0,55)
#F500 / DR500 (r = -0,53)
#ALTI / OUVRAGE (r = -0,50)
#MAR_MAX / MAR50 (r = 0,47)
# éviter de les mettre ensemble dans les modèles
vif(GLU_site_covs %>% select(where(is.numeric)))

# Covariables d'observation

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

GLU_obs_covs_2 <- data.frame(
  HEURE = unlist(GLU_obs_covs$HEURE),
  DATE  = unlist(GLU_obs_covs$DATE),
  TEMP  = unlist(GLU_obs_covs$TEMP),
  U     = unlist(GLU_obs_covs$U),
  FF    = unlist(GLU_obs_covs$FF),
  RR1   = unlist(GLU_obs_covs$RR1),
  NBR_MAR = unlist(GLU_obs_covs$NBR_MAR))
vif(GLU_obs_covs_2 %>% select(where(is.numeric)))
corrplot(cor_obs, order = "hclust", addrect = 3)

# TEMP : problème
# DATE : problème
# FF : pas de problème
# HEURE : pas de problème
# NBR_MAR : pas de problème
# RR1 : pas de problème
# U : problème
# H : problème

# On ne garde que DATE, FF, HEURE, NBR_MAR, RR1 et OBS

GLU_obs_covs <- list(
  OBS     = dataglu %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataglu %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataglu %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataglu %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  FF      = dataglu %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataglu %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

# GLU_obs_covs : DATE, FF, HEURE, NBR_MAR, RR1 et OBS
# GLU_site_covs : ALTI, MAR_MAX,MAR50, OUVRAGE, F100, DH500, DR100, DR500, RIVE
# pas de soucis sur les données de sites 
#OUVRAGE / DR500 (r = -0,55)
#F500 / DR500 (r = -0,53)
#ALTI / OUVRAGE (r = -0,50)
#MAR_MAX / MAR50 (r = 0,47)
# éviter de les mettre ensemble dans les modèles

################################################################################
####################### Création des modèles d'occu ############################
################################################################################

y.wt <- dataglu %>% 
  st_drop_geometry() %>% 
  dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>%
  mutate(across(everything(), ~ as.numeric(. > 0))) %>%
  as.matrix()

umfglu <- unmarkedFrameOccu(y=y.wt,
                            siteCovs= GLU_site_covs,
                            obsCovs=GLU_obs_covs)

umfglu@obsCovs$NBR_MAR_log <- log1p(umfglu@obsCovs$NBR_MAR)
summary(umfglu@siteCovs$MAR_MAX)
hist(umfglu@siteCovs$MAR_MAX)
umfglu@siteCovs$MAR_MAX_log <- log1p(umfglu@siteCovs$MAR_MAX)

fm0 <- occu(formula = ~ 1 ~1, data = umfglu)
predict(fm0,type="state")
predict(fm0,type="det")

obs.boot.fm0 <- AICcmodavg::mb.gof.test(fm0, nsim = 100, lot.hist=F)
# très mauvais ajustement

# GLU_obs_covs : DATE, FF, HEURE, NBR_MAR, RR1 et OBS
# GLU_site_covs : ALTI, MAR_MAX,MAR50, OUVRAGE, F100, DH500, DR100, DR500, RIVE

full <- occu(formula = ~ DATE + FF + HEURE + NBR_MAR + RR1 + OBS ~ MAR_MAX + ALTI + MAR50 + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfglu)

obs.boot.full <- AICcmodavg::mb.gof.test(full, nsim = 1000, lot.hist=F)
# 6.5 pas possible pour le QAic

################################################################################
########################## Intégration des covs  ###############################
################################################################################

# d'abord test de la corrélation entre NBR_MAR et MAR_MAX car pas possible de mettre les deux ensemble

cor.test(dataglu$NBR_MAR_P1, dataglu$MAR_MAX, method = "spearman")
cor.test(dataglu$NBR_MAR_P2, dataglu$MAR_MAX, method = "spearman")
cor.test(dataglu$NBR_MAR_P3, dataglu$MAR_MAX, method = "spearman")

fm0 <- occu(formula = ~ 1 ~1, data = umfglu)
fm00 <- occu(formula = ~ NBR_MAR ~1, data = umfglu)
fm000 <- occu(formula = ~ 1 ~ MAR_MAX, data = umfglu)
fm0000 <- occu(formula = ~ NBR_MAR ~ MAR_MAX, data = umfglu)

aictab(list(fm0, fm00, fm000, fm0000),
       modnames = c("null","NBRMAR","MAR_MAX","NBR_MAR ~ MAR_MAX"))

fm0 <- occu(formula = ~ 1 ~1, data = umfglu)
fm1 <- occu(formula = ~OBS ~1, data = umfglu)
fm2 <- occu(formula = ~HEURE ~1, data = umfglu)
fm3 <- occu(formula = ~DATE ~1, data = umfglu)
fm4 <- occu(formula = ~FF ~1, data = umfglu)
fm5 <- occu(formula = ~NBR_MAR ~1, data = umfglu)
fm6  <- occu(~ NBR_MAR_log + RR1 ~ 1, data = umfglu)
fm7 <- occu(formula = ~RR1 ~1, data = umfglu)
fm8 <- occu(formula = ~RR1 + NBR_MAR + FF + DATE + HEURE + OBS ~1, data = umfglu)

AIC_detect_glu_occu <-aictab(list(fm0, fm1, fm2, fm3, fm4, fm5, fm6, fm7, fm8),
       modnames = c("null","OBS","HEURE","DATE","FF","NBR_MAR","NBR_MAR_log","RR1","full"))

#NBR_MAR domine très largement et l'association de certaines variables améliore le modèle
# les autres variables n'apportent quasi-rien tout seul

summary(fm7)

fm_nbrmar_obs    <- occu(~ NBR_MAR + OBS ~ 1, data = umfglu)
fm_nbrmar_heure  <- occu(~ NBR_MAR + HEURE ~ 1, data = umfglu)
fm_nbrmar_date   <- occu(~ NBR_MAR + DATE ~ 1, data = umfglu)
fm_nbrmar_ff     <- occu(~ NBR_MAR + FF ~ 1, data = umfglu)
fm_nbrmar_rr1    <- occu(~ NBR_MAR + RR1 ~ 1, data = umfglu)
fm_nbrmarlog_obs    <- occu(~ NBR_MAR_log + OBS ~ 1, data = umfglu)
fm_nbrmarlog_heure  <- occu(~ NBR_MAR_log + HEURE ~ 1, data = umfglu)
fm_nbrmarlog_date   <- occu(~ NBR_MAR_log + DATE ~ 1, data = umfglu)
fm_nbrmarlog_ff     <- occu(~ NBR_MAR_log + FF ~ 1, data = umfglu)
fm_nbrmarlog_rr1    <- occu(~ NBR_MAR_log + RR1 ~ 1, data = umfglu)

AIC_detect_glu_occu_bi <- aictab(list(fm8, fm5, fm_nbrmar_obs, fm_nbrmar_heure, fm_nbrmar_date, fm_nbrmar_ff, fm_nbrmar_rr1, fm_nbrmarlog_obs, fm_nbrmarlog_heure, fm_nbrmarlog_date, fm_nbrmarlog_ff, fm_nbrmarlog_rr1), modnames = c("full", "nbr_mar","NBR_MAR+obs","NBR_MAR+heure", "NBR_MAR+date", "NBR_MAR+ff", "NBR_MAR+rr1", "NBR_MARlog+obs","NBR_MARlog+heure", "NBR_MARlog+date","NBR_MARlog+ff","NBR_MARlog+rr1"))

# DATE et NBR_MAR c'est très bien ensemble 

nbrmar <- occu(~ NBR_MAR + DATE ~ MAR_MAX + ALTI, data = umfglu)
nbrmarlog <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI, data = umfglu)

aictab(list(nbrmar, nbrmarlog), modnames = c("nbr_mar", "nbr_marlog"))

full_site_obs <- occu(~ NBR_MAR + DATE ~ MAR_MAX + ALTI + MAR50 + OUVRAGE + DR500, data = umfglu)

full_site_obslog <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + MAR50 + OUVRAGE + DR500, data = umfglu)

full_site_obslog2 <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + MAR50 + OUVRAGE + F100 + DR100 + DR500 + DH500 + RIVE, data = umfglu)

summary(full_site_obslog2)
obs.boot.fullQAIC <- AICcmodavg::mb.gof.test(full_site_obs, nsim = 100, lot.hist=F)
obs.boot.fulllogQAIC <- AICcmodavg::mb.gof.test(full_site_obslog, nsim = 100, lot.hist=F)
# celui retenu est logmar et date 
# chat de 4.93 donc pas possible de faire le Qaicc

########################## occupation ###############################

fm_occ_null    <- occu(~ NBR_MAR_log + DATE ~ 1, data = umfglu)
fm_occ_alti    <- occu(~ NBR_MAR_log + DATE ~ ALTI, data = umfglu)
fm_occ_marmax  <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX, data = umfglu)
fm_occ_mar50   <- occu(~ NBR_MAR_log + DATE ~ MAR50, data = umfglu)
fm_occ_ouvrage <- occu(~ NBR_MAR_log + DATE ~ OUVRAGE, data = umfglu)
fm_occ_f100    <- occu(~ NBR_MAR_log + DATE ~ F100, data = umfglu)
fm_occ_dh500   <- occu(~ NBR_MAR_log + DATE ~ DH500, data = umfglu)
fm_occ_dr100   <- occu(~ NBR_MAR_log + DATE ~ DR100, data = umfglu)
fm_occ_dr500   <- occu(~ NBR_MAR_log + DATE ~ DR500, data = umfglu)
fm_occ_rive    <- occu(~ NBR_MAR_log + DATE ~ RIVE, data = umfglu)

AIC_occu_glu <- aictab(list(fm_occ_null, fm_occ_alti, fm_occ_marmax, fm_occ_mar50, fm_occ_ouvrage, fm_occ_f100, fm_occ_dh500, fm_occ_dr100, fm_occ_dr500, fm_occ_rive), modnames = c("null", "ALTI", "MAR_MAX", "MAR50", "OUVRAGE", "F100", "DH500", "DR100", "DR500", "RIVE"))

# MAR_MAX est le meilleur
# enlèver MAR50 car moins bien et ne tester que les combinaisons avec MAR_MAX

fm_occ_marmax_alti     <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI, data = umfglu)
fm_occ_marmax_ouvrage  <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + OUVRAGE, data = umfglu)
fm_occ_marmax_f100     <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + F100, data = umfglu)
fm_occ_marmax_dh500    <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + DH500, data = umfglu)
fm_occ_marmax_dr100    <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + DR100, data = umfglu)
fm_occ_marmax_dr500    <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + DR500, data = umfglu)
fm_occ_marmax_rive     <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + RIVE, data = umfglu)
fm_MARMAX_log <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX_log + ALTI,data = umfglu)

AIC_occu_glu_bi <- aictab(list(fm_occ_marmax, fm_occ_marmax_alti, fm_occ_marmax_ouvrage, fm_occ_marmax_f100, fm_occ_marmax_dh500,fm_occ_marmax_dr100, fm_occ_marmax_dr500, fm_occ_marmax_rive,fm_MARMAX_log), modnames = c("MAR_MAX", "MAR_MAX + ALTI", "MAR_MAX + OUVRAGE", "MAR_MAX + F100", "MAR_MAX + DH500","MAR_MAX + DR100", "MAR_MAX + DR500", "MAR_MAX + RIVE", "log"))

fm_occ_marmax_altiDR500 <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + DR500, data = umfglu)
fm_occ_marmax_altiDR100 <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + DR100, data = umfglu)
fm_occ_marmax_altiF100 <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + F100, data = umfglu)
fm_occ_marmax_altiRIVE <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + RIVE, data = umfglu)
fm_occ_marmax_altiDR500RIVE <- occu(~ NBR_MAR_log + DATE ~ MAR_MAX + ALTI + DR500 + RIVE, data = umfglu)

AIC_occu_glu_tri <- aictab(list(fm_occ_marmax_alti, fm_occ_marmax_altiDR500, fm_occ_marmax_altiDR100, fm_occ_marmax_altiF100, fm_occ_marmax_altiRIVE, fm_occ_marmax_altiDR500RIVE),modnames = c("MAR_MAX + ALTI", "MAR_MAX + ALTI + DR500", "MAR_MAX + ALTI + DR100", "MAR_MAX + ALTI + F100","MAR_MAX + ALTI + RIVE", "MAR_MAX + ALTI + DR500 + RIVE"))
 
cor(GLU_site_covs$DR500, GLU_site_covs$DR100, use = "complete.obs") # pas de sens
cor(GLU_site_covs$DR500, GLU_site_covs$ALTI, use = "complete.obs") # carré
cor(GLU_site_covs$ALTI, GLU_site_covs$OUVRAGE, use = "complete.obs") # corrélé
cor(GLU_site_covs$DR500, GLU_site_covs$F100, use = "complete.obs") # corrélé

# fm_occ_marmax_alti
# le modèle retenu est le NBRMAR log + date et MARMAX + ALTI 
# le modèle retenu est lieu enfait : fm_occ_marmax_altiDR500

obs.boot.fm_FINAL <- AICcmodavg::mb.gof.test(mod_FINAL_glu_occ, nsim = 1000, lot.hist=F)
predict(mod_FINAL_glu_occ, "state")

obs.boot.fm_FINAL2 <- AICcmodavg::mb.gof.test(fm_occ_marmax_altiDR500, nsim = 1000, lot.hist=F)

predict(mod_FINAL_glu_occ, 'state')
pred_occ <- predict(mod_FINAL_glu_occ, type = "state")
summary(predict(mod_FINAL_glu_occ, 'state'))
ggplot(pred_occ, aes(x = 1:nrow(pred_occ), y = Predicted)) +
  geom_point() +
  labs(x = "Site", y = "Probabilité d'occupation estimée") +
  theme_minimal()

plot(pred_occ$Predicted)

plot(dataglu$MAR_MAX, pred_occ$Predicted,
     xlab = "MAR_MAX",
     ylab = "Probabilité d'occupation prédite")

plot(dataglu$ALTI, pred_occ$Predicted,
     xlab = "Altitude",
     ylab = "Probabilité d'occupation prédite")

fm_occ_marmax_alti_sans_MAX <- occu(~ NBR_MAR + DATE ~ ALTI, data = umfglu)
fm_occ_marmax_alti_sans_MAR <- occu(~ DATE ~ MAR_MAX + ALTI, data = umfglu)

obs.boot.fm_FINAL_sans_MAX <- AICcmodavg::mb.gof.test(fm_occ_marmax_alti_sans_MAX, nsim = 100, lot.hist=F)
obs.boot.fm_FINAL_sans_MAR <- AICcmodavg::mb.gof.test(fm_occ_marmax_alti_sans_MAR, nsim = 100, lot.hist=F)


mod_FINAL_glu_occ <- fm_occ_marmax_altiDR500 
pred_occ <- predict(mod_FINAL_glu_occ, type = "state")
# occupation

set.seed(123)

B <- 2000

mean_psi <- numeric(B)

for (i in 1:B) {
  # simulation des coefficients du modèle
  beta <- MASS::mvrnorm(
    1,
    mu = coef(mod_FINAL_glu_occ, type = "state"),
    Sigma = vcov(mod_FINAL_glu_occ, type = "state"))
  
  X <- model.matrix(~ MAR_MAX + ALTI + DR500, data = as.data.frame(siteCovs(umfglu)))
  
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
    mu = coef(mod_FINAL_glu_occ, type = "det"),
    Sigma = vcov(mod_FINAL_glu_occ, type = "det"))
  
  X <- model.matrix(
    ~ NBR_MAR_log + DATE,
    data = as.data.frame(obsCovs(umfglu)))
  
  p <- plogis(X %*% beta)
  
  mean_p[i] <- mean(p)
}

mean_p_obs <- mean(
  predict(mod_FINAL_glu_occ, type = "det")$Predicted,
  na.rm = TRUE)

IC_p <- quantile(
  mean_p,
  c(0.025, 0.975),
  na.rm = TRUE)

mean_p_obs
IC_p

summary(mod_FINAL_glu_occ)


# export de la table d'aic 
AIC_detect_glu_occu
AIC_detect_glu_occu_bi
AIC_occu_glu
AIC_occu_glu_bi
AIC_occu_glu_tri

liste_aic <- list(
  "Structure détection univariée" = AIC_detect_glu_occu, 
  "Structure détection bivariée" = AIC_detect_glu_occu_bi,
  "Structure détection univariée" = AIC_occu_glu,
  "Structure abondance bivariée" = AIC_occu_glu_bi,
  "Structure abondance trivariée" = AIC_occu_glu_tri)

tableau_complet <- imap_dfr(liste_aic, ~ as.data.frame(.x) %>% 
                              mutate(Modele_set = .y)) %>%
  select(Modele_set, Modnames, K, AICc, Delta_AICc, AICcWt) %>%
  mutate(across(c(AICc, Delta_AICc, AICcWt), ~ round(.x, 2)))

write.csv(tableau_complet, "annexe_AIC_OCCUPATION.csv", row.names = FALSE)
