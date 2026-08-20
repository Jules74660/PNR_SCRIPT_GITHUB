# ce script a pour objectif d'estimer l'abondance relative des sites sur la Gluèyre

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
####################### Création des modèles N-Mixture #########################
################################################################################

y.count <- dataglu %>% 
  st_drop_geometry() %>% 
  dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>%
  as.matrix()

umfglu <- unmarkedFramePCount(y = y.count,
                                    siteCovs = GLU_site_covs,
                                    obsCovs = GLU_obs_covs)

umfglu@obsCovs$NBR_MAR_log <- log1p(umfglu@obsCovs$NBR_MAR)
umfglu@siteCovs$MAR_MAX_log <- log1p(umfglu@siteCovs$MAR_MAX)
umfglu@siteCovs$MAR50log <- log1p(umfglu@siteCovs$MAR50)

################################################################################
######################## Choix de la distribution et K #########################
################################################################################

fm_P   <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "P",   K = 250)
fm_NB  <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "NB",  K = 250)
fm_ZIP <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "ZIP", K = 250)

pred_P   <- predict(fm_P, type = "state")
pred_NB  <- predict(fm_NB, type = "state")
pred_ZIP <- predict(fm_ZIP, type = "state")

sum(pred_P$Predicted, na.rm = TRUE)
sum(pred_NB$Predicted, na.rm = TRUE)
sum(pred_ZIP$Predicted, na.rm = TRUE)

fm_P@opt$convergence
fm_NB@opt$convergence
fm_ZIP@opt$convergence

AIC_distri <- aictab(list(fm_P, fm_NB, fm_ZIP), modnames = c("Poisson", "NegBin", "ZIP"))
# NegBin domine largement mais très instable et donne des estimations à 49 000 individus
# ZIP est le deuxième avec des estimations plus cohérentes 

K_vals <- c(30, 50, 75, 100, 150, 200, 250, 300)

resultats <- lapply(K_vals, function(k) {
  fm <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "ZIP", K = k)
  data.frame(K = k, 
             lam = coef(fm, type="state"),
             AICc = AICc(fm),
             convergence = fm@opt$convergence)
})

do.call(rbind, resultats)

K_vals <- c(30, 50, 75, 100, 150, 200, 250, 300)

resultats_P <- lapply(K_vals, function(k) {
  fm <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "P", K = k)
  data.frame(K = k, 
             lam = coef(fm, type="state"),
             AICc = AICc(fm),
             convergence = fm@opt$convergence)
})

do.call(rbind, resultats_P)

fm_P   <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "P",   K = 75)
fm_ZIP <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfglu, mixture = "ZIP", K = 75)

aictab(list(fm_P, fm_ZIP), modnames = c("Poisson", "ZIP"))

################################################################################
######################## ajustement Qaic avec chat  ############################
################################################################################

fmfull_ZIP <- pcount(~ NBR_MAR_log + OBS + HEURE + DATE + FF + RR1 ~ MAR_MAX + MAR_MAX + ALTI + MAR50 + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfglu, mixture = "ZIP", K = 75)

fmfullobs_ZIP <- pcount(~ NBR_MAR_log + OBS + HEURE + DATE + FF + RR1 ~ 1, data = umfglu, mixture = "ZIP", K = 75)

fmfullsite_ZIP <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + ALTI + MAR50 + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfglu, mixture = "ZIP", K = 75)

summary(fmfullsite_ZIP)

obs.boot.fullgluabond <- Nmix.gof.test(fmfull_ZIP, nsim = 100, lot.hist=F)
# 3.12
obs.boot.fullgluobs <- Nmix.gof.test(fmfullobs_ZIP, nsim = 100, lot.hist=F)
# 3.69
obs.boot.fullglusite <- Nmix.gof.test(fmfullsite_ZIP, nsim = 100, lot.hist=F)
# 2.98
################################################################################
########################## Intégration des covs  ###############################
################################################################################

mod_det_null   <- pcount(~1 ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_obs    <- pcount(~OBS ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_heure  <- pcount(~HEURE ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_date   <- pcount(~DATE ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_temp   <- pcount(~TEMP ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_u      <- pcount(~U ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_ff     <- pcount(~FF ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_rr1    <- pcount(~RR1 ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_nbrmar <- pcount(~NBR_MAR ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_nbrmarlog <- pcount(~NBR_MAR_log ~1, data = umfglu, mixture = "ZIP", K = 75)
mod_det_full   <- pcount(~OBS + HEURE + DATE + FF + RR1 + NBR_MAR ~1, 
                         data = umfglu, mixture = "ZIP", K = 75)

AIC_detect_glu <- aictab(list(mod_det_null, mod_det_obs, mod_det_heure, mod_det_date, mod_det_temp, mod_det_u, mod_det_ff, mod_det_rr1, mod_det_nbrmar,mod_det_nbrmarlog, mod_det_full), modnames = c("null","OBS","HEURE","DATE","TEMP","U","FF","RR1","NBR_MAR", "NBR_MARlog","full"), c.hat = 3.69)

# NBR MAR log le remporte largmement

summary(mod_det_full)

mod_nbrmar_obs    <- pcount(~ NBR_MAR_log + OBS ~ 1, data = umfglu, mixture = "ZIP", K = 75)
mod_nbrmar_heure  <- pcount(~ NBR_MAR_log + HEURE ~ 1, data = umfglu, mixture = "ZIP", K = 75)
mod_nbrmar_date   <- pcount(~ NBR_MAR_log + DATE ~ 1, data = umfglu, mixture = "ZIP", K = 75)
mod_nbrmar_ff     <- pcount(~ NBR_MAR_log + FF ~ 1, data = umfglu, mixture = "ZIP", K = 75)
mod_nbrmar_rr1    <- pcount(~ NBR_MAR_log + RR1 ~ 1, data = umfglu, mixture = "ZIP", K = 75)
mod_nbrmarlin_date   <- pcount(~ NBR_MAR + DATE ~ 1, data = umfglu, mixture = "ZIP", K = 75)

AIC_detect_glu_bi <- aictab(list(mod_det_nbrmarlog, mod_nbrmar_obs, mod_nbrmar_heure, mod_nbrmar_date, mod_nbrmar_ff, mod_nbrmar_rr1,mod_nbrmarlin_date,fmfullsite_ZIP), modnames = c("NBR_MAR_mog","NBR_MAR_log+obs","NBR_MAR_log+heure", "NBR_MAR_log+date", "NBR_MAR_log+ff", "NBR_MAR_log+rr1", "NBR_MAR+date", "fmfullsite_ZIP"), c.hat = 2.98)

summary(fmfullsite_ZIP)

# la meilleure structure est NBR_MAR_log + DATE largement

########################## occupation ###############################

mod_ab_null<- pcount(~ NBR_MAR_log + DATE ~ 1, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_alti<- pcount(~ NBR_MAR_log + DATE ~ ALTI, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_marmax<- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX, data = umfglu, mixture = "ZIP", K = 75)

mod_ab_marmaxlog<- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log, data = umfglu, mixture = "ZIP", K = 75)

mod_ab_mar50<- pcount(~ NBR_MAR_log + DATE ~ MAR50, data = umfglu, mixture = "ZIP", K = 75)

mod_ab_mar50log<- pcount(~ NBR_MAR_log + DATE ~ MAR50log, data = umfglu, mixture = "ZIP", K = 75)

mod_ab_ouvrage<- pcount(~ NBR_MAR_log + DATE ~ OUVRAGE, data = umfglu, mixture = "ZIP", K = 75)

mod_ab_f100<- pcount(~ NBR_MAR_log + DATE ~ F100, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_dh500<- pcount(~ NBR_MAR_log + DATE ~ DH500, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_dr100<- pcount(~ NBR_MAR_log + DATE ~ DR100, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_dr500<- pcount(~ NBR_MAR_log + DATE ~ DR500, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_rive <- pcount(~ NBR_MAR_log + DATE ~ RIVE, data = umfglu, mixture = "ZIP", K = 75)

AIC_abond_glu <- aictab(list(mod_ab_null, mod_ab_alti, mod_ab_marmax, mod_ab_marmaxlog, mod_ab_mar50,mod_ab_mar50log, mod_ab_ouvrage, mod_ab_f100, mod_ab_dh500, mod_ab_dr100, mod_ab_dr500, mod_ab_rive,fmfullsite_ZIP), modnames = c("mod_ab_null", "mod_ab_alti", "mod_ab_marmax", "mod_ab_marmaxlog","mod_ab_mar50", "mod_ab_mar50log","mod_ab_ouvrage", "mod_ab_f100", "mod_ab_dh500", "mod_ab_dr100", "mod_ab_dr500", "mod_ab_rive", "fmfullsite"), c.hat = 2.98)

# le full et le mod_ab_marmaxlog
# modèle bi avec MAR_MAXlog

mod_ab_marmaxlog
mod_ab_mlogDH500 <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + DH500, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogALTI <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + ALTI, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogOUVRAGE <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + OUVRAGE, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogF100 <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + F100, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogDR100 <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + DR100, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogDR500 <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + DR500, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogRIVE <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + RIVE, data = umfglu, mixture = "ZIP", K = 75)
mod_ab_mlogALTIDH500 <- pcount(~ NBR_MAR_log + DATE ~ MAR_MAX_log + DH500 + ALTI, data = umfglu, mixture = "ZIP", K = 75)

AIC_abond_glu_bi <- aictab(list(mod_ab_marmaxlog,mod_ab_mlogDH500,mod_ab_mlogALTI,mod_ab_mlogOUVRAGE,mod_ab_mlogF100,mod_ab_mlogDR100,mod_ab_mlogDR500,mod_ab_mlogRIVE,fmfullsite_ZIP,mod_ab_mlogALTIDH500), modnames = c("MAR_MAX_log","MAR_MAX_log+DH500","MAR_MAX_log+ALTI","MAR_MAX_log+OUVRAGE","MAR_MAX_log+gF100","MAR_MAX_log+DR100","MAR_MAX_log+DR500","MAR_MAX_log+RIVE", "fmfullsite", "MAR_MAX_log+ALTI+DH500"), c.hat = 2.98)

# mod_ab_mlogDH500 est le mieux 
# mod_ab_mlogALTIDH500 encore mieux
cor(umfglu@siteCovs$DH500, umfglu@siteCovs$ALTI, use = "complete.obs")
summary(mod_ab_mlogALTIDH500)

mod_final_abond_glu <- mod_ab_mlogALTIDH500
  
gof_mod_final_abond_glu <- Nmix.gof.test(mod_final_abond_glu, nsim = 1000, lot.hist=F)

################################################################################
################# CALCUL de l'ABONDANCE (bootstrap) ############################
################################################################################

set.seed(123)

B <- 2000

mean_lambda <- numeric(B)
total_lambda <- numeric(B)

for (i in 1:B) {
  # simulation des coefficients d'abondance
  beta <- MASS::mvrnorm(
    1,
    mu = coef(mod_final_abond_glu, type = "state"),
    Sigma = vcov(mod_final_abond_glu, type = "state"))
  
  # simulation du coefficient de zero-inflation
  beta_psi <- MASS::mvrnorm(
    1,
    mu = coef(mod_final_abond_glu, type = "psi"),
    Sigma = vcov(mod_final_abond_glu, type = "psi"))
  
  X <- model.matrix(~ MAR_MAX_log + DH500 + ALTI, data = as.data.frame(siteCovs(umfglu)))
  
  lambda <- exp(X %*% beta)
  # probabilité zero-inflation (intercept seul si pas de covariable sur cette partie)
  psi_zero <- plogis(beta_psi)
  
  lambda_final <- (1 - psi_zero) * lambda
  
  mean_lambda[i] <- mean(lambda_final)
  total_lambda[i] <- sum(lambda_final)
}

IC_lambda_mean <- quantile(mean_lambda, c(0.025, 0.975), na.rm = TRUE)
IC_lambda_total <- quantile(total_lambda, c(0.025, 0.975), na.rm = TRUE)

mean(mean_lambda)   # reste identique, basé sur predict()
IC_lambda_mean

total_lambda_obs  # reste identique, basé sur predict()
IC_lambda_total
coef(mod_final_abond_glu)
# détection

set.seed(123)

B <- 2000

mean_p <- numeric(B)

for (i in 1:B) {
  
  beta <- MASS::mvrnorm(1,
                        mu = coef(mod_final_abond_glu, type = "det"),
                        Sigma = vcov(mod_final_abond_glu, type = "det"))
  
  X <- model.matrix(
    ~ NBR_MAR_log + DATE,
    data = as.data.frame(obsCovs(umfglu)))
  
  p <- plogis(X %*% beta)   # ici plogis reste correct, la détection est bien en logit
  
  mean_p[i] <- mean(p)
}

mean_p_obs <- mean(
  predict(mod_final_abond_glu, type = "det")$Predicted,
  na.rm = TRUE)

IC_p <- quantile(
  mean_p,
  c(0.025, 0.975),
  na.rm = TRUE)

mean_p_obs
IC_p


summary(mod_final_abond_glu)

effet_DATE <- coef(mod_final_abond_glu)["p(DATE)"] * diff(range(umfglu@obsCovs$DATE, na.rm = TRUE))
effet_NBRMAR <- coef(mod_final_abond_glu)["p(NBR_MAR_log)"] * diff(range(umfglu@obsCovs$NBR_MAR_log, na.rm = TRUE))

effet_DATE
effet_NBRMAR

range_DH500 <- diff(range(umfglu@siteCovs$DH500, na.rm = TRUE))
range_ALTI <- diff(range(umfglu@siteCovs$ALTI, na.rm = TRUE))
range_MARMAXlog <- diff(range(umfglu@siteCovs$MAR_MAX_log, na.rm = TRUE))

effet_DH500 <- coef(mod_final_abond_glu)["lam(DH500)"] * range_DH500
effet_ALTI <- coef(mod_final_abond_glu)["lam(ALTI)"] * range_ALTI
effet_MARMAXlog <- coef(mod_final_abond_glu)["lam(MAR_MAX_log)"] * range_MARMAXlog

effet_DH500
effet_ALTI
effet_MARMAXlog



# export de la table d'aic 
AIC_distri
AIC_detect_glu
AIC_detect_glu_bi
AIC_abond_glu
AIC_abond_glu_bi

liste_aic <- list(
  "Distribution (ZIP, P et BN)" = AIC_distri, 
  "Structure détection univariée" = AIC_detect_glu,
  "Structure détection bivariée" = AIC_detect_glu_bi,
  "Structure abondance univariée" = AIC_abond_glu,
  "Structure abondance bivariée" = AIC_abond_glu_bi)

tableau_complet <- imap_dfr(liste_aic, ~ as.data.frame(.x) %>% 
                              mutate(Modele_set = .y)) %>%
  mutate(AICc_final    = ifelse(is.na(c_hat), AICc, QAICc),
    Delta_final   = ifelse(is.na(c_hat), Delta_AICc, Delta_QAICc),
    Poids_final   = ifelse(is.na(c_hat), AICcWt, QAICcWt),
    Critere       = ifelse(is.na(c_hat), "AICc", "QAICc")) %>%
  select(Modele_set, Modnames, K, Critere, AICc_final, Delta_final, Poids_final, c_hat) %>%
  mutate(across(c(AICc_final, Delta_final, Poids_final), ~ round(.x, 2)))

write.csv(tableau_complet, "annexe_AIC_N_MIXTURE.csv", row.names = FALSE)
