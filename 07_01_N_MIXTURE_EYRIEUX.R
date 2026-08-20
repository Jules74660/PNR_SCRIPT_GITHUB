# ce script a pour objectif d'obtenir la probabilité d'occupation des sites sur l'Eyrieux

pacman::p_load(readr, ggplot2, tidyverse, dplyr, unmarked, AICcmodavg, car, corrplot)

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

?corrplot()
# TEMP, DATE U et H sont encore une fois corrélé ce qui nécessite de faire un choix qui va être la DATE

EYR_obs_covs <- list(
  OBS     = dataeyr %>% st_drop_geometry() %>% dplyr::select(OBS_P1, OBS_P2, OBS_P3),
  DATE    = dataeyr %>% st_drop_geometry() %>% dplyr::select(DATE_P1, DATE_P2, DATE_P3),
  HEURE   = dataeyr %>% st_drop_geometry() %>% dplyr::select(HEURE_P1, HEURE_P2, HEURE_P3),
  NBR_MAR = dataeyr %>% st_drop_geometry() %>% dplyr::select(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
  FF      = dataeyr %>% st_drop_geometry() %>% dplyr::select(FF_P1, FF_P2, FF_P3),
  RR1     = dataeyr %>% st_drop_geometry() %>% dplyr::select(RR1_P1, RR1_P2, RR1_P3))

################################################################################
####################### Création des modèles N-Mixture #########################
################################################################################

y.count <- dataeyr %>% 
  st_drop_geometry() %>% 
  dplyr::select(SONN_P1, SONN_P2, SONN_P3) %>%
  as.matrix()

umfeyr <- unmarkedFramePCount(y = y.count,
                              siteCovs = EYR_site_covs,
                              obsCovs = EYR_obs_covs)

umfeyr@obsCovs$NBR_MAR_log <- log1p(umfeyr@obsCovs$NBR_MAR)
umfeyr@siteCovs$MAR_MAX_log <- log1p(umfeyr@siteCovs$MAR_MAX)
umfeyr@siteCovs$MAR50log <- log1p(umfeyr@siteCovs$MAR50)

################################################################################
######################## Choix de la distribution et K #########################
################################################################################

fm_P   <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "P", K = 75)
fm_NB  <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfeyr, mixture = "NB",  K = 75)
fm_ZIP <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfeyr, mixture = "ZIP", K = 75)
fm_ZIP <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "ZIP", K = 75)

pred_P   <- predict(fm_P, type = "state")
pred_NB  <- predict(fm_NB, type = "state")
pred_ZIP <- predict(fm_ZIP, type = "state")

sum(pred_P$Predicted, na.rm = TRUE)
sum(pred_NB$Predicted, na.rm = TRUE)
sum(pred_ZIP$Predicted, na.rm = TRUE)

fm_P@opt$convergence
fm_NB@opt$convergence
fm_ZIP@opt$convergence

AIC_distri_eyr <- aictab(list(fm_P, fm_NB, fm_ZIP), modnames = c("Poisson", "NegBin", "ZIP"))
# NegBin domine largement mais très instable et donne des estimations à 1882 individus
# ZIP est le deuxième avec des estimations plus cohérentes 

K_vals <- c(30, 50, 75, 100, 150, 200, 250, 300)

resultats_NB_eyr <- lapply(K_vals, function(k) {
  fm <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "NB", K = k)
  pred <- predict(fm, type = "state")
  data.frame(K = k, 
             lam = coef(fm, type="state")[1],
             total = sum(pred$Predicted, na.rm = TRUE),
             AICc = AICc(fm),
             convergence = fm@opt$convergence)
})

do.call(rbind, resultats_NB_eyr)

resultats_ZIP_eyr <- lapply(K_vals, function(k) {
  fm <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "ZIP", K = k)
  pred <- predict(fm, type = "state")
  data.frame(K = k, total = sum(pred$Predicted, na.rm = TRUE), AICc = AICc(fm))
})
do.call(rbind, resultats_ZIP_eyr)

resultats_P_eyr <- lapply(K_vals, function(k) {
  fm <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "P", K = k)
  pred <- predict(fm, type = "state")
  data.frame(K = k, total = sum(pred$Predicted, na.rm = TRUE), AICc = AICc(fm))
})
do.call(rbind, resultats_P_eyr)

fm_P_final   <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "P", K = 75)
fm_ZIP_final <- pcount(~ RR1 + NBR_MAR_log ~ ALTI + MAR_MAX_log, data = umfeyr, mixture = "ZIP", K = 75)

aictab(list(fm_P_final, fm_ZIP_final), modnames = c("Poisson", "ZIP"))
# ZIP meilleure même si estimation moins bien je trouve (plus de 1000 individus) contre 700 à la loi de poisson


################################################################################
######################## ajustement Qaic avec chat  ############################
################################################################################

fmfullobs_ZIPeyr <- pcount(~ NBR_MAR_log + OBS + HEURE + DATE + FF + RR1 ~ 1, data = umfglu, mixture = "ZIP", K = 75)

fmfullsite_ZIPeyr <- pcount(~ NBR_MAR_log ~ MAR_MAX_log + ALTI + MAR50 + OUVRAGE + F100 + DH500 + DR100 + DR500 + RIVE, data = umfeyr, mixture = "ZIP", K = 75)

summary(fmfullsite_ZIPeyr)

obs.boot.fulleyrobs <- Nmix.gof.test(fmfullobs_ZIPeyr, nsim = 100, lot.hist=F)
# 3.72
obs.boot.fulleyrsite <- Nmix.gof.test(fmfullsite_ZIPeyr, nsim = 100, lot.hist=F)
# 2.89
################################################################################
########################## Intégration des covs  ###############################
################################################################################


########################## détection ###############################


mod_det_null    <- pcount(~1 ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_obs     <- pcount(~OBS ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_heure   <- pcount(~HEURE ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_date    <- pcount(~DATE ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_ff      <- pcount(~FF ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_rr1     <- pcount(~RR1 ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_nbrmar  <- pcount(~NBR_MAR ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_nbrmarlog <- pcount(~NBR_MAR_log ~1, data = umfeyr, mixture = "ZIP", K = 75)
mod_det_full    <- pcount(~OBS + HEURE + DATE + FF + RR1 + NBR_MAR ~1, 
                          data = umfeyr, mixture = "ZIP", K = 75)

AIC_detect_eyr <- aictab(list(mod_det_null, mod_det_obs, mod_det_heure, mod_det_date, mod_det_ff, 
            mod_det_rr1, mod_det_nbrmar, mod_det_nbrmarlog, mod_det_full),
       modnames = c("null","OBS","HEURE","DATE","FF","RR1","NBR_MAR","NBR_MARlog","full"), c.hat = 3.72)

# NBR_MAR_log l'emporte largement 

summary(mod_det_full)s

mod_nbrmar_obs <- pcount(~ NBR_MAR_log + OBS ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmar_heure <- pcount(~ NBR_MAR_log + HEURE ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmar_date <- pcount(~ NBR_MAR_log + DATE ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmar_ff <- pcount(~ NBR_MAR_log + FF ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmar_rr1 <- pcount(~ NBR_MAR_log + RR1 ~ 1, data = umfeyr, mixture = "ZIP", K = 75)

mod_nbrmarlin_obs <- pcount(~ NBR_MAR + OBS ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmarlin_heure <- pcount(~ NBR_MAR + HEURE ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmarlin_date <- pcount(~ NBR_MAR + DATE ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmarlin_ff <- pcount(~ NBR_MAR + FF ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_nbrmarlin_rr1 <- pcount(~ NBR_MAR + RR1 ~ 1, data = umfeyr, mixture = "ZIP", K = 75)

AIC_detect_eyr_bi <- aictab(list(mod_det_nbrmarlog, mod_nbrmar_obs, mod_nbrmar_heure, mod_nbrmar_date, mod_nbrmar_ff, mod_nbrmar_rr1,mod_det_full, mod_nbrmarlin_obs,mod_nbrmarlin_heure, mod_nbrmarlin_date,mod_nbrmarlin_ff,mod_nbrmarlin_rr1), modnames = c("NBR_MAR_log","NBR_MAR_log+obs","NBR_MAR_log+heure", "NBR_MAR_log+date", "NBR_MAR_log+ff", "NBR_MAR_log+rr1","mod_det_full", "NBR_MAR+obs","NBR_MAR+heure", "NBR_MAR+date","NBR_MAR+ff","NBR_MAR+rr1"), c.hat = 3.72)

# OK c'est NBR_MAR_log seul qui l'emporte hehe

########################## abondance ###############################

mod_ab_null <- pcount(~ NBR_MAR_log ~ 1, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_alti <- pcount(~ NBR_MAR_log ~ ALTI, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_marmax <- pcount(~ NBR_MAR_log ~ MAR_MAX, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_marmaxlog <- pcount(~ NBR_MAR_log ~ MAR_MAX_log, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_mar50<- pcount(~ NBR_MAR_log ~ MAR50, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_mar50log<- pcount(~ NBR_MAR_log ~ MAR50log, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_ouvrage <- pcount(~ NBR_MAR_log ~ OUVRAGE, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_f100 <- pcount(~ NBR_MAR_log ~ F100, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dh500<- pcount(~ NBR_MAR_log ~ DH500, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100<- pcount(~ NBR_MAR_log ~ DR100, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr500<- pcount(~ NBR_MAR_log ~ DR500, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_rive <- pcount(~ NBR_MAR_log ~ RIVE, data = umfeyr, mixture = "ZIP", K = 75)

AIC_abond_eyr <- aictab(list(mod_ab_null,mod_ab_alti,mod_ab_marmax,mod_ab_marmaxlog,mod_ab_mar50,mod_ab_mar50log,mod_ab_ouvrage,mod_ab_f100,mod_ab_dh500,mod_ab_dr100,mod_ab_dr500,mod_ab_rive,fmfullsite_ZIPeyr), modnames = c("NULL","ALTI","MAR_MAX","MAR_MAX_log","MAR50","MAR50_log","OUVRAGE","F100","DH500","DR100","DR500","RIVE","fmfullsite"), c.hat = 2.89)

# DR100 le remporte même devant MARMAXlog quelle folie 

mod_ab_dr100alti <- pcount(~ NBR_MAR_log ~ DR100 + ALTI, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100marmax <- pcount(~ NBR_MAR_log ~ DR100 + MAR_MAX, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100marmaxlog <- pcount(~ NBR_MAR_log ~ DR100 + MAR_MAX_log, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100mar50 <- pcount(~ NBR_MAR_log ~ DR100 + MAR50, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100mar50log <- pcount(~ NBR_MAR_log ~ DR100 + MAR50log, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100ouvrage <- pcount(~ NBR_MAR_log ~ DR100 + OUVRAGE, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100f100 <- pcount(~ NBR_MAR_log ~ DR100 + F100, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100dh500 <- pcount(~ NBR_MAR_log ~ DR100 + DH500, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100dr500 <- pcount(~ NBR_MAR_log ~ DR100 + DR500, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100rive <- pcount(~ NBR_MAR_log ~ DR100 + RIVE, data = umfeyr, mixture = "ZIP", K = 75)
mod_ab_dr100altimarmaxlog <- pcount(~ NBR_MAR_log ~ DR100 + ALTI + MAR_MAX_log, data = umfeyr, mixture = "ZIP", K = 75)

AIC_abond_eyr_bi <- aictab(list(mod_ab_dr100, mod_ab_dr100alti, mod_ab_dr100marmax,mod_ab_dr100marmaxlog,mod_ab_dr100mar50, mod_ab_dr100mar50log, mod_ab_dr100ouvrage, mod_ab_dr100f100, mod_ab_dr100dh500, mod_ab_dr100dr500, mod_ab_dr100rive, fmfullsite_ZIPeyr), modnames = c("DR100", "DR100+alti", "DR100+marmax","DR100+marmaxlog","DR100+mar50", "DR100+mar50log", "DR100+ouvrage", "DR100+f100", "DR100+dh500", "DR100+dr500", "DR100+rive", "fmfullsite"), c.hat = 2.89)

cor(umfeyr@siteCovs$ALTI, umfeyr@siteCovs$MAR_MAX_log, use = "complete.obs")
cor(umfeyr@siteCovs$DR100, umfeyr@siteCovs$ALTI, use = "complete.obs")

AIC_abond_eyr_tri <- aictab(list(mod_ab_dr100, mod_ab_dr100alti, mod_ab_dr100marmaxlog, mod_ab_dr100altimarmaxlog,fmfullsite_ZIPeyr),modnames = c("mod_ab_dr100", "dr100+alti", "dr100+marmaxlog", "dr100+alti+marmaxlog", "fmfullsite_ZIPeyr"),c.hat = 2.89)

summary(mod_ab_dr100altimarmaxlog)

mod_ab_dr100altimarmaxlog_P <- pcount(~ NBR_MAR_log ~ DR100 + ALTI + MAR_MAX_log, data = umfeyr, mixture = "P", K = 75)

predfinal <- predict(mod_ab_dr100altimarmaxlog, type = "state")
sum(predfinal$Predicted, na.rm = TRUE)
predmoinsfinal <- predict(mod_ab_dr100alti, type = "state")
sum(predmoinsfinal$Predicted, na.rm = TRUE)
predencoremoinsfinal <- predict(mod_ab_dr100marmaxlog, type = "state")
sum(predencoremoinsfinal$Predicted, na.rm = TRUE)
test <- predict(mod_ab_dr100altimarmaxlog_P, type = "state")
sum(test$Predicted, na.rm = TRUE)

# estimation bizarre mais okk pas mal on reste dans le clous (pas trop) bref belle conclusion

mod_final_abond_eyr <- mod_ab_dr100altimarmaxlog

################################################################################
################# CALCUL de l'ABONDANCE (bootstrap) ############################
################################################################################

set.seed(123)

B <- 2000

mean_lambda <- numeric(B)
total_lambda <- numeric(B)

pred_abond_eyr <- predict(mod_final_abond_eyr, type = "state")
mean_lambda_obs <- mean(pred_abond_eyr$Predicted, na.rm = TRUE)
total_lambda_obs <- sum(pred_abond_eyr$Predicted, na.rm = TRUE)

for (i in 1:B) {
  # simulation des coefficients d'abondance
  beta <- MASS::mvrnorm(
    1,
    mu = coef(mod_final_abond_eyr, type = "state"),
    Sigma = vcov(mod_final_abond_eyr, type = "state"))
  beta_psi <- MASS::mvrnorm(
    1,
    mu = coef(mod_final_abond_eyr, type = "psi"),
    Sigma = vcov(mod_final_abond_eyr, type = "psi"))
  
  X <- model.matrix(~ DR100 + ALTI + MAR_MAX_log, data = as.data.frame(siteCovs(umfeyr)))
  
  lambda <- exp(X %*% beta)
  psi_zero <- plogis(beta_psi)
  
  lambda_final <- (1 - psi_zero) * lambda
  
  mean_lambda[i] <- mean(lambda_final)
  total_lambda[i] <- sum(lambda_final)
}

IC_lambda_mean <- quantile(mean_lambda, c(0.025, 0.975), na.rm = TRUE)
IC_lambda_total <- quantile(total_lambda, c(0.025, 0.975), na.rm = TRUE)

mean_lambda_obs   # reste identique, basé sur predict()
IC_lambda_mean

total_lambda_obs  # reste identique, basé sur predict()
IC_lambda_total
coef(mod_final_abond_eyr)

# détect
set.seed(123)

B <- 2000

mean_p <- numeric(B)

for (i in 1:B) {
  
  beta <- MASS::mvrnorm(1,
                        mu = coef(mod_final_abond_eyr, type = "det"),
                        Sigma = vcov(mod_final_abond_eyr, type = "det"))
  
  X <- model.matrix(
    ~ NBR_MAR_log,
    data = as.data.frame(obsCovs(umfeyr)))
  
  p <- plogis(X %*% beta)
  
  mean_p[i] <- mean(p)
}

mean_p_obs <- mean(
  predict(mod_final_abond_eyr, type = "det")$Predicted,
  na.rm = TRUE)

IC_p <- quantile(
  mean_p,
  c(0.025, 0.975),
  na.rm = TRUE)

mean_p_obs
IC_p

summary(mod_final_abond_eyr)

obs.boot.mod_final_abond_eyr <- Nmix.gof.test(mod_final_abond_eyr, nsim = 1000, lot.hist=F)

summary(mod_final_abond_eyr)


# export de la table d'aic 
AIC_distri_eyr
AIC_detect_eyr
AIC_detect_eyr_bi
AIC_abond_eyr
AIC_abond_eyr_bi
AIC_abond_eyr_tri

liste_aic_eyr <- list(
  "Distribution (ZIP, P et BN)" = AIC_distri_eyr, 
  "Structure détection univariée" = AIC_detect_eyr,
  "Structure détection bivariée" = AIC_detect_eyr_bi,
  "Structure abondance univariée" = AIC_abond_eyr,
  "Structure abondance bivariée" = AIC_abond_eyr_bi,
  "Structure abondance trivariée" = AIC_abond_eyr_tri)

tableau_complet <- imap_dfr(liste_aic_eyr, ~ as.data.frame(.x) %>% 
                              mutate(Modele_set = .y)) %>%
  mutate(AICc_final    = ifelse(is.na(c_hat), AICc, QAICc),
         Delta_final   = ifelse(is.na(c_hat), Delta_AICc, Delta_QAICc),
         Poids_final   = ifelse(is.na(c_hat), AICcWt, QAICcWt),
         Critere       = ifelse(is.na(c_hat), "AICc", "QAICc")) %>%
  select(Modele_set, Modnames, K, Critere, AICc_final, Delta_final, Poids_final, c_hat) %>%
  mutate(across(c(AICc_final, Delta_final, Poids_final), ~ round(.x, 2)))

write.csv(tableau_complet, "annexe_AIC_N_MIXTURE_eyr.csv", row.names = FALSE)
