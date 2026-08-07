# ce script a pour objectif de traiter le jeu de données consititué pour quantifier la variabilité inter-opérateur

################################################################################
#################### Script 01 : Import and formatting #########################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)
pacman::p_load(tidyverse, readxl, ggplot2, readr)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

varobs <- sf::st_read("IMPORT/VAR_OBS.shp") %>% select(ID_SITE, ANAIS_MARE, JUL_MARE, ANAIS_SON, JUL_SON)

varobs <- varobs %>% mutate(across(c(ID_SITE),as.numeric))
varobs <- varobs %>% arrange(ID_SITE) # dans l'odre des sites

# enlever site 295.10 et 195.20 car pas fait 

varobs <- varobs %>% filter(ID_SITE != 295.10 & ID_SITE != 195.20)

# Il reste 48 données 
# différence entre 

varobs <- varobs %>% mutate(diff_mare = ANAIS_MARE - JUL_MARE, diff_son = ANAIS_SON - JUL_SON)

str(varobs)

# TEST de la différence 

varobs$diff_mare <- varobs$ANAIS_MARE - varobs$JUL_MARE
varobs$moy_mare <- (varobs$ANAIS_MARE + varobs$JUL_MARE) / 2
varobs$diff_son <- varobs$ANAIS_SON - varobs$JUL_SON
varobs$moy_son <- (varobs$ANAIS_SON + varobs$JUL_SON) / 2

shapiro.test(varobs$diff_mare)
shapiro.test(varobs$diff_son) # rejette l'hypthèse de normalité : test non paramétrique
# test de Wilcoxon pour les données appariées et non normales 

wilcox.test(varobs$ANAIS_MARE, varobs$JUL_MARE, paired = TRUE)
wilcox.test(varobs$ANAIS_SON, varobs$JUL_SON, paired = TRUE)

sum(!is.na(varobs$ANAIS_SON) & !is.na(varobs$JUL_SON))
mean(varobs$ANAIS_SON - varobs$JUL_SON, na.rm = TRUE)
median(varobs$ANAIS_SON - varobs$JUL_SON, na.rm = TRUE)
table(sign(varobs$ANAIS_SON - varobs$JUL_SON))
table(sign(varobs$JUL_SON - varobs$ANAIS_SON))

median(varobs$diff_mare, na.rm = TRUE)
mean(varobs$diff_mare, na.rm = TRUE)
mean(varobs$diff_son, na.rm = TRUE)

biais <- mean(varobs$diff_mare, na.rm = TRUE)
sd_diff <- sd(varobs$diff_mare, na.rm = TRUE)

ggplot(varobs, aes(x = moy_mare, y = diff_mare)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = biais, color = "blue") +
  geom_hline(yintercept = biais + 1.96 * sd_diff, color = "red", linetype = "dashed") +
  geom_hline(yintercept = biais - 1.96 * sd_diff, color = "red", linetype = "dashed") +
  labs(x = "Moyenne des deux opérateurs", y = "Différence (Anaïs - Jul)",
       title = "Bland-Altman : nombre de mares") +
  theme_classic()

ggplot(varobs, aes(x = ANAIS_MARE, y = JUL_MARE)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  labs(x = "Nombre de mares (Anaïs)", y = "Nombre de mares (Jul)") +
  theme_classic()

library(ggrepel)

var_son <- ggplot(varobs, aes(x = ANAIS_SON, y = JUL_SON)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(
    data = subset(varobs, abs(diff_son) >= 3),
    aes(label = ID_SITE),
    size = 3,
    box.padding = 0.5,
    max.overlaps = Inf) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linetype = "dashed") +
  labs(x = "Nombre d'individus comptés (OBS2)",
    y = "Nombre d'individus comptés (OBS1)") +
  theme_classic()

# Graphique Bland-Altman  

biais <- mean(varobs$diff_son, na.rm = TRUE)
sd_diff <- sd(varobs$diff_son, na.rm = TRUE)

bland_plot <- ggplot(varobs, aes(moy_son, diff_son)) +
  geom_hline(yintercept = biais,colour = "#2C7FB8",linewidth = 0.8) +
  geom_hline(yintercept = biais + 1.96*sd_diff, linetype = 2,colour = "red") +
  geom_hline(yintercept = biais - 1.96*sd_diff,linetype = 2,colour = "red") +
  geom_text_repel(
    data = subset(varobs, abs(diff_son) >= 3),
    aes(label = ID_SITE),
    size = 3,
    box.padding = 0.5,
    max.overlaps = Inf) +
  geom_point(alpha = 0.7, size = 2) +
  theme_classic() +
  labs(x = "Moyenne (OBS1-OBS2) ",
       y = "Différence (OBS2−OBS1)")

var_son + bland_plot


install.packages("irr")
library(irr) 

icc_son <- varobs %>%
  sf::st_drop_geometry() %>%
  select(ANAIS_SON, JUL_SON)

icc_mar <- varobs %>%
  sf::st_drop_geometry() %>%
  select(ANAIS_MARE, JUL_MARE)

icc(
  icc_son,
  model = "twoway",
  type = "agreement",
  unit = "single"
)

icc(
  icc_mar,
  model = "twoway",
  type = "agreement",
  unit = "single"
)

sum(complete.cases(varobs$ANAIS_SON, varobs$JUL_SON))

limiterep <- sf::st_read("IMPORT/LIMITES_REP_2026.shp") %>% select(ID_SITE)

