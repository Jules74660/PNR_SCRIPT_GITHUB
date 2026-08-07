# ce script a pour objectif de faire les statistiques descriptives du jeu de données formatées dans le script précédent : 01_IMPORT_FROM

################################################################################
################## Script 02 : Statistiques descriptives #######################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)

pacman::p_load(readr, ggplot2, tidyverse, dplyr, gt, patchwork, sf, viridis, shadowtext, grid,png, ggtext,ggspatial, prettymapr, leaflet)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

# /// IMPORT DES DONNEES /// ####

data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp")

# MAR_MAX 

#data <- data %>%
#  mutate(
#    MAR_MAX = pmax(NBR_MAR_P1,NBR_MAR_P2,NBR_MAR_P3, na.rm = TRUE), 
#    MAR_MAX = ifelse(is.infinite(MAR_MAX), NA_real_, MAR_MAX))

# NBR_SONNEUR_MAX

#data <- data %>% mutate(NBR_SON_TOT_P1 = Nbr_adu_P1 + Nbr_sub_P1, NBR_SON_TOT_P2 = Nbr_adu_P2 #+ Nbr_sub_P2, NBR_SON_TOT_P3 = Nbr_adu_P3 + Nbr_sub_P3)

# /// Effectifs maximums comptés et effectifs au fil des passages /// ####
# tableau : 

dataglu <- data %>% filter(RIVIERE == "Glueyre")
dataeyr <- data %>% filter(RIVIERE == "Eyrieux")

sum(dataglu$Nbr_adu_P1, na.rm = TRUE) # P1 : 349
sum(dataglu$Nbr_sub_P1, na.rm = TRUE) # P1 : 349 + 6 = 355
sum(dataglu$SONN_P1, na.rm = TRUE)
sum(dataglu$Nbr_adu_P2, na.rm = TRUE) # P2 : 315
sum(dataglu$Nbr_sub_P2, na.rm = TRUE) # P2 : 315 + 12 = 327
sum(dataglu$SONN_P2, na.rm = TRUE)
sum(dataglu$Nbr_adu_P3, na.rm = TRUE) # P3 : 355
sum(dataglu$Nbr_sub_P3, na.rm = TRUE) # P3 : 355 + 34 = 389
sum(dataglu$SONN_P3, na.rm = TRUE)

sum(dataeyr$Nbr_adu_P1, na.rm = TRUE) # P1 : 233
sum(dataeyr$Nbr_sub_P1, na.rm = TRUE) # P1 : 233 + 10 = 243
sum(dataeyr$SONN_P1, na.rm = TRUE)
sum(dataeyr$Nbr_adu_P2, na.rm = TRUE) # P2 : 201
sum(dataeyr$Nbr_sub_P2, na.rm = TRUE) # P2 : 201 + 5 = 206
sum(dataeyr$SONN_P2, na.rm = TRUE)
sum(dataeyr$Nbr_adu_P3, na.rm = TRUE) # P3 : 201
sum(dataeyr$Nbr_sub_P3, na.rm = TRUE) # P3 : 201 + 7 = 208
sum(dataeyr$SONN_P3, na.rm = TRUE)

sum(data$EFF_MAX, na.rm = TRUE)
sum(dataeyr$EFF_MAX, na.rm = TRUE)

sum(dataglu$NBR_MAR_P1, na.rm = TRUE) # 4646
sum(dataglu$NBR_MAR_P2, na.rm = TRUE) # 3753
sum(dataglu$NBR_MAR_P3, na.rm = TRUE) # 2608

sum(dataeyr$NBR_MAR_P1, na.rm = TRUE) # 4940
sum(dataeyr$NBR_MAR_P2, na.rm = TRUE) # 5921
sum(dataeyr$NBR_MAR_P3, na.rm = TRUE) # 1575

sum(data$NBR_MAR_P1, data$NBR_MAR_P2, data$NBR_MAR_P3, na.rm = TRUE)

# calculer le pourcentage de complexes et de mares solitaires 
dataglu %>% summarise(P1_complex = 100 * mean(NBR_MAR_P1 > 1, na.rm = TRUE),
            P2_complex = 100 * mean(NBR_MAR_P2 > 1, na.rm = TRUE),  
            P3_complex = 100 * mean(NBR_MAR_P3 > 1, na.rm = TRUE))

dataeyr %>% summarise(P1_complex = 100 * mean(NBR_MAR_P1 > 1, na.rm = TRUE),
                      P2_complex = 100 * mean(NBR_MAR_P2 > 1, na.rm = TRUE),  
                      P3_complex = 100 * mean(NBR_MAR_P3 > 1, na.rm = TRUE))

# Tableau du pourcentage de sites occupées en fonction des passages sur l'Eyrieux et la Gluèyre 

dataglu %>% summarise(P1_occup = 100 * mean(SONN_P1 > 0, na.rm = TRUE),
            P2_occup = 100 * mean(SONN_P2[NBR_MAR_P2 > 0] > 0, na.rm = TRUE),  
            P3_occup = 100 * mean(SONN_P3[NBR_MAR_P3 > 0] > 0, na.rm = TRUE))

dataeyr %>% summarise(P1_occup = 100 * mean(SONN_P1 > 0, na.rm = TRUE),
                      P2_occup = 100 * mean(SONN_P2[NBR_MAR_P2 > 0] > 0, na.rm = TRUE),  
                      P3_occup = 100 * mean(SONN_P3[NBR_MAR_P3 > 0] > 0, na.rm = TRUE))

# combien de sites avec mares > 0

sum(dataglu$NBR_MAR_P1 > 0, na.rm = TRUE)
sum(dataglu$NBR_MAR_P2 > 0, na.rm = TRUE)
sum(dataglu$NBR_MAR_P3 > 0, na.rm = TRUE)

sum(data$NBR_MAR_P1 > 0, na.rm = TRUE)
sum(data$EFF_MAX, na.rm = TRUE)

sum(dataeyr$NBR_MAR_P1 > 0, na.rm = TRUE)
sum(dataeyr$NBR_MAR_P2 > 0, na.rm = TRUE)
sum(dataeyr$NBR_MAR_P3 > 0, na.rm = TRUE)

sum(data$NBR_MAR_P1, na.rm = TRUE)
sum(data$NBR_MAR_P3, na.rm = TRUE)
sum(data$NBR_MAR_P2, na.rm = TRUE)

9586 + 4183 + 9674

# Tableau du pourcentage du nombre de sites occupées en fonction des passages sur l'Eyrieux et la Gluèyre 

data %>%
  group_by(RIVIERE) %>%
  summarise(
    n_sites = n(),
    P1 = 100 * mean(NBR_SON_TOT_P1 > 0, na.rm = TRUE),
    P2 = 100 * mean(NBR_SON_TOT_P2 > 0, na.rm = TRUE),
    P3 = 100 * mean(NBR_SON_TOT_P3 > 0, na.rm = TRUE))

# Plot des mares en eau sur l'eyrieux et sur la GLuèyre 
# NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3 boxplot 

# Glueyre ####

data_long_glu <- dataglu %>%
  st_drop_geometry() %>% 
  pivot_longer(cols = c(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
               names_to = "Passage",
               values_to = "Nb_mares") %>%
  mutate(Passage = dplyr::recode(Passage,
                          "NBR_MAR_P1" = "Passage 1",
                          "NBR_MAR_P2" = "Passage 2",
                          "NBR_MAR_P3" = "Passage 3"))

stat_glu <- data_long_glu %>%
  wilcox_test(Nb_mares ~ Passage, paired = TRUE, p.adjust.method = "BH") %>%
  filter(group1 == "Passage 1")  # on ne garde que les comparaisons vs Passage 1

get_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns")}

stat_glu <- stat_glu %>% mutate(stars = get_stars(p.adj))
n_labels_glu <- n_labels_glu %>%
  left_join(stat_glu %>% select(Passage = group2, stars), by = "Passage") %>%
  mutate(stars = ifelse(is.na(stars), "", stars),  # Passage 1 = référence, pas d'étoile
         label = ifelse(pct_perte == 0,
                        paste0("Total = ", total_mares),
                        paste0("Total = ", total_mares, " (-", pct_perte, "%) ", stars)))


plot_glu <- ggplot(data_long_glu, aes(x = Passage, y = Nb_mares, fill = Passage)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white", width = 0.5) +
  geom_text(data = n_labels_glu, 
            aes(x = Passage, y = y_pos, label = label),
            inherit.aes = FALSE, fontface = "bold", size = 5, lineheight = 0.9) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "", y = "Nombre de mares en eau") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"))

# Eyrieux ####

data_long_eyr <- dataeyr %>%
  st_drop_geometry() %>% 
  pivot_longer(cols = c(NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3),
               names_to = "Passage",
               values_to = "Nb_mares") %>%
  mutate(Passage = dplyr::recode(Passage,
                          "NBR_MAR_P1" = "Passage 1",
                          "NBR_MAR_P2" = "Passage 2",
                          "NBR_MAR_P3" = "Passage 3"))

stat_eyr <- data_long_eyr %>%
  wilcox_test(Nb_mares ~ Passage, paired = TRUE, p.adjust.method = "BH") %>%
  filter(group1 == "Passage 1")

get_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns")}

stat_eyr <- stat_eyr %>% mutate(stars = get_stars(p.adj))

n_labels_eyr <- n_labels_eyr %>%
  left_join(stat_eyr %>% select(Passage = group2, stars), by = "Passage") %>%
  mutate(stars = ifelse(is.na(stars), "", stars),
         label = ifelse(pct_var == 0,                                    # ← pct_var, pas pct_perte
                        paste0("Total = ", total_mares),
                        paste0("Total = ", total_mares, " (", 
                               ifelse(pct_var > 0, "+", ""), pct_var, "%) ", stars)))

plot_eyr <- ggplot(data_long_eyr, aes(x = Passage, y = Nb_mares, fill = Passage)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white", width = 0.5) +
  geom_text(data = n_labels_eyr, 
            aes(x = Passage, y = y_pos, label = label),
            inherit.aes = FALSE, fontface = "bold", size = 5, lineheight = 0.9) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "", y = "Nombre de mares en eau") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"))

par(mfrow = c(1, 2))
plot_glu + plot_eyr

# test 
find("friedman.test")
dim(mat_glu)
dim(mat_eyr)
colnames(mat_glu)
colnames(mat_eyr)
mat_glu <- as.matrix(na.omit(st_drop_geometry(dataglu)[, c("NBR_MAR_P1", "NBR_MAR_P2", "NBR_MAR_P3")]))
friedman.test(mat_glu)

mat_eyr <- as.matrix(na.omit(st_drop_geometry(dataeyr)[, c("NBR_MAR_P1", "NBR_MAR_P2", "NBR_MAR_P3")]))
friedman.test(mat_eyr)

pairwise.wilcox.test(data_long_glu$Nb_mares, data_long_glu$Passage, paired = TRUE, p.adjust.method = "BH")

pairwise.wilcox.test(data_long_eyr$Nb_mares, data_long_eyr$Passage, paired = TRUE, p.adjust.method = "BH")
