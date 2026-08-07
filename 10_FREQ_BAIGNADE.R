################################################################################
############### Script 09 : impact fréquentation estivale ######################
################################################################################

# I. SETUP ####

# //// CHARGEMENT DES PACKAGES /// ####

# library(pacman)

pacman::p_load(readr, ggplot2, tidyverse, dplyr, gt, patchwork, sf, viridis, shadowtext, grid,png, ggtext,ggspatial, prettymapr, leaflet, terra, data.table)

# //// CHEMIN D'ACCÈS /// ####

getwd() # si on ouvre avec le r.proj on devrait avoir le bon chemin sinon on copie colle le résultat

# Chemin <- ".../.../"
# setwd(Chemin)

data <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp") %>% st_transform(crs = 2154) 
freq <- sf::st_read("IMPORT/BAIGNADE/FREQ_glu.shp") %>% st_transform(crs = 2154)
freq <- freq %>% select(ID_SITE,NBR_MAR_P4, Nbr_adu_P4, Nbr_sub_P4, Ind_rep_P4, Remarque_P) %>% rename(Remarq_P4 = Remarque_P)

# colonne avec tot sonneur 

freq <- freq %>% mutate(SONN_P4 = coalesce(Nbr_adu_P4, 0) + coalesce(Nbr_sub_P4, 0))

# join data à freq par ID_SITE
freq <- freq %>% left_join(
    data %>% st_drop_geometry(),
    by = "ID_SITE") %>% select(ID_SITE, SONN_P1, SONN_P2, SONN_P3, SONN_P4,NBR_MAR_P1, NBR_MAR_P2, NBR_MAR_P3, NBR_MAR_P4)

freq <- freq %>% mutate(
    DIFF_P3P4 = SONN_P4 - SONN_P3,
    DIFF_txt = if_else(DIFF_P3P4 > 0,
      paste0("+", DIFF_P3P4),
      as.character(DIFF_P3P4)))

#freq %>% st_write("EXPORT/FREQ_GLU.shp",delete_dsn = TRUE)

sum(freq$SONN_P4)
sum(freq$SONN_P3)
sum(freq$SONN_P2)
sum(freq$SONN_P1)
# taux d'occupation
freq %>% st_drop_geometry() %>% summarise(P1_occup = 100 * mean(SONN_P1 > 0, na.rm = TRUE))
freq %>% st_drop_geometry() %>% summarise(P2_occup = 100 * mean(SONN_P2 > 0, na.rm = TRUE))
freq %>% st_drop_geometry() %>% summarise(P3_occup = 100 * mean(SONN_P3 > 0, na.rm = TRUE))
freq %>% st_drop_geometry() %>% summarise(P4_occup = 100 * mean(SONN_P4 > 0, na.rm = TRUE))

table(freq$SONN_P3>0,freq$SONN_P4>0)

# accès baignade 
baignade <- sf::st_read("IMPORT/BAIGNADE/ACCES.shp") %>% st_transform(crs = 2154) 
str(baignade)
summary(baignade)

# distance minimale entre chaque site et un accès de baignade pour 2026
dist <- st_distance(freq, baignade)
freq$dist_baignade <- apply(dist, 1, min) |> as.numeric()

# distance minimale entre chaque site et un accès de baignade pour 2020 et 2024

jeg <- read_excel("IMPORT/2024/distance_acces_presence.xlsx") %>% filter(annee == "2020") %>% filter(site > 11 & site < 75.2)

peg <- read_excel("IMPORT/2024/distance_acces_presence.xlsx") %>% filter(annee == "2024") %>% filter(site > 11 & site < 75.2)

#jeg_2020 <- jeg %>%
#  transmute(
#    site = site,
#    distance = distance,
#    presence = presence,
#    annee = 2020)

#peg_2024 <- peg %>%
#  transmute(
#    site = site,
#    distance = distance,
#    presence = presence,
#    annee = 2024)

freq_2026 <- freq %>%
  filter(dist_baignade < 3000) %>%
  st_drop_geometry() %>%
  transmute(
    site = ID_SITE,
    distance = dist_baignade,
    presence_01 = if_else(SONN_P1 > 0, 1, 0),
    presence_02 = if_else(SONN_P2 > 0, 1, 0),
    presence_03 = if_else(SONN_P3 > 0, 1, 0),
    presence_04 = if_else(SONN_P4 > 0, 1, 0),
    annee = 2026)

#donnees_baignade <- bind_rows(
#  jega_2020,
#  peg_2024,
#  freq_2026)

freq_2026_long <- freq_2026 %>%
  pivot_longer(cols = starts_with("presence_"),
    names_to = "passage",
    values_to = "presence") %>%
  mutate(passage = dplyr::recode(
      passage,
      "presence_01" = "P1",
      "presence_02" = "P2",
      "presence_03" = "P3",
      "presence_04" = "P4"),
    presence = factor(presence,
      levels = c(0, 1),
      labels = c("Absence", "Présence")))

# test pour voir diff entre occup et pas occup
library(rstatix)
freq_2026_long %>%
  group_by(passage) %>%
  wilcox_test(distance ~ presence) %>%
  adjust_pvalue(method = "BH")

pval_df <- tibble(
  passage = c("P1", "P2", "P3", "P4"),
  group1 = c("Absence", "Absence", "Absence", "Absence"),
  group2 = c("Présence", "Présence", "Présence", "Présence"),
  p.adj = c(0.203, 0.815, 0.815, 0.815)) %>%
  mutate(y.position = c(190, 190, 190, 190),
    label = case_when(
      p.adj < 0.001 ~ paste0("p = ", format.pval(p.adj, digits = 3), " ***"),
      p.adj < 0.01  ~ paste0("p = ", format.pval(p.adj, digits = 3), " **"),
      p.adj < 0.05  ~ paste0("p = ", format.pval(p.adj, digits = 3), " *"),
      TRUE          ~ paste0("p = ", format.pval(p.adj, digits = 3), " ns")))

ggplot(freq_2026_long,
       aes(x = presence,
           y = distance,
           fill = presence)) +
  geom_boxplot(width = 0.55,
    outlier.shape = NA,
    colour = "grey30",
    linewidth = 0.7) +
  geom_jitter(width = 0.12,
    alpha = 0.55,
    size = 1.8,
    colour = "grey20") +
  stat_pvalue_manual(
    pval_df,
    label = "label",
    tip.length = 0.01,
    size = 4) + 
  facet_wrap(~passage, nrow = 1) +
  scale_fill_manual(values = c(
    "Absence" = "#D9EAF7",
    "Présence" = "#BFDDA8")) +
  labs(x = "",
    y = "Distance minimale à un accès de baignade (m)") +
  theme_classic(base_size = 14) +
   theme(legend.position = "none",
    strip.background = element_rect(fill = "grey92", colour = NA),
    strip.text = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.spacing = unit(1.2, "lines"))
