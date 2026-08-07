
PELO <- sf::st_read("EXPORT/DONNEES_SONNEURS_2026_JR.shp") %>% dplyr::select(ID_SITE,Faune_P1, Faune_P2, Faune_P3,RIVIERE, SONN_P1)

# /// Pelophylax /// ####

# Sur Faune_1, Faune_2, Faune_3 extraire les "P" dans chaque ligne pour créer une colonne de présence absence de Pelophylax 

PELO$PELO <- PELO %>% 
  select(Faune_P1, Faune_P2, Faune_P3) %>%
  apply(1, function(x) any(x == "P", na.rm = TRUE)) %>%
  as.integer()

# Pourcentage de sites occupés basé sur les sites du premier passage 
# sur combien de sites

PELOglu <- PELO %>% filter(RIVIERE == "Glueyre")
PELOeyr <- PELO %>% filter(RIVIERE == "Eyrieux")

sum(PELOglu$PELO, na.rm = TRUE)
sum(PELOeyr$PELO, na.rm = TRUE)

PELOglu %>% summarise(P1_occup = 100 * mean(PELO > 0, na.rm = TRUE))
PELOeyr %>% summarise(P1_occup = 100 * mean(PELO > 0, na.rm = TRUE))

# nombre par passage 
PELO <- PELO %>%
  mutate(PELO_P1 = as.integer(Faune_P1 == "P"),
    PELO_P2 = as.integer(Faune_P2 == "P"),
    PELO_P3 = as.integer(Faune_P3 == "P"))

PELOglu <- PELOglu %>%
  mutate(PELO_P1 = as.integer(Faune_P1 == "P"),
         PELO_P2 = as.integer(Faune_P2 == "P"),
         PELO_P3 = as.integer(Faune_P3 == "P"))


PELOeyr <- PELOeyr %>%
  mutate(PELO_P1 = as.integer(Faune_P1 == "P"),
         PELO_P2 = as.integer(Faune_P2 == "P"),
         PELO_P3 = as.integer(Faune_P3 == "P"))

sum(PELOeyr$PELO_P1, na.rm = TRUE)

