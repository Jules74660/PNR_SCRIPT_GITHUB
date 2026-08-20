
METEO_2020_2024 <- read_delim("IMPORT/01__COVARIABLES__/METEO_FRANCE/Q_07_previous-1950-2024_RR-T-Vent.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% dplyr::filter(NUM_POSTE %in% c("07064001")) %>% dplyr::select(NUM_POSTE, AAAAMMJJ, RR, TM)

METEO_FRANCE_2025_2026 <- read_delim("IMPORT/01__COVARIABLES__/METEO_FRANCE/H_07_latest-2025-2026.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% dplyr::select(NUM_POSTE, AAAAMMJJ, RR, TM) %>% dplyr::filter(NUM_POSTE %in% c("07064001")) 

# en gros j'aimerais faire des graphiques de la température et de la précipiation sur les années 2020, 2024 et 2026 mais c'est sur deux fichiers différents, comment peut tu m'aider pour faire des grahiques qui ont du sens avec des maximales ou je ne sais quoi par ce que dans météo france il y a TM : moyenne quotidienne des températures horaires sous abri et TX température maximale sous abri et TN température minimale sous abri et RR quantité de précipitation tombée en 24 heures (de 06 h FU le jour J à 06 h FU le jour J+1). La valeur relevée à J+1 est affectée au jour J. J'aimerais donc savoir comment représenter pour voir un peu les différences météorologiques et climatiques et en gros on peut afficher que les périodes mars à aout pour chaque graphique. donc en fait j'ai deux rivières et la j'ai choisi la station pour l'Eyrieux avec 07064001 et j'aimerais donc avoir un nombre réduit de graphiques avec genre 1 pour la précipitation sur les 3 années, 1 pour la température sur les 3 années, ce qui ferait quatre par rivière (l'autre station c'est : 07286002)

library(tidyverse)
library(lubridate)

# =========================================================
# 1. FONCTION DE CHARGEMENT + FUSION D'UNE STATION
#    (fusionne le fichier historique "previous" et le fichier
#    récent "latest", qui utilisent tous deux AAAAMMJJ/RR/TM)
# =========================================================
charger_station <- function(id_poste, nom_riviere) {
  
  # --- fichier historique : déjà au pas journalier (AAAAMMJJ) ---
  ancien <- read_delim(
    "IMPORT/01__COVARIABLES__/METEO_FRANCE/Q_07_previous-1950-2024_RR-T-Vent.csv",
    delim = ";", escape_double = FALSE, trim_ws = TRUE
  ) %>%
    filter(NUM_POSTE == id_poste) %>%
    select(NUM_POSTE, AAAAMMJJ, RR, TM, TN, TX) %>%
    mutate(date = ymd(AAAAMMJJ)) %>%
    select(NUM_POSTE, date, RR, TM, TN, TX)
  
  # --- fichier récent : pas HORAIRE (AAAAMMJJHH) -> à agréger en journalier ---
  # Colonnes du fichier horaire : RR1 = précip. de la dernière heure (pas RR),
  # T = température instantanée en °C (pas TM). On vérifie leur présence.
  recent_brut <- read_delim(
    "IMPORT/01__COVARIABLES__/METEO_FRANCE/H_07_latest-2025-2026.csv",
    delim = ";", escape_double = FALSE, trim_ws = TRUE
  ) %>%
    filter(NUM_POSTE == id_poste)
  
  # sécurité : si jamais le nom de la colonne de température n'est pas "T",
  # ce message te dira comment l'appeler à la place
  stopifnot(
    "Colonne RR1 introuvable dans le fichier horaire" = "RR1" %in% names(recent_brut),
    "Colonne T introuvable (regarde `names(recent_brut)` pour le vrai nom, ex: T, TD...)" =
      "T" %in% names(recent_brut)
  )
  
  recent_horaire <- recent_brut %>%
    select(NUM_POSTE, AAAAMMJJHH, RR1, T) %>%
    mutate(
      datetime = ymd_h(as.character(AAAAMMJJHH)),
      date     = as_date(datetime)
    )
  
  recent <- recent_horaire %>%
    group_by(NUM_POSTE, date) %>%
    summarise(
      RR = sum(RR1, na.rm = TRUE),  # cumul journalier des précip. horaires (RR1)
      TM = mean(T, na.rm = TRUE),   # moyenne journalière de la température horaire (T)
      TN = min(T, na.rm = TRUE),    # min journalier -> équivalent de TN
      TX = max(T, na.rm = TRUE),    # max journalier -> équivalent de TX
      .groups = "drop"
    )
  
  bind_rows(ancien, recent) %>%
    mutate(
      annee = year(date),
      mois  = month(date)
    ) %>%
    # on ne garde que les années comparées et la période mars-août
    filter(annee %in% c(2020, 2024, 2026), mois %in% 3:8) %>%
    # axe x "calendaire" commun (année fictive) pour superposer les années
    mutate(jour_mois = as.Date(format(date, "2024-%m-%d"))) %>%
    mutate(riviere = nom_riviere)
}

# =========================================================
# 2. CHARGEMENT DES DEUX STATIONS -> à renommer selon tes rivières
# =========================================================
eyrieux <- charger_station("07064001", "Eyrieux")
riviere2 <- charger_station("07286002", "Rivière 2")
meteo <- charger_station("07096001", "Station 07096001")


donnees <- bind_rows(eyrieux, riviere2)
donnees <- charger_station("07096001", "Station 07096001")

# =========================================================
# 3. PRÉCIPITATIONS CUMULÉES SUR LA SAISON (mars -> août)
#    -> montre directement les années sèches vs humides
# =========================================================
donnees_cumul <- donnees %>%
  arrange(riviere, annee, date) %>%
  group_by(riviere, annee) %>%
  mutate(RR_cumul = cumsum(replace_na(RR, 0))) %>%
  ungroup()

theme_set(theme_minimal(base_size = 13))

# --- thème "classe" avec fond gris clair ---
theme_rapport <- theme(
  plot.background   = element_rect(fill = "grey97", color = NA),
  panel.background  = element_rect(fill = "grey93", color = NA),
  panel.grid.major  = element_line(color = "white", linewidth = 0.6),
  panel.grid.minor  = element_blank(),
  plot.title        = element_text(face = "bold", size = 15, margin = margin(b = 10)),
  legend.position    = "top",
  legend.title       = element_text(face = "bold")
)

# --- palette de couleurs par année (à ajuster si tu changes les années) ---
palette_annees <- c(
  "2020" = "#8AA29E",  # vert-gris sourd
  "2024" = "#3D5A80",  # bleu profond
  "2026" = "#C9552F"   # terracotta
)

# --- repères de mois en français, sur l'axe x commun (année fictive 2024) ---
reperes_mois <- as.Date(c("2024-03-01","2024-04-01","2024-05-01",
                          "2024-06-01","2024-07-01","2024-08-01"))
labels_mois <- c("Mars","Avr.","Mai","Juin","Juil.","Août")

for (r in unique(donnees_cumul$riviere)) {
  
  p_precip <- donnees_cumul %>%
    filter(riviere == r) %>%
    ggplot(aes(x = jour_mois, y = RR_cumul, color = factor(annee))) +
    geom_line(linewidth = 1.1) +
    scale_x_date(breaks = reperes_mois, labels = labels_mois) +
    scale_color_manual(values = palette_annees) +
    labs(x = NULL, y = "Précipitations cumulées (mm)", color = "Année") +
    theme_rapport
  ggsave(paste0("precipitation_", r, ".png"), p_precip, width = 9, height = 5, dpi = 300)
  print(p_precip)
}

# =========================================================
# 4. TEMPÉRATURE MOYENNE QUOTIDIENNE (mars -> août)
#    lissage léger pour rendre les courbes comparables entre années
# =========================================================
for (r in unique(donnees$riviere)) {
  
  p_temp <- donnees %>%
    filter(riviere == r) %>%
    ggplot(aes(x = jour_mois, y = TM, color = factor(annee))) +
    geom_line(linewidth = 0.4, alpha = 0.35) + 
    geom_smooth(se = FALSE, span = 0.15, linewidth = 1.2) + 
    scale_x_date(breaks = reperes_mois, labels = labels_mois) +
    scale_color_manual(values = palette_annees) +
    labs(x = NULL, y = "Température moyenne (°C)", color = "Année") +
    theme_rapport
  
  ggsave(paste0("temperature_", r, ".png"), p_temp, width = 9, height = 5, dpi = 300)
  print(p_temp)

}

library(patchwork)

liste_precip <- list()

for (r in unique(donnees_cumul$riviere)) {
  
  p_precip <- donnees_cumul %>%
    filter(riviere == r) %>%
    ggplot(aes(x = jour_mois, y = RR_cumul, color = factor(annee))) +
    geom_line(linewidth = 1.1) +
    scale_x_date(breaks = reperes_mois, labels = labels_mois) +
    scale_color_manual(values = palette_annees) +
    labs(x = NULL,
      y = "Précipitations cumulées (mm)",
      color = "Année"
    ) +
    theme_rapport
  
  liste_precip[[r]] <- p_precip
}

# Affichage côte à côte
(liste_precip[[1]] | liste_precip[[2]])

# Sauvegarde
ggsave("precipitations_2rivieres.png",
       (liste_precip[[1]] | liste_precip[[2]]),
       width = 14, height = 5, dpi = 300)

liste_temp <- list()

for (r in unique(donnees$riviere)) {
  
  p_temp <- donnees %>%
    filter(riviere == r) %>%
    ggplot(aes(x = jour_mois, y = TM, color = factor(annee))) +
    geom_line(linewidth = 0.4, alpha = 0.35) +
    geom_smooth(se = FALSE, span = 0.15, linewidth = 1.2) +
    scale_x_date(breaks = reperes_mois, labels = labels_mois) +
    scale_color_manual(values = palette_annees) +
    labs(
      x = NULL,
      y = "Température moyenne (°C)",
      color = "Année"
    ) +
    theme_rapport
  
  liste_temp[[r]] <- p_temp
}

# Affichage côte à côte
(liste_temp[[1]] | liste_temp[[2]])

# Sauvegarde
ggsave("temperatures_2rivieres.png",
       (liste_temp[[1]] | liste_temp[[2]]),
       width = 14, height = 5, dpi = 300)

# =========================================================
# NOTE : si tes fichiers sources contiennent aussi TX (max) et TN (min),
# ajoute-les au select() de charger_station() pour tracer un ruban
# TN-TX autour de TM avec geom_ribbon() : ça donne une lecture encore
# plus parlante de l'amplitude thermique journalière par année.
# =========================================================

donnees_cumul <- donnees %>%
  arrange(annee, date) %>%
  group_by(annee) %>%
  mutate(RR_cumul = cumsum(replace_na(RR, 0))) %>%
  ungroup()

ggplot(donnees_cumul,
       aes(x = jour_mois,
           y = RR_cumul,
           color = factor(annee))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = palette_annees) +
  scale_x_date(breaks = reperes_mois,
               labels = labels_mois) +
  labs(
    x = NULL,
    y = "Précipitations cumulées (mm)",
    color = "Année") +
  theme_rapport

p_precip

p_temp <- ggplot(donnees,
                 aes(x = jour_mois,
                     y = TM,
                     color = factor(annee))) +
  geom_line(alpha = 0.35, linewidth = 0.4) +
  geom_smooth(se = FALSE,
              span = 0.15,
              linewidth = 1.2) +
  scale_color_manual(values = palette_annees) +
  scale_x_date(breaks = reperes_mois,
               labels = labels_mois) +
  labs(
    x = NULL,
    y = "Température (°C)",
    color = "Année"
  ) +
  theme_rapport

p_temp


ggsave("precipitations_2020_2024_2026.png",
       p_precip,
       width = 9,
       height = 5,
       dpi = 300)

ggsave("temperatures_2020_2024_2026.png",
       p_temp,
       width = 9,
       height = 5,
       dpi = 300)

# GLU 

GLU <- charger_station("07096001", "Gluiras")


GLU_cumul <- GLU %>%
  arrange(annee, date) %>%
  group_by(annee) %>%
  mutate(RR_cumul = cumsum(replace_na(RR, 0))) %>%
  ungroup()

p_precip_GLU <- ggplot(GLU_cumul,
                       aes(x = jour_mois,
                           y = RR_cumul,
                           color = factor(annee))) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = palette_annees) +
  scale_x_date(breaks = reperes_mois,
               labels = labels_mois) +
  labs(
    x = NULL,
    y = "Précipitations cumulées (mm)",
    color = "Année"
  ) +
  theme_rapport

p_temp_GLU <- ggplot(GLU,
                     aes(x = jour_mois,
                         y = TM,
                         color = factor(annee))) +
  geom_line(alpha = 0.35, linewidth = 0.4) +
  geom_smooth(se = FALSE,
              span = 0.15,
              linewidth = 1.2) +
  scale_color_manual(values = palette_annees) +
  scale_x_date(breaks = reperes_mois,
               labels = labels_mois) +
  labs(
    x = NULL,
    y = "Température moyenne (°C)",
    color = "Année"
  ) +
  theme_rapport

p_temp_GLU + p_precip_GLU
