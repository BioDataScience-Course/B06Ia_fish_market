# Importation, nettoyage et remaniement des données
# Note: vous ne devez pas exécuter ce script ni le modifier : l'importation a
# déjà été réalisée pour vous. Il est fourni à titre indicatif.

# Configuration de l'environnement
SciViews::R()

# Importation (les données issues de Kaggle sont déjà dans le dossier data/raw)
# voir https://www.kaggle.com/datasets/vipullrathod/fish-market/data
fish <- read("data/raw/fish.csv")

# Changement du noms des variables et élimination d'attributs inutiles
# Note: le code suivant utilise une nouvelle syntaxe qui remplace le pipe
# par des "bullet-points" .= et qui utilise l'évaluation standard dans les
# fonctions "speedy". Ce code devrait être compréhensible, même pour ceux qui
# sont habitué à l'ancienne syntaxe ou au {tidyverse}.
{
  .= fish
  .= janitor::clean_names(.)
  .= srename(., standard_length = 'length1',
                fork_length     = 'length2',
                total_length    = 'length3')
  .= smutate(., species = as.factor(.$species))
  .= setattr(., 'spec', NULL)
  .= setattr(., 'problems', NULL)
} -> fish
# L'équivalent en tidyverse s'écrit:
#fish <- fish %>%
#  janitor::clean_names() %>%
#  rename(standard_length = length1,
#         fork_length     = length2,
#         total_length    = length3) %>%
#  mutate(species = as.factor(species)) %>%
#  setattr('spec', NULL) %>%
#  setattr('problems', NULL)

skimr::skim(fish)

# Elimination d'un poisson d'une masse de 0g
fish <- sfilter(fish, weight > 0)

# Ajout des labels
# Les longueurs sont en centimètres et la masse en grammes
# Les longueurs sont mesurées du museau jusque :
# - l'extrémité de la nageoire caudale pour la longeur totale
# - la fin des rayons médians de la caudale pour la longueur à la fourche
# - le début de la caudale pour la longueur standard (exclut donc la caudale)
fish <- labelise(fish,
  label = list(
    species         = "Espèce",
    weight          = "Masse",
    standard_length = "Longueur standard",
    fork_length     = "Longueur à la fourche",
    total_length    = "Longueur totale",
    height          = "Hauteur",
    width           = "Largeur"),
  units = list(
    weight          = "g",
    standard_length = "cm",
    fork_length     = "cm",
    total_length    = "cm",
    height          = "cm",
    width           = "cm")
)

# Sauvegarde du jeu de données et nettoyage de l'environnement
write$rds(fish, "data/fish.rds", compress = "xz")
rm(fish, .)
