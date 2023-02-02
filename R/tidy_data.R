# Importation, nettoyage et remaniement des données

## Packages --------
SciViews::R

## Importation ---------
fish <- read("data/raw/Fish.csv")

# Changement du noms des variables ------
fish <- janitor::clean_names(fish)

fish <- srename(fish,
  standard_length = length1,
  fork_length = length2,
  total_length = length3)

#  Remmaniement des données ----
skimr::skim(fish)

fish <- smutate(fish,
  species = as.factor(species))

summary(fish)

# Elimination d'un poisson d'une masse de 0
fish <- sfilter(fish, weight > 0)

# Ajout des labels -----
## Les longueur sont en centimètre et la masse en grammes
# TODO

# Sauvegarde du tableaux
write$rds(fish, "data/fish.rds", compress = "xz")

