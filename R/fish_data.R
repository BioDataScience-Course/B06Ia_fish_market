SciViews::R

# Les données sont disponibles avec des données supplémentaires via
# le lien <https://www.kaggle.com/aungpyaeap/fish-market?select=Fish.csv>
fish <- read("data/raw/Fish.csv")
table(fish$Species)

