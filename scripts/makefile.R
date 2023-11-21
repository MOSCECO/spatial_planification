# makefile

source(here::here("scripts", "boot.R"))

# classification complète des espèces
source(here::here("scripts", "classification.R"))

# sauvegarde des rasters de richesses spécifiques
source(here::here("scripts", "rasters_species_richness.R"))

# boostrap des richesses spécifiques
source(here::here("scripts", "bootstrap_species_richness.R"))
source(here::here("scripts", "bootstrap_species_richness_figures.R"))

# Routine Marxan
source(here::here("scripts", "marxan_01_parametrage.R"))
source(here::here("scripts", "marxan_02_routine.R"))
source(here::here("scripts", "marxan_03_visualisation.R"))
