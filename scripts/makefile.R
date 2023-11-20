# makefile

source(here::here("scripts", "boot.R"))

# classification complète des espèces
source(here::here("scripts", "classification.R"))

# boostrap des richesses spécifiques
source(here::here("scripts", "bootstrap_species_richness.R"))

# Routine Marxan
source(here::here("scripts", "marxan_01_parametrage.R"))
source(here::here("scripts", "marxan_02_routine.R"))
