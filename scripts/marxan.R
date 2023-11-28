# makefile marxan cluster meSU

source(here::here("scripts", "boot.R"))
source(here("scripts", "sensibility_index.R"))
source(here("scripts", "marxan_01_parametrage.R"))
##########################
# LANCEMENT DU MODÈLE ####
##########################
source(here("scripts", "marxan_02_routine.R"))
source(here("scripts", "marxan_03_visualisation.R"))
