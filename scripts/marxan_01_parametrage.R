################################################################################
#### PARAMÉTRAGE                                                            ####
################################################################################

# 1. Choix de la zone d'étude ####
nisl   <- "GLP"
projt  <- "current"
supfam <- "all"
subfam <- switch(supfam, all = "ALL", Majoidea = "MAJ", Muricoidea = "MUR")

# 2. Sous-échantillons ####
my_subsets <- list(
  ALL = c(
    "Amphithrax hemphilli",
    "Macrocoeloma nodipes",
    "Mithraculus coryphe",
    "Mithraculus forceps",
    "Mithrax pleuracanthus",
    "Omalacantha bicornuta",
    "Stenorhynchus seticornis",
    "Teleophrys ruber",
    "Claremontiella nodulosa",
    "Coralliophila galea",
    "Coralliophila salebrosa",
    "Favartia alveata",
    "Favartia varimutabilis",
    "Phyllonotus pomum",
    "Siratus consuelae",
    "Stramonita rustica",
    "Trachypollia didyma",
    "Vasula deltoidea",
    NULL
  ) %>% sort(),
  MAJ = c(
    "Amphithrax hemphilli",
    "Macrocoeloma nodipes",
    "Mithraculus coryphe",
    "Mithraculus forceps",
    "Mithrax pleuracanthus",
    "Omalacantha bicornuta",
    "Stenorhynchus seticornis",
    "Teleophrys ruber",
    NULL
  ),
  MUR = c(
    "Claremontiella nodulosa",
    "Coralliophila galea",
    "Coralliophila salebrosa",
    "Favartia alveata",
    "Favartia varimutabilis",
    "Phyllonotus pomum",
    "Siratus consuelae",
    "Stramonita rustica",
    "Trachypollia didyma",
    "Vasula deltoidea",
    NULL
  ) %>% sort()
)

# 3. Choix des quantité d'unités de conservation à préserver ####
target <- c(
  "Amphithrax hemphilli",
  "Macrocoeloma nodipes",
  "Mithraculus coryphe",
  "Mithraculus forceps",
  "Mithrax pleuracanthus",
  "Omalacantha bicornuta",
  "Stenorhynchus seticornis",
  "Teleophrys ruber",
  "Claremontiella nodulosa",
  "Coralliophila galea",
  "Coralliophila salebrosa",
  "Favartia alveata",
  "Favartia varimutabilis",
  "Phyllonotus pomum",
  "Siratus consuelae",
  "Stramonita rustica",
  "Trachypollia didyma",
  "Vasula deltoidea",
  NULL
) %>%
  sort()

# Ou quantités identiques pour toutes les espèces
# À mettre en commentaire si des espèces sont sélectionnées dans
# la liste ci-dessus.
target <- 1

# Proportion des cellules des espèces à conserver
# target_prop <- rep(0.85, length(target))

# 4. Choix des pénalités pour la non-présence des espèces dans ####
# la réserve finale

spf <- c(
  "Amphithrax hemphilli",
  "Macrocoeloma nodipes",
  "Mithraculus coryphe",
  "Mithraculus forceps",
  "Mithrax pleuracanthus",
  "Omalacantha bicornuta",
  "Stenorhynchus seticornis",
  "Teleophrys ruber",
  "Claremontiella nodulosa",
  "Coralliophila galea",
  "Coralliophila salebrosa",
  "Favartia alveata",
  "Favartia varimutabilis",
  "Phyllonotus pomum",
  "Siratus consuelae",
  "Stramonita rustica",
  "Trachypollia didyma",
  "Vasula deltoidea",
  NULL
)

# Ou quantités identiques pour toutes les espèces
# À mettre en commentaire si des espèces sont sélectionnées dans
# la liste ci-dessus.
spf <- 1000

# 5. Choix des coûts de mailles à partir des pressions anthropiques ####
# cost_threshold <- 0.85
cost_threshold <- "none"
cost <- 1

# 6. Prise en compte ou non du statut des mailles au sein du réseau AMP ####
status <- "NA"
# status <- "IN"
# status <- "OUT"

# 7. Choix du nombre de répétitions du modèle ####
# repetitions <- "auto"
repetitions <- 100000

##########################
# LANCEMENT DU MODÈLE ####
##########################
source(here("scripts", "marxan_02_routine.R"))
source(here("scripts", "marxan_03_visualisation.R"))
