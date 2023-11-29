################################################################################
#### PARAMÉTRAGE                                                            ####
################################################################################

# Initialisation
source(here::here("scripts", "boot.R"))
source(here("scripts", "sensibility_index.R"))

# 1. Choix des conditions initiales ####
nisl     <- "MTQ"
projt    <- "current"
supfam   <- "all"
subfam   <- switch(supfam, all = "ALL", Majoidea = "MAJ", Muricoidea = "MUR")
scenario <- "opti" # "pessi"
status   <- "none"

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
my_subsets <- my_subsets[subfam]

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

# Choix des coûts de mailles à partir :

# - De rien, modèle "nul"
# spatRast_cost_list <- NULL

spatRast_cost_list <- list(

  # - De la qualité de hotspot de diversité
  # Objectif : Plus une zone ré-échantillonnée est riche en espèce, plus il est
  # couteux de ne pas l'inclure dans la réserve.
  # Concrètement, j'augmente le coût de chaque unité de planification
  # proportionnellement à sa richesse spécifique.
  hotspot = rl$current$q0.5,

  # - De la variation de richesses spécifiques
  # Coefficient de variation
  # Plus une zone perd des espèces (coefficient de variation qui tend vers -1)
  # plus elle est sensible aux changements climatiques à venir et plus il est
  # nécessaire de la conserver, donc plus le coût de l'écartement de cette
  # maille dans la réserve est élevé.
  # percvar = rv[[scenario]]$q0.5/2 + 0.5,

  # - De la diversité beta temporelle
  # betadiv = rb[[scenario]]$beta.sor,

  NULL
)
# - De la variation de richesses spécifiques (pourcentage de variation > 0.5)
# cost_threshold <- 0.85

# Choix du nombre de répétitions du modèle ####
# repetitions <- "auto"

# nreps_total <- 230
nreps_total <- 100000
nreps_ncpus <- split(
  1:nreps_total, ceiling((1:nreps_total)/(detectCores() - 1))
)

source(here("scripts", "marxan_02_routine2.R"))
source(here("scripts", "marxan_03_visualisation.R"))
