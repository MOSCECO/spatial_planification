# Cours ENSTA 2023
# Broussin Joséphine, Maniel Grégoire

# Introduction à Marxan
# Planification spatiale pour la conservation en Mer Méditerrannée

# mer <- "Adriatique"    # 2728  cellules
# mer <- "Egee"          # 2804  "
# mer <- "Gabes"         # 2486  "
# mer <- "Iberie"        # 2537  "
# mer <- "Ligure"        # 2560  "
# mer <- "Mediterrannee" # 44819 "

e <- ext_seas[[mer]]
zone <- switch(
  mer, 
  Adriatique   = "ADR", 
  Egee         = "EGE", 
  Gabes        = "GAB", 
  Iberie       = "IBE", 
  Ligure       = "LIG", 
  Mediterrannee = "MED"
)

# species
sp <- list.files(
  here("data", "analysis"), pattern = "species", full.names = T
) %>% rast() %>% terra::crop(., e)
# classification
cl <- list.files(
  here("data", "analysis"), pattern = "classification", full.names = T
) %>% read.csv()
cl$Species <- cl$Species %>% 
  gsub(" ", "_", .)
cl$SpeciesID <- 1:nrow(cl)
# pressures
pr <- list.files(
  here("data", "analysis"), pattern = "pressure", full.names = T
) %>% rast() %>% terra::crop(., e)
# protected areas
pa <- list.files(
  here("data", "analysis"), pattern = "mpas.+\\.tif$", full.names = T
) %>% rast() %>% terra::crop(., e)

# Passage de probabilité de présence à présence-absence
pat <- 500 # PARAMETRE 01 ---- pat = presence-absence threshold
sp_pa <- sp
values(sp_pa) <- ifelse(values(sp_pa) > pat, 1, 0)
names(sp_pa) <- gsub("_Current", "", names(sp_pa))
sp_pa <- terra::subset(sp_pa, names(as.data.frame(sp_pa) %>% colSums()))
print(
  paste(
    "Le jeu de données biologique initial contient", 
    length(names(sp_pa)), 
    "espèces."
  )
)

# Sous-échantillons
ALL_SPE <- cl$Species
TELEOST <- cl[cl$Class == "Teleostei", "Species"]
NOTELEO <- cl[cl$Class != "Teleostei", "Species"]
my_subsets <- list(
  ALL_SPE = ALL_SPE, 
  TELEOST = TELEOST, 
  NOTELEO = NOTELEO
)

# Sous-ensemble de données
sp_subs <- sapply(
  my_subsets, 
  \(my_sub) terra::subset(sp_pa, my_sub), 
  USE.NAMES = T, 
  simplify = F
)

# Noms des espèces dans les sous-ensembles
sp_names <- sapply(
  my_subsets, 
  \(my_sub) names(terra::subset(sp_pa, my_sub)), 
  USE.NAMES = T, 
  simplify = F
)

# Carte de richesse spécifique
sp_sr <- sapply(
  sp_subs, 
  \(r) terra::app(r, sum), 
  USE.NAMES = T
)

# 2. Choix des paramètres de Marxan ----
# Paramètes du modèle (à modifier manuellement dans input.dat)
# Nombre de répétitions : 
# PARAMETRE 03----
repetitions <- as.data.frame(sp) %>% 
  nrow() %>% 
  plyr::round_any(1000, f = ceiling)
# Conservation Features (éléments de conservation)
# Paramètres à adapter ----
# the amount of the conservation feature to be included within 
# the reserve network
# PARAMETRE fixé----
target <- c(
  # Non téléostéens
  # "Balaenoptera_physalus",   # Cétacé
  # "Chelonia_mydas",          # Reptile
  # "Dermochelys_coriacea",    # Reptile
  # "Globicephala_melas",      # Cétacé
  # "Pelagia_noctiluca",       # Cnidaire
  # "Physeter_macrocephalus",  # Cétacé
  # "Pinna_nobilis",           # Bivalve
  "Posidonia_oceanica",      # Phanérogame
  # "Stenella_coeruleoalba",   # Cétacé
  # "Tursiops_truncatus",      # Cétacé
  # "Ziphius_cavirostris",     # Cétacé
  "Zostera_marina",          # Phanérogame
  # Téléostéens
  # * : espèce commerciale
  # $ : espèce commerciale avec contrôle strict
  # ¤ : espèce emblématique
  # "Boops_boops",             # Bogue*
  # "Engraulis_encrasicolus",  # Anchois commun*
  # "Hippocampus_guttulatus",  # Hippocampe moucheté¤
  # "Hippocampus_hippocampus", # Hippocampe commun¤
  # "Merluccius_merluccius",   # Merlu commun*
  # "Mullus_barbatus",         # Rouget de vase*
  # "Mullus_surmuletus",       # Rouget de roche*
  # "Pagellus_erythrinus",     # Pageot commun*
  # "Sardina_pilchardus",      # Sardine commune*
  # "Sardinella_aurita",       # Sardinelle ronde*
  # "Solea_solea",             # Sole commune*
  # "Sprattus_sprattus",       # Sprat*
  # "Thunnus_thynnus",         # Thon rouge$
  # "Trachurus_trachurus",      # Chinchard*
  NULL
)
target <- 1
# Proportion des cellules des espèces à conserver
target_prop <- c(
  0.80, # Posidonia oceanica
  0.80  # Zostera marina
)

# Traduction en nombre de cellules à intégrer dans la réserve finale
target_cell <- if(!is.double(target)) {
  mapply(
    \(spe, pro) {
      m  <- sp_pa[[which(grepl(spe, names(sp)))]]
      current <- table(as.data.frame(m))[2] %>% as.numeric()
      protect <- ceiling(table(as.data.frame(m))[2]*pro) %>% as.numeric()
      paste(
        "L'espèce", 
        sub("_", " ", spe), 
        "est présente dans", 
        current, 
        "unités de conservation."
      ) %>% print()
      paste(
        protect, "unités de conservations seront sélectionnées pour conserver", 
        (pro*100) %>% paste0("%"), 
        "de cette distribution."
      ) %>% print()
      return(protect)
    },
    target, 
    target_prop, 
    SIMPLIFY  = F, 
    USE.NAMES = T
  )
}

# Abréviation dans le nom du modèle
TGT <- if (!is.double(target)) { "YES" } else { "NO" }

# Attribution des bonus/malus aux éléments de conservation
# SPF : Species Penalty Factor / CPF : Conservation Penalty Factor
# PARAMETRE 04 ----
spf <- c(
  # Non téléostéens
  # "Balaenoptera_physalus",   # Cétacé
  "Chelonia_mydas",          # Reptile
  "Dermochelys_coriacea",    # Reptile
  # "Globicephala_melas",      # Cétacé
  # "Pelagia_noctiluca",       # Cnidaire
  # "Physeter_macrocephalus",  # Cétacé
  # "Pinna_nobilis",           # Bivalve
  # "Posidonia_oceanica",      # Phanérogame
  # "Stenella_coeruleoalba",   # Cétacé
  # "Tursiops_truncatus",      # Cétacé
  # "Ziphius_cavirostris",     # Cétacé
  # "Zostera_marina",          # Phanérogame
  # Téléostéens
  # * : espèce commerciale
  # $ : espèce commerciale avec contrôle strict
  # ¤ : espèce emblématique
  # "Boops_boops",             # Bogue*
  # "Engraulis_encrasicolus",  # Anchois commun*
  "Hippocampus_guttulatus",  # Hippocampe moucheté¤
  "Hippocampus_hippocampus", # Hippocampe commun¤
  # "Merluccius_merluccius",   # Merlu commun*
  # "Mullus_barbatus",         # Rouget de vase*
  # "Mullus_surmuletus",       # Rouget de roche*
  # "Pagellus_erythrinus",     # Pageot commun*
  # "Sardina_pilchardus",      # Sardine commune*
  # "Sardinella_aurita",       # Sardinelle ronde*
  # "Solea_solea",             # Sole commune*
  # "Sprattus_sprattus",       # Sprat*
  # "Thunnus_thynnus",         # Thon rouge$
  # "Trachurus_trachurus",      # Chinchard*
  NULL
)
spf <- 1000
SPF <- ifelse(is.double(spf), spf, "SPE")

# Coût associé à la rétention d'une unité de gestion dans une réserve 

# PARAMETRE 05 ----
cost <- "shipping"
cost_threshold <- 0.85
# cost <- 1 

# Abréviation dans le nom du modèle
COST <- if (!is.double(cost)) { substr(toupper(cost), 1, 3) 
} else { cost }

# Ou nom du raster de contraintes utilisé
# Carte des zones à enjeux économique : tourisme, forages pétroliers, 
# zones de passage...
# Carte des zones dégradées (donc un coût supérieure pour ne pas ajouter
# des zones qui ne servent à rien dans la réserve)

# Prise en compte du statut des cellules au sein du réseau AMP déjà existant
status <- "NA"
# status <- "IN"
# status <- "OUT"
pa_status <- switch(
  status, 
  `NA` = "none",
  IN   = "locked_in",
  OUT  = "locked_out"
)

# 3. Génération des fichiers pour Marxan ----
# Génération des fichiers pour faire fonctionner Marxan
# Fichiers d'entrées et de sorties de données pour Marxan
makeMyDir(here("data", "marxan"))

marxan_file_names <- sapply(
  names(sp_subs),
  \(n) {
    paste(
      n, 
      paste(
        "PAT", pat, 
        "REP", repetitions, 
        "SPF", SPF, 
        "COS", COST, 
        "TGT", TGT,
        "STT", status,
        "ZON", zone, 
        sep = "-"
      ),
      sep = "_"
    )
  }
)

# Préparation du fichier de sortie
fichiers_sorties <- sapply(
  marxan_file_names, 
  \(f) {
    # Création du fichier correspondant au run Marxan
    path <- here("data", "marxan", f)
    makeMyDir(path, del = T)
    
    # Création des fichiers données et sorties pour le run
    path_inout <- list(
      input  = here(path, "input"), 
      output = here(path, "output")
    )
    lapply(path_inout, makeMyDir)
    
    # Copie du logiciel Marxan
    file.copy(
      from = list.files(
        here("data"), 
        pattern = "Marxan_x64$", # ou Marxan_x64.exe sous Windows
        full.names = T
      ), 
      to   = path
    )
    
    # Modification et copie du fichier de paramètres initiaux dans le fichier
    # de sortie. 
    txt <- readLines(
      here("data", "input.dat")
    )
    txt[14] <- sub("[0-9]+", repetitions,  txt[14]) 
    
    fileConn <- file(here(path, "input.dat"))
    write(x = txt, file = fileConn)
    close(fileConn)
    
    
    return(path)
  }, 
  simplify = F, 
  USE.NAMES = T
)

# applications
mapply(
  \(species, dataset, speciesRichness, path) {
    
    # Fichiers de sauvegarde
    path_inout <- list(
      input  = here(path, "input"), 
      output = here(path, "output")
    )
    
    # Fichier pour les éléments à conserver : 
    # ConsFeaFile ----
    ConsFeaFile <- conservationFeature(
      species     = species, 
      target      = target,
      target_cell = target_cell,
      spf         = spf,
      path_inout  = path_inout
    )
    
    # Fichier d'interation unités de gestion et élément de conservation :
    # PuVSCFFile ----
    puvspr <- unitsVScf(
      spatial_raster = dataset, 
      path_inout     = path_inout
    )
    
    # Fichier résumant les unités de gestion disponibles. 
    # PlanUnFile ----
    PlanUnFile <- planningUnits(
      spatRast_data   = dataset, 
      spatRast_cost   = pr[[cost]], 
      spatRast_status = pa$MPAnationalStatus,
      pa_status       = pa_status,
      cost_threshold  = cost_threshold, 
      path_inout      = path_inout
    )
    
    # 4. Éxecution de Marxan ----
    # exécution marxan
    setwd(path)
    system(command = here(path, "Marxan_x64")) 
    # ou Marxan_x64.exe sous Windows
    
    # traitement fichier de sortie marxan
    rout <- outputMarxanFiles_ensta(
      spatial_raster = speciesRichness, path_inout = path_inout
    )
  }, 
  sp_names, 
  sp_subs, 
  sp_sr, 
  fichiers_sorties, 
  USE.NAMES = T, 
  SIMPLIFY = F
)