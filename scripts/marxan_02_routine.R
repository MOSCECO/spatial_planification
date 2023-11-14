# Planification spatiale aux Antilles françaises

# emprise spatiale
e <- ext_marxan[[nisl]]

# species
sp <- popaPlot(
  projRasters        = mae,
  type               = "presence_absence",
  superfamily        = supfam,
  ensemble_algorithm = "wmean",
  projection_time    = projt,
  do_plot            = FALSE,
  do_plot_combine    = FALSE
)
sp_pa <- sp$species %>% terra::crop(e)

# Sélection des profondeurs 0-150m
dmask <- climosaic$depth %>% terra::crop(e)
dmask <- ifel(dmask > -150, 1, NA)
sp_pa <- sp_pa * dmask

# classification
cl <- read.csv(here("data", "tidy", "classification.csv"))
cl$Species <- cl$Species %>%
  gsub(" ", "_", .)
cl$SpeciesID <- 1:nrow(cl)

# Passage de probabilité de présence à présence-absence
print(
  paste(
    "Le jeu de données biologique initial contient",
    length(names(sp_pa)),
    "espèces."
  )
)

# Sous-ensemble de données
sp_subs <- sapply(
  my_subsets,
  \(my_sub) terra::subset(sp_pa, my_sub),
  USE.NAMES = T,
  simplify = F
)

# Noms des espèces dans les sous-ensembles
sp_names <- my_subsets

# Carte de richesse spécifique
sp_sr <- sapply(
  sp_subs,
  \(r) terra::app(r, sum),
  USE.NAMES = T
)

repetitions <- if (!is.double(repetitions)) {
  as.data.frame(sp) %>%
  nrow() %>%
  plyr::round_any(1000, f = ceiling)
} else { repetitions }
# Conservation Features (éléments de conservation)
# Paramètres à adapter ----
# the amount of the conservation feature to be included within
# the reserve network
# PARAMETRE fixé----

# 2. TARGET
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
# SPF PARAMETRE 04 ----
SPF <- ifelse(is.double(spf), spf, "SPE")

# Coût associé à la rétention d'une unité de gestion dans une réserve

# PARAMETRE 05 COST ----
# Abréviation dans le nom du modèle
COST <- if (!is.double(cost)) { substr(toupper(cost), 1, 3)
} else { cost }

# Prise en compte du statut des cellules au sein du réseau AMP déjà existant
pa_status <- switch(
  status,
  `NA` = "none",
  IN   = "locked_in",
  OUT  = "locked_out"
)

# 3. Génération des fichiers pour Marxan ----
# Génération des fichiers pour faire fonctionner Marxan
# Fichiers d'entrées et de sorties de données pour Marxan
makeMyDir(here("data", "analysis", "marxan"))

marxan_file_names <- sapply(
  names(sp_subs),
  \(n) {
    paste(
      n,
      paste(
        # "PAT", pat,
        "REP", repetitions,
        "SPF", SPF,
        "COS", COST,
        "TGT", TGT,
        "STT", status,
        "ZON", nisl,
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
    path <- here("data", "analysis", "marxan", f)
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
        here("scripts", "Marxan"),
        pattern = "Marxan_x64$", # Marxan_x64$
        full.names = T
      ),
      to   = path
    )

    # Modification et copie du fichier de paramètres initiaux dans le fichier
    # de sortie.
    txt <- readLines(here("data", "raw", "input.dat"))
    txt[14] <- sub("[0-9]+", repetitions, txt[14])

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

    # species <- sp_names$ALL
    # dataset <- sp_subs$ALL
    # speciesRichness <- sp_sr$ALL
    # path <- fichiers_sorties$ALL

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
    ) # erreur : correspondance nom d'espèce avec un espace ou avec un "_"

    # Fichier d'interation unités de gestion et élément de conservation :
    # PuVSCFFile ----
    puvspr <- unitsVScf(
      spatial_raster = dataset,
      path_inout     = path_inout
    ) # erreur : correspondance nom d'espèce avec un espace ou avec un "_"

    # Fichier résumant les unités de gestion disponibles.
    # PlanUnFile ----
    PlanUnFile <- planningUnits(
      spatRast_data   = dataset,
      # spatRast_cost   = pr[[cost]],
      # spatRast_status = pa$MPAnationalStatus,
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
  SIMPLIFY  = F
)
