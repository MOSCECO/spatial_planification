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

# Nombre de répétitions
repetitions <- if (!is.double(repetitions)) {
  as.data.frame(sp$species) %>%
    nrow() %>%
    plyr::round_any(1000, f = ceiling)
} else { repetitions }

# TARGET
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
TGT <- if (!is.double(target)) "YES" else "NO"

# Attribution des bonus/malus aux éléments de conservation

# SPF PARAMETRE 04 ----
# SPF : Species Penalty Factor / CPF : Conservation Penalty Factor
# Coût associé à la rétention d'une unité de gestion dans une réserve
SPF <- ifelse(is.double(spf), spf, "SPE")

# PARAMETRE 05 COST ----
#
spatRast_cost_list100 <- if (is.null(spatRast_cost_list)) NULL else {
  Sapply(
    spatRast_cost_list[unlist(lapply(spatRast_cost_list, \(x) !is.null(x)))],
    \(x) {
      # x <- spatRast_cost_list$hotspot
      x <- terra::crop(x, e)
      x/max(values(x), na.rm = T)*100
    })
}

# Abréviation dans le nom du modèle
COST <- if (is.null(spatRast_cost_list)) "NA" else {
  names(spatRast_cost_list100) %>%
    toupper() %>%
    substr(1, 3) %>%
    paste(collapse = "+")
}

# Prise en compte du statut des cellules au sein du réseau AMP déjà existant
pa_status <- switch(
  status,
  none      = "NA",
  locked_in = "IN",
  OUT       = "OUT"
)

PROJT <- switch(
  projt,
  current = "Contemporaine",
  ssp126 = "Projection optimiste",
  ssp585 = "Projection pessimiste"
)

ZON <- switch(
  nisl,
  ANT = "Antilles",
  GLP = "Guadeloupe",
  MTQ = "Martinique"
)
# 3. Génération des fichiers pour Marxan ----
# Génération des fichiers pour faire fonctionner Marxan
# Fichiers d'entrées et de sorties de données pour Marxan
makeMyDir(here("data", "analysis", "marxan"))

marxan_file_logs <- Sapply(
  names(sp_subs),
  \(n) {
    paste(
      paste("Distribution", ":", PROJT),
      paste("Taxon", ":", n),
      paste("Zone", ":", ZON),
      paste("Coûts", ":", COST),
      paste("Facteur de pénalité spécifique", ":", SPF),
      paste("Cible", ":", TGT),
      paste("Statuts", ":", pa_status),
      paste("Répétitions", ":", repetitions),
      sep = "\n"
    )
  }
)

ncosts <- names(spatRast_cost_list100) %>% substr(1,3) %>% paste(collapse = "+")
marxan_file_names <- sapply(
  names(sp_subs),
  \(n) {
    paste(
      projt, tolower(n), tolower(nisl), paste("cost", ncosts, sep = "-"),
      sep = "_"
    )
  })

# Préparation du fichier de sortie
fichiers_sorties <- sapply(
  names(marxan_file_names),
  \(ntax) {

    f <- marxan_file_names[[ntax]]

    # Création du fichier correspondant au run Marxan
    path <- here("data", "analysis", "marxan", f)
    makeMyDir(path, del = T)

    # Création des fichiers données et sorties pour le run
    path_inout <- list(
      input  = here(path, "input"),
      output = here(path, "output")
    )
    lapply(path_inout, makeMyDir)

    # Et pour les figures
    path_figures <- here(path, "figures")
    makeMyDir(path_figures)

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

    # Rédaction des détails de la routine
    fileConn <- file(here(path, "log"))
    write(x = marxan_file_logs[[ntax]], file = fileConn)
    close(fileConn)

    return(path)
  },
  simplify = F,
  USE.NAMES = T
)

# applications
Mapply(
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
    path_figures <- here(path, "figures")

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
      spatRast_data      = dataset,
      spatRast_cost_list = spatRast_cost_list100,
      path_inout         = path_inout,
      path_figures       = path_figures
    )

    # 4. Éxecution de Marxan ----

    # Mise en éxecutable
    cmd <- paste("chmod u+x", here(path, "Marxan_x64"))
    system(cmd)

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
  fichiers_sorties
)
