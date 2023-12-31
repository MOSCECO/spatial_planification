# génération des figures après un crash de la mémoire vive du mesu pour Marxan

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
nreps_total <- if (!is.double(nreps_total)) {
  as.data.frame(sp$species) %>%
    nrow() %>%
    plyr::round_any(1000, f = ceiling)
} else { nreps_total }

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
ncosts <- if (is.null(spatRast_cost_list)) "na" else {
  names(spatRast_cost_list100) %>% substr(1,3) %>% paste(collapse = "+")
}
marxan_file_names <- sapply(
  names(sp_subs),
  \(n) {
    paste(
      projt, tolower(n), tolower(nisl),
      paste("cost", ncosts, sep = "-"),
      nreps_total,
      sep = "_"
    )
  })

# Préparation du fichier de sortie final
fichiers_sorties <- Sapply(
  names(marxan_file_names),
  \(ntax) {
    # ntax <- "ALL"
    f <- marxan_file_names[[ntax]]
    # Création du fichier correspondant au run Marxan
    path <- here("data", "analysis", "marxan", f)
    return(path)
  })

# Aggrégation des fichiers de sorties dans le dossier "output" initial
Mapply(
  \(speciesRichness, path) {

    # speciesRichness <- sp_sr$ALL
    # path <- fichiers_sorties$ALL

    # Fichiers de sauvegarde
    path_inout <- list(
      input  = here(path, "input"),
      output = here(path, "output")
    )
    path_figures <- here(path, "figures")

    # traitement fichier de sortie marxan
    rout <- outputMarxanFiles_mosceco(
      spatial_raster = speciesRichness, path_inout = path_inout, progress = T
    )

  },
  sp_sr,
  fichiers_sorties
)
