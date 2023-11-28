# beta diversity cluster
T0 <- Sys.time()
source(here::here("scripts", "boot.R"))

# destination folder
pout <- here("data", "tidy", "beta_diversity")
makeMyDir(pout)

ens_alg <- "ca"

# Importation des raster de richesses spécifiques
sr <- list.files(
  here("data", "tidy", "rasters_species_richness"),
  pattern = "ca",
  full.names = T
) %>% lapply(rast)

names(sr) <- list.files(
  here("data", "tidy", "rasters_species_richness"),
  pattern = "ca"
) %>%
  str_split("_") %>%
  lapply(pluck, length(.[[1]])) %>%
  lapply(\(x) substr(x, 1, nchar(x) - 4)) %>%
  unlist(use.names = F)

sr_crop <- lapply(sr, \(x) x * dmask)
sr_dtfr <- lapply(sr_crop, as.data.frame)

# Données sur lesquelles itérer
xy <- as.data.frame(sr_crop$current, xy = T) %>% select(x, y)
scenarii <- list(
  optimistic  = list(d0 = sr_dtfr$current, d1 = sr_dtfr$ssp126),
  pessimistic = list(d0 = sr_dtfr$current, d1 = sr_dtfr$ssp585)
)

lapply(
  names(scenarii),
  \(scenario) {

    print(scenario)

    # Chargement des modélisations
    d0 <- scenarii[[scenario]]$d0
    d1 <- scenarii[[scenario]]$d1

    # Séparation en morceaux itérables
    d0_chunks <- split(d0, ceiling(seq_along(1:nrow(d0))/10000))
    d1_chunks <- split(d1, ceiling(seq_along(1:nrow(d1))/10000))

    # Calcul de la beta diversité temporelle
    t0 <- Sys.time()
    beta_dtfr <- Mapply(\(x, y) betapart::beta.temp(x, y), d0_chunks, d1_chunks)
    t1 <- Sys.time()
    print(paste("Duration for", scenario, "scenario"))
    print(t1 - t0)

    # Conversion en spatial raster de caractéristiques identiques à l'original
    beta_rast <- rast(
      cbind(xy, do.call(rbind, beta_dtfr)),
      crs = crs(sr_crop$current),
      ext = ext(sr_crop$current)
    )

    # sauvegarde
    file_name <- paste(
      "temporal", "beta", "diversity", ens_alg, scenario, sep = "_"
    )
    writeRaster(
      beta_rast, here(pout, file_name %>% paste0(".tif")), overwrite = T
    )
  })

TF <- Sys.time()
print("Total duration of the computation")
print(TF - T0)
