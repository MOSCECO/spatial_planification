# indice de sensibilité aux changements climatiques à partir des richesses
# spécifiques

ens_alg <- "ca"

# Importation des raster de richesses spécifiques ré-échantillonnées
rl <- list.files(
  here("data", "tidy", "bootstrap_species_richness"),
  pattern = paste0("est.+", ens_alg),
  full.names = T
) %>% lapply(rast)

names(rl) <- list.files(
  here("data", "tidy", "bootstrap_species_richness"),
  pattern = paste0("est.+", ens_alg)
) %>%
  str_split("_") %>%
  lapply(pluck, length(.[[1]])) %>%
  lapply(\(x) substr(x, 1, nchar(x) - 4)) %>%
  unlist(use.names = F)

# Opérations sur rasters
r0 <- rl$current$q50.0
r1 <- rl$ssp126$q50.0
r5 <- rl$ssp585$q50.0

# Soustraction
r01 <- r0 - r1
x11(); plot(r01)
r05 <- r0 - r5
x11(); plot(r05)

# Pourcentage de variation
r01 <- (r1 - r0)/max(r0, r1)
x11(); plot(r01)
r05 <- (r5 - r0)/max(r0, r5)
x11(); plot(r05)
