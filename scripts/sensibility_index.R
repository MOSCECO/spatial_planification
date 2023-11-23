# indice de sensibilité aux changements climatiques à partir des richesses
# spécifiques

ens_alg <- "ca"

# Richesse spécifique ####

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
r10 <- r1 - r0
x11(); plot(r10)
r50 <- r5 - r0
x11(); plot(r50)

# Pourcentage de variation
r10m <- (r1 - r0)/max(r0, r1)
x11(); plot(r10m)
r50m <- (r5 - r0)/max(r0, r5)
x11(); plot(r50m)

# Diversité beta ####

betar <- list.files(here("data", "tidy", "beta_diversity"), full.names = T) %>%
  lapply(rast)
names(betar) <- c("opti", "pessi")

x11(); plot(betar$opti$beta.sor)
x11(); plot(betar$pessi$beta.sor)



















