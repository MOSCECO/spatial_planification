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
sr_dtfr %>% lapply(dim)
d0 <- sr_dtfr$current
d1 <- sr_dtfr$ssp126

mytimes <- lapply(
  seq(500, 10000, 500),
  \(nr) {
    print(nr)
    t0 <- Sys.time()
    betapart::beta.temp(
      d0[1:nr, ],
      d1[1:nr, ]
    )
    t1 <- Sys.time()
    return(t1 - t0)
  }
)

tb <- tibble(
  nrows = seq(500, 10000, 500),
  times = as.numeric(mytimes)
)

ggplot(data = tb, aes(x = nrows, y = times)) +
  geom_line() +
  geom_point() +
  xlab("Nombre de lignes dans le tableau") +
  ylab("Temps en secondes")





















