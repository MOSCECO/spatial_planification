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

# Soustraction
rd       <- list()
rd$opti  <- subset(rl$current, 2:4) - subset(rl$ssp126, 2:4)
rd$pessi <- subset(rl$current, 2:4) - subset(rl$ssp585, 2:4)

# Pourcentage de variation
pv <- function(x, y) (x - y)/max(x, y)
rv <- list()
rv$opti <- pv(subset(rl$ssp126, 2:4), subset(rl$current, 2:4))
rv$pessi <- pv(subset(rl$ssp585, 2:4), subset(rl$current, 2:4))

# Diversité beta ####
rb <- list.files(here("data", "tidy", "beta_diversity"), full.names = T) %>%
  lapply(rast)
names(rb) <- c("opti", "pessi")
