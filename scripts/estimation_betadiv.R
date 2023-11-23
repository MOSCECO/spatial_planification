# Estimation du temps de calcul de diversité beta pour la taille de maille

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
