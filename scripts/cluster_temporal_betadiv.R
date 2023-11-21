# beta diversity cluster

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



# scénario optimiste ----
xy <- as.data.frame(sr_crop$current, xy = T) %>% select(x, y)
d0 <- sr_dtfr$current
d1 <- sr_dtfr$ssp126

d0_chunks <- split(d0, ceiling(seq_along(1:nrow(d0))/10000))
d1_chunks <- split(d1, ceiling(seq_along(1:nrow(d1))/10000))

beta_dtfr <- Mapply(\(x, y) betapart::beta.temp(x, y), d0_chunks, d1_chunks)
beta_rast <- rast(cbind(xy, do.call(rbind, beta_dtfr)))

# sauvegarde
file_name <- paste(
  "temporal", "beta", "diversity", ens_alg, "optimistic", sep = "_"
)
writeRaster(beta_rast, here(pout, file_name %>% paste0(".tif")))



# scénario optimiste ----
xy <- as.data.frame(sr_crop$current, xy = T) %>% select(x, y)
d0 <- sr_dtfr$current
d1 <- sr_dtfr$ssp585

d0_chunks <- split(d0, ceiling(seq_along(1:nrow(d0))/10000))
d1_chunks <- split(d1, ceiling(seq_along(1:nrow(d1))/10000))

beta_dtfr <- Mapply(\(x, y) betapart::beta.temp(x, y), d0_chunks, d1_chunks)
beta_rast <- rast(cbind(xy, do.call(rbind, beta_dtfr)))

# sauvegarde
file_name <- paste(
  "temporal", "beta", "diversity", ens_alg, "pessimistic", sep = "_"
)
writeRaster(beta_rast, here(pout, file_name %>% paste0(".tif")))
