# Visualisation des sorties de Marxan
my_crs <- "EPSG:4326"

# Vecteurs des frontières
shp  <- maps_marxan[[nisl]]
path <- fichiers_sorties[[subfam]]
path_figures <- here(path, "figures")
path_spat <- here(path_figures, "planification") ; makeMyDir(path_spat)
path_tif <- here(path_spat, "tif") ; makeMyDir(path_tif)
path_png <- here(path_spat, "png") ; makeMyDir(path_png)

# raster
r   <- rast(list.files(here(path, "output"), pattern = "tif", full.names = T))
r   <- terra::project(r, my_crs)
shp <- st_transform(shp, my_crs)

rdf <- as.data.frame(r, xy = T)

# richesse spécifique
d1 <- rdf %>% select(x, y, sum)
names(d1)[3] <- "Richesse\nspécifique"

p1 <- ggplot() +
  geom_tile(data = d1, aes(x, y, fill = get("Richesse\nspécifique"))) +
  scale_fill_viridis_c(limits = c(0, length(my_subsets[[subfam]]))) +
  geom_sf(data = shp, fill = "lightgrey", col = NA) +
  guides(fill = guide_colorbar(title = names(d1)[3])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    # axis.line            = element_blank(),
    # axis.text            = element_blank(),
    # axis.ticks           = element_blank(),
    axis.title           = element_blank(),
    panel.background     = element_blank(),
    panel.border         = element_blank(),
    panel.grid           = element_blank(),
    panel.spacing        = unit(0, "lines"),
    plot.background      = element_blank()
  )

# irremplaçabilité
d2 <- rdf %>% select(x, y, irremplacabilite)
names(d2)[3] <- "Irremplaçabilité"

p2 <- ggplot() +
  geom_tile(data = d2, aes(x, y, fill = get("Irremplaçabilité"))) +
  scale_fill_gradient(
    low = "yellow2", high = "red", na.value = "white"
  ) +
  geom_sf(data = shp, fill = "lightgrey", col = NA) +
  guides(fill = guide_colorbar(title = names(d2)[3])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    # axis.line            = element_blank(),
    # axis.text            = element_blank(),
    # axis.ticks           = element_blank(),
    axis.title           = element_blank(),
    panel.background     = element_blank(),
    panel.border         = element_blank(),
    panel.grid           = element_blank(),
    panel.spacing        = unit(0, "lines"),
    plot.background      = element_blank()
  )

p3 <- ggplot() +
  geom_tile(data = d2, aes(x, y, fill = get("Irremplaçabilité"))) +
  scale_fill_gradient(
    low = "yellow2", high = "red", na.value = "white"
  ) +
  geom_sf(data = shp, fill = "lightgrey", col = NA) +
  guides(fill = guide_colorbar(title = names(d2)[3])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.line            = element_blank(),
    axis.text.y          = element_blank(),
    axis.ticks.y         = element_blank(),
    axis.title           = element_blank(),
    panel.background     = element_blank(),
    panel.border         = element_blank(),
    panel.grid           = element_blank(),
    panel.spacing        = unit(0, "lines"),
    plot.background      = element_blank()
  )

p4 <- p1 + p3 + plot_layout(guides = "collect")

sorties <- list(
  raster = r, speciesRichness = p1, irremplacabilite = p2, all = p4
)

n <- marxan_file_names[[1]]

# sorties graphiques
g <- sorties

# raster
file_name <- here(path_tif, paste(n, "raster", sep = "_")) %>%
  paste0(".tif")
writeRaster(g$raster, file_name, overwrite = T)

# richesse spécifique
file_name <- here(path_png, paste(n, "richesse_specifique", sep = "_")) %>%
  paste0(".png")
ggexport(
  g$speciesRichness,
  filename = file_name,
  width    = 3000,
  height   = 2000,
  res      = 300
)

# irremplaçabilité
file_name <- here(path_png, paste(n, "irremplacabilite", sep = "_")) %>%
  paste0(".png")
ggexport(
  g$irremplacabilite,
  filename = file_name,
  width  = 3000,
  height = 2000,
  res    = 300
)

# les deux
file_name <- here(path_png, paste(n, "all", sep = "_")) %>% paste0(".png")
ggexport(
  g$all,
  filename = file_name,
  width  = 5000,
  height = 2000,
  res    = 500
)
