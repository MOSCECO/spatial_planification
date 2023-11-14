# Visualisation des sorties de Marxan
my_crs <- "EPSG:4326"

# Vecteurs des frontières
shp  <- maps_marxan[[nisl]]
path <- fichiers_sorties[[subfam]]

# raster
r <- rast(list.files(here(path, "output"), pattern = "tif", full.names = T))
r <- terra::project(r, my_crs)
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

makeMyDir(here("figures"))
makeMyDir(here("figures", "00_raster"))
makeMyDir(here("figures", "01_SPecies_Richness"))
makeMyDir(here("figures", "02_IRRemplacabilite"))
makeMyDir(here("figures", "03_Synthese_SPR_IRR"))
n <- marxan_file_names[[1]]

# sorties graphiques
g <- sorties

# raster
file_name <- here("figures", "00_raster", paste(n, "raster", sep = "_")) %>%
  paste0(".tif")
writeRaster(g$raster, file_name, overwrite = T)

# richesse spécifique
file_name <- here("figures", "01_SPecies_Richness",
                  paste(n, "sp_richness", sep = "_")) %>%
  paste0(".png")
ggexport(
  g$speciesRichness,
  filename = file_name,
  width  = 3000,
  height = 2000,
  res    = 300
)

# irremplaçabilité
file_name <- here("figures", "02_IRRemplacabilite",
                  paste(n, "irremplacabilite", sep = "_")) %>%
  paste0(".png")
ggexport(
  g$irremplacabilite,
  filename = file_name,
  width  = 3000,
  height = 2000,
  res    = 300
)

# les deux
file_name <- here("figures", "03_Synthese_SPR_IRR", paste(n, "all", sep = "_")) %>%
  paste0(".png")
ggexport(
  g$all,
  filename = file_name,
  width  = 5000,
  height = 2000,
  res    = 500
)

# Enregistrement des distributions d'espèces
# makeMyDir(here("figures", "Species_distribution"))
# makeMyDir(here("figures", "Species_distribution", mer))
# path1 <- here("figures", "Species_distribution", mer, "presence_probability")
# makeMyDir(path1)
# path2 <- here("figures", "Species_distribution", mer, "presence_absence")
# makeMyDir(path2)
# path3 <- here("figures", "Species_distribution", mer, "synthese")
# makeMyDir(path3)

# lapply(
#   names(sp),
#   \(n) {
#
#     # Probabilité de présence
#     r <- sp[[n]]
#     rdf <- as.data.frame(r, xy = T)
#     names(rdf)[3] <- "Probabilité\nde présence"
#
#     p1 <- ggplot() +
#       geom_tile(data = rdf, aes(x, y, fill = get("Probabilité\nde présence"))) +
#       scale_fill_viridis_c(option = "C") +
#       geom_sf(data = shp, fill = "lightgrey", col = NA) +
#       guides(fill = guide_colorbar(title = names(rdf)[3])) +
#       scale_x_continuous(expand = c(0, 0)) +
#       scale_y_continuous(expand = c(0, 0)) +
#       theme(
#         axis.title           = element_blank(),
#         panel.background     = element_blank(),
#         panel.border         = element_blank(),
#         panel.grid           = element_blank(),
#         panel.spacing        = unit(0, "lines"),
#         plot.background      = element_blank()
#       )
#
#     file_name <- here(
#       path1,
#       paste(n, "probabilite", "presence", mer, sep = "_") %>%
#         paste0(".png")
#     )
#
#     if (!dir.exists(file_name)) {
#       ggexport(
#         p1, filename = file_name, width = 2500, height = 2500, res = 500
#       )
#     }
#
#     # Présence-Absence
#     r2 <- sp_pa[[n]]
#     rdf2 <- as.data.frame(r2, xy = T)
#     names(rdf2)[3] <- "Présence\nAbsence"
#     rdf2[, 3] <- as.factor(rdf2[, 3])
#
#     p2 <- ggplot() +
#       geom_tile(data = rdf2, aes(x, y, fill = get("Présence\nAbsence"))) +
#       scale_fill_manual(values = c("#270591FF", "#F6E726FF")) +
#       geom_sf(data = shp, fill = "lightgrey", col = NA) +
#       guides(
#         fill = guide_legend(
#           title = paste(
#             names(rdf2)[3],
#             "(" %>%
#               paste0(paste("Seuil", "=", pat) %>% paste0(")")), sep = "\n"
#           )
#         )
#       ) +
#       scale_x_continuous(expand = c(0, 0)) +
#       scale_y_continuous(expand = c(0, 0)) +
#       theme(
#         axis.title           = element_blank(),
#         panel.background     = element_blank(),
#         panel.border         = element_blank(),
#         panel.grid           = element_blank(),
#         panel.spacing        = unit(0, "lines"),
#         plot.background      = element_blank()
#       )
#
#     file_name <- here(
#       path2,
#       paste(n, "presence", "absence", mer, sep = "_") %>%
#         paste0(".png")
#     )
#
#     if (!dir.exists(file_name)) {
#       ggexport(
#         p2, filename = file_name, width = 2500, height = 2500, res = 500
#       )
#     }
#
#     # Synthèse
#     p3 <- ggplot() +
#       geom_tile(data = rdf2, aes(x, y, fill = get("Présence\nAbsence"))) +
#       scale_fill_manual(values = c("#270591FF", "#F6E726FF")) +
#       geom_sf(data = shp, fill = "lightgrey", col = NA) +
#       guides(
#         fill = guide_legend(
#           title = paste(
#             names(rdf2)[3],
#             "(" %>%
#               paste0(paste("Seuil", "=", pat) %>% paste0(")")), sep = "\n"
#           )
#         )
#       ) +
#       scale_x_continuous(expand = c(0, 0)) +
#       scale_y_continuous(expand = c(0, 0)) +
#       theme(
#         axis.line            = element_blank(),
#         axis.text.y          = element_blank(),
#         axis.ticks.y         = element_blank(),
#         axis.title           = element_blank(),
#         panel.background     = element_blank(),
#         panel.border         = element_blank(),
#         panel.grid           = element_blank(),
#         panel.spacing        = unit(0, "lines"),
#         plot.background      = element_blank()
#       )
#
#     p4 <- p1 + p3 + plot_layout(guides = "collect")
#
#     file_name <- here(
#       path3,
#       paste(n, "synthese", "presence", mer, sep = "_") %>%
#         paste0(".png")
#     )
#
#     if (!dir.exists(file_name)) {
#       ggexport(
#         p4, filename = file_name, width = 5000, height = 2000, res = 500
#       )
#     }
#
#   }
# )

# makeMyDir(here("figures", "Anthropic_pressures", mer))
#
# lapply(
#   names(pr),
#   \(n) {
#
#     r <- pr[[n]]
#     rdf <- as.data.frame(r, xy = T)
#     names(rdf)[3] <- "Pression"
#
#     p1 <- ggplot() +
#       geom_tile(data = rdf, aes(x, y, fill = get(names(rdf)[3]))) +
#       scale_fill_viridis_c(option = "F") +
#       geom_sf(data = shp, fill = "lightgrey", col = NA) +
#       guides(fill = guide_colorbar(title = names(rdf)[3])) +
#       scale_x_continuous(expand = c(0, 0)) +
#       scale_y_continuous(expand = c(0, 0)) +
#       theme(
#         axis.title           = element_blank(),
#         panel.background     = element_blank(),
#         panel.border         = element_blank(),
#         panel.grid           = element_blank(),
#         panel.spacing        = unit(0, "lines"),
#         plot.background      = element_blank()
#       )
#
#     file_name <- here(
#       "figures",
#       "Anthropic_pressures",
#       mer,
#       paste(n) %>%
#         paste0(".png")
#     )
#
#     if (!dir.exists(file_name)) {
#       ggexport(
#         p1, filename = file_name, width = 2500, height = 2500, res = 500
#       )
#     }
#   }
# )
