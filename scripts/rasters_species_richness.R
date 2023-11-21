# sauvegarde des rasters de richesse spécifique

# itérations sur
# i_supfam <- c("all", "Muricoidea", "Majoidea")
i_supfam <- c("all")
i_ensalg <- c("wmean", "ca")
i_projtm <- c("current", "ssp126", "ssp585")

# dossier de sortie
pout <- here("data", "tidy", "rasters_species_richness")
makeMyDir(pout)

lapply(
  i_supfam,
  \(supfam) {
    lapply(
      i_ensalg,
      \(ens_alg) {
        lapply(
          i_projtm,
          \(projt) {

            sp <- popaPlot(
              projRasters         = mae,
              type                = "presence_absence",
              superfamily         = supfam,
              ensemble_algorithm  = ens_alg,
              projection_time     = projt,
              threshold_algorithm = "TSS",
              do_plot             = FALSE,
              do_plot_combine     = FALSE
            )
            sp_pa <- sp$species

            file_name <- paste("raster", supfam, ens_alg, projt, sep = "_") %>%
              paste0(".tif")
            writeRaster(sp_pa, here(pout, file_name))

          })
      })
  })
