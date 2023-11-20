# diminution des outliers de richesses spécifiques

# itérations sur
# i_supfam <- c("all", "Muricoidea", "Majoidea")
i_supfam <- c("all")
i_ensalg <- c("wmean", "ca")
i_projtm <- c("current", "ssp126", "ssp585")

# Paramétrage bootstrap
n_resamp <- 10

# dossier de sortie
pout <- here("data", "tidy", "bootstrap_species_richness")
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

            file_name <- paste(supfam, ens_alg, projt, sep = "_")

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

            # Sélection des profondeurs 0-150m
            sp_pa <- sp_pa * dmask
            sp_cb <- sp$combine * dmask

            # bootstrap
            bootstrap <- lapply(
              seq(1, n_resamp),
              \(x) {
                sample(
                  x       = names(sp_pa),
                  size    = length(names(sp_pa)),
                  replace = TRUE
                )
              })
            # Sélection des noms uniques d'espèces
            bootstrap <- bootstrap %>%
              lapply(\(nsp) unique(nsp))
            # Création de raster sommes de toutes les présences/absences des
            # espèces selon le jeu d'espèces issu du bootstrap
            bootstrap_raster <- bootstrap %>%
              lapply(\(nsp) app(subset(sp_pa, nsp), sum))
            bootstrap_raster <- Mapply(
              \(r, id) {names(r) <- paste0("run", id) ; return(r)},
              bootstrap_raster,
              1:n_resamp
            )
            bsr <- Reduce(c, bootstrap_raster)
            # sauvegarde du raster de tous les runs de ré-échantillonnage
            writeRaster(
              bsr,
              here(pout, paste0("bootstrap_sr_", file_name, ".tif")),
              overwrite = T
            )

            # Calcul de trois estimateurs de richesse spécifique
            bsr_q02.5 <- app(bsr, \(x) quantile(x, probs = 0.025, na.rm = T))
            bsr_q50.0 <- app(bsr, \(x) quantile(x, probs = 0.500, na.rm = T))
            bsr_q97.5 <- app(bsr, \(x) quantile(x, probs = 0.975, na.rm = T))

            # aggrégation aux données initiales et sauvegarde
            BSR <- c(sp$combine, bsr_q02.5, bsr_q50.0, bsr_q97.5)
            names(BSR) <- c("init", "q02.5", "q50.0", "q97.5")
            writeRaster(
              BSR,
              here(pout, paste0("bootstrap_sr_est_", file_name, ".tif")),
              overwrite = T
            )
            # plot
            makeMyDir(here("figures", "species_richness_bootstrap"))
            png(
              here(
                "figures",
                "species_richness_bootstrap",
                paste0("comp_", file_name, ".png")
              )
            )
            par(mfrow = c(1, 4))
            plot(BSR$init, main = "RS observée")
            plot(BSR$q02.5, main = "RS ré-échantillonnée\nQuantile 2.5")
            plot(BSR$q50.0, main = "RS ré-échantillonnée\nMédiane")
            plot(BSR$q97.5, main = "RS ré-échantillonnée\nQuantile 97.5")
            dev.off()

          })
      })
  })
