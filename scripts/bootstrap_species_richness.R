# diminution des outliers de richesses spécifiques

source(here::here("scripts", "boot.R"))

# itérations sur
i_supfam <- c("all", "Muricoidea", "Majoidea")
i_ensalg <- c("ca", "wmean")
i_projtm <- c("current", "ssp126", "ssp585")

# Paramétrage bootstrap
n_resamp <- 2

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

            # supfam  <- "Muricoidea"
            # ens_alg <- "ca"
            # projt   <- "ssp126"

            file_name <- paste(supfam, ens_alg, projt, sep = "_")

            print(file_name)

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
            print("Bootsrap")
            bootstrap <- lapply(
              seq(1, n_resamp),
              \(x) {
                sample(
                  x       = names(sp_pa),
                  size    = length(names(sp_pa)),
                  replace = TRUE
                )
              })
            print("Bootsrap ok")

            # Sélection des noms uniques d'espèces
            print("Selection des noms d'espèces")
            bootstrap <- bootstrap %>%
              lapply(\(nsp) unique(nsp))
            print("Selection des noms d'espèces ok")
            # Création de raster sommes de toutes les présences/absences des
            # espèces selon le jeu d'espèces issu du bootstrap
            print("Aggrégation du raster")
            t0 <- Sys.time()
            bootstrap_raster <- mclapply(
              1:length(bootstrap),
              \(i) {
                # i <- 1
                parPrint(paste0(i, "/", length(bootstrap)))
                nsp    <- bootstrap[[i]]
                sr_sub <- subset(sp_pa, nsp)
                return(wrap(sum(sr_sub, na.rm = TRUE)))
              },
              mc.cores = detectCores() - 1
            ) %>% lapply(unwrap)
            t1 <- Sys.time()
            print(t1-t0)
            print("Aggrégation du raster ok")

            print("Changement des noms du raster")
            bootstrap_raster <- Mapply(
              \(r, id) {names(r) <- paste0("run", id) ; return(r)},
              bootstrap_raster,
              1:n_resamp
            )
            print("Changement des noms du raster ok")

            print("Aggrégation des rasters")
            bsr <- do.call(c, bootstrap_raster)
            print("Aggrégation des rasters ok")

            # sauvegarde du raster de tous les runs de ré-échantillonnage
            # print("Sauvegarde")
            # writeRaster(
            #   bsr,
            #   here(pout, paste0("bootstrap_sr_", file_name, ".tif")),
            #   overwrite = T
            # )
            # print("Sauvegarde ok")

            # Calcul de trois estimateurs de richesse spécifique
            # aggrégation aux données initiales
            print("Raster des quantiles")

            bsr_chunks <- fragSpatialRaster(bsr, dvd = 100)

            bsr_chunks_quant <- mclapply(
              bsr_chunks,
              \(chnk) {
                quantile(
                  chnk, probs = c(0.025, 0.500, 0.975), na.rm = TRUE
                )
              },
              mc.cores = 1
            )

            bsr_quant <- Reduce(merge, bsr_chunks_quant)

            print("Raster des quantiles ok")

            BSR <- c(sp$combine, bsr_quant)
            names(BSR)[1] <- "init"

            # sauvegarde
            print("Sauvegarde")
            writeRaster(
              BSR,
              here(pout, paste0("bootstrap_sr_est_", file_name, ".tif")),
              overwrite = T
            )
            print("Sauvegarde ok")

          })
      })
  })
