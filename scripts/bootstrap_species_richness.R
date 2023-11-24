# diminution des outliers de richesses spécifiques

source(here::here("scripts", "boot.R"))

# itérations sur
# i_supfam <- c("all", "Muricoidea", "Majoidea")
i_supfam <- c("all")
i_ensalg <- c("ca", "wmean")
i_projtm <- c("current", "ssp126", "ssp585")

# Paramétrage bootstrap
n_resamp <- 50

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

            # supfam  <- "all"
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
            # t0 <- Sys.time()
            # bootstrap_raster <- bootstrap %>%
            #   lapply(
            #     \(nsp) app(
            #       subset(sp_pa, nsp), sum ou \(i) sum(i) et cores = x
            #     )
            #   )
            # t1 <- Sys.time()
            # t1-t0
            t0 <- Sys.time()
            bootstrap_array <- bootstrap %>%
              mclapply(
                \(nsp) {
                  sr_sub <- subset(sp_pa, nsp)
                  sr_arr <- as.array(sr_sub)
                  parPrint("Somme des rasters")
                  out <- apply(sr_arr, c(1,2), sum)
                  parPrint("Somme des rasters ok")
                  return(out)
                },
                mc.cores = detectCores() - 1
              )
            t1 <- Sys.time()
            print(t1-t0)
            print("Aggrégation du raster ok")

            bootstrap_raster <- bootstrap_array %>%
              lapply(rast, crs = crs(sp$combine), ext = ext(sp$combine))

            print("Changement des noms du raster")
            bootstrap_raster <- Mapply(
              \(r, id) {names(r) <- paste0("run", id) ; return(r)},
              bootstrap_raster,
              1:n_resamp
            )
            print("Changement des noms du raster ok")
            bsr <- Reduce(c, bootstrap_raster)
            # sauvegarde du raster de tous les runs de ré-échantillonnage
            print("Sauvegarde")
            writeRaster(
              bsr,
              here(pout, paste0("bootstrap_sr_", file_name, ".tif")),
              overwrite = T
            )
            print("Sauvegarde ok")
            # Calcul de trois estimateurs de richesse spécifique
            bsr_arr <- as.array(bsr)
            e <- quantile(bsr, 0.025, na.rm = T)
            e <- quantile(bsr, 0.500, na.rm = T)
            e <- quantile(bsr, 0.975, na.rm = T)

            nr <- dim(bsr_arr)[1]
            nc <- dim(bsr_arr)[2]
            library(primes)
            dvd <- gcd(round(nr, -2), round(nc, -2))
            chunks_rows <- split(1:nr, ceiling(seq_along(1:nr)/dvd))
            chunks_cols <- split(1:nc, ceiling(seq_along(1:nc)/dvd))
            bsr_chunks <- lapply(
              chunks_rows,
              \(nr) lapply(chunks_cols, \(nc) bsr_arr[nr, nc, ])
            )
            bsr_chunks <- Reduce(
              append, lapply(1:length(bsr_chunks), \(x) pluck(bsr_chunks, x))
            )
            names(bsr_chunks) <- 1:length(bsr_chunks)



            bsr_quant <- lapply(
              c(0.025, 0.500, 0.975),
              \(prob) {
                # prob <- 0.025
                print(prob)
                mclapply(
                  bsr_chunks,
                  \(chnk) {
                    # chnk <- bsr_chunks[[348]]
                    parPrint("Quantile")
                    out <- apply(
                      chnk,
                      c(1:2),
                      \(x) quantile(x, probs = prob, na.rm = TRUE)
                    )
                    parPrint("Quantile ok")
                    return(out)
                  }, mc.cores = detectCores() - 1)
              })
            bsr_quant_rast <- bsr_quant %>%
              lapply(rast, crs = crs(sp$combine), ext = ext(sp$combine))

            # aggrégation aux données initiales et sauvegarde
            BSR <- c(sp$combine, Reduce(c, bsr_quant_rast))
            names(BSR) <- c("init", "q02.5", "q50.0", "q97.5")
            writeRaster(
              BSR,
              here(pout, paste0("bootstrap_sr_est_", file_name, ".tif")),
              overwrite = T
            )

          })
      })
  })
