# figures bootstrap species richness
save_figures <- FALSE

bsr <- Sapply(
  # c("ca", "wmean"),
  c("ca"),
  \(ens_alg) {

    # ens_alg <- "ca"

    # Dossier de sortie
    path_ens_alg <- here("figures", "species_richness_bootstrap", ens_alg)
    makeMyDir(path_ens_alg)

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

    # Graphes de comparaison entre richesse spécifiques modélisées et
    # ré-échantillonnées
    spec_pjs_plots <- Sapply(
      names(rl),
      \(nsr) {

        # nsr <- names(rl)[1]

        # Dossier de sortie
        path_nsr <- here(path_ens_alg, nsr)
        makeMyDir(path_nsr)

        # nsr <- names(rl)[[1]]
        sr <- rl[[nsr]]

        ps <- lapply(
          islands,
          \(nisl) {
            # nisl <- "GLP"

            # chargement des éléments du graphe
            isl          <- maps[[nisl]]
            e            <- ext(climatologies[[nisl]])
            sr_crop      <- terra::crop(sr, e)
            tb           <- as.data.frame(sr_crop, xy = T)

            plots_isl_bsr <- Sapply(
              names(tb[, 3:ncol(tb)]),
              \(value) {
                # value <- "q02.5"
                nval <- switch(
                  value,
                  init  = "Modélisations",
                  q02.5 = "Ré-échantillonnage (quantile 2.5%)",
                  q50.0 = "Ré-échantillonnage (médiane)",
                  q97.5 = "Ré-échantillonnage (quantile 97.5%)"
                )

                # figures ggplot2
                ggplot() +
                  geom_tile(
                    data = tb, aes(x = x, y = y, fill = factor(ceiling(get(value))))
                  ) +
                  geom_sf(data = isl) +
                  scale_fill_viridis_d(limits = factor(0:max(tb$init)))  +
                  labs(title = nval, x = "Longitude", y = "Latitude") +
                  {
                    if (value == "init") guides(
                      fill = guide_legend("Richesse\nspécifique")
                    )
                  } +
                  {
                    if (value != "init") {
                      theme_map() +
                        theme(legend.position = "none")
                    }
                  }
              })

            # x11()
            # plots_isl_bsr[[1]] | (
            #   (plots_isl_bsr[[2]] / plots_isl_bsr[[3]] / plots_isl_bsr[[4]]) +
            #     plot_layout(guides = "collect")
            # )

            # 1 : Richesse spécifique modélisée observée
            # 2 : Richesse spécifique ré-échantillonnée (quantile 2.5%)
            # 3 : Richesse spécifique ré-échantillonnée (médiane)
            # 4 : Richesse spécifique ré-échantillonnée (quantile 97.5%)
            p <- (plots_isl_bsr[[1]] | plots_isl_bsr[[2]]) /
              (plots_isl_bsr[[3]] | plots_isl_bsr[[4]])

            # nom des fichiers de sauvegarde
            file_name <- paste(
              "comparison", "bootstrap", "species", "richness",
              tolower(nisl), ens_alg, nsr,
              sep = "_"
            ) %>%
              paste0(".png")

            # sauvegarde
            if (save_figures) {
              ggexport(
                plot      = p,
                filename  = here(path_nsr, file_name),
                width     = 2000,
                height    = 1600,
                res       = 100,
                units     = "px",
                device    = "png",
                limitsize = F
              )
            }

            # préparation de la seconde carte sans certains éléments graphiques
            plots_isl_bsr[[1]] <- if(nisl == "MTQ") {
              plots_isl_bsr[[1]] +
                theme(
                  title        = element_blank(),
                  axis.title.y = element_blank(),
                  axis.line.y  = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank()
                )
            } else {
              plots_isl_bsr[[1]] + theme(legend.position = "none")
            }

            plots_isl_bsr[[2]] <- if(nisl == "MTQ") {
              plots_isl_bsr[[2]] + theme(title = element_blank())
            } else { plots_isl_bsr[[2]] }

            plots_isl_bsr[[3]] <- if(nisl == "MTQ") {
              plots_isl_bsr[[3]] + theme(title = element_blank())
            } else { plots_isl_bsr[[3]] }

            plots_isl_bsr[[4]] <- if(nisl == "MTQ") {
              plots_isl_bsr[[4]] + theme(title = element_blank())
            } else { plots_isl_bsr[[4]] }

            return(plots_isl_bsr)
          }
        )

        P <- Mapply(\(x, y) x + y, ps$GLP, ps$MTQ)
        P_out <- (P[[1]] | P[[2]]) / (P[[3]] | P[[4]])

        # nom des fichiers de sauvegarde
        file_name <- paste(
          "comparison", "bootstrap", "species", "richness",
          "ant", ens_alg, nsr,
          sep = "_"
        ) %>%
          paste0(".png")

        if (save_figures) {
          ggexport(
            plot      = P_out,
            filename  = here(path_nsr, file_name),
            width     = 4200,
            height    = 2000,
            res       = 200,
            units     = "px",
            device    = "png",
            limitsize = F
          )
        }

        return(P)
      })
  })
