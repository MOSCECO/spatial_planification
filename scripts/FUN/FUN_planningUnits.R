planningUnits <- function(
    spatRast_data,
    spatRast_cost_list, # liste de rasters de coûts
    path_inout,
    path_figures,
    writePlot = TRUE,
    writeFile = TRUE
){
  # spatRast_cost_list <- spatRast_cost_list100

  # Pré-traitement
  r <- spatRast_data[[1]]
  r_proj <- terra::project(r, "EPSG:4087")
  r_size <- terra::cellSize(r_proj, unit = "km")

  # Statut des cellules uniforme dans notre cas
  my_status <- 1

  if (!is.null(spatRast_cost_list)) {
    # Association d'un coût à la cellule selon une pression

    # Fonction de coût pour une grille de valeur en pourcentage correspondant à
    # une qualité d'intérêt pour l'unité de planification.

    my_costs <- Sapply(
      names(spatRast_cost_list100),
      \(nsr) {

        # nsr <- "hotspot"
        # nsr <- "percvar"
        # nsr <- "betadiv"
        label_legend <- switch(
          nsr,
          hotspot = "Pourcentage de la \nrichesse spécifique\nmaximale",
          percvar = "Pourcentage du\ncoefficient de\nvariation maximal",
          betadiv = "Pourcentage de\nla beta-diversité\n maximale"
        )
        sr <- spatRast_cost_list[[nsr]]
        sr_cost <- percentage2Cost(sr)
        l <- list(perc = sr, cost = sr_cost)

        # Dossier de sortie
        path_sc <- here(path_figures, "spatial_cost")
        makeMyDir(path_sc)

        # polygone de l'île
        shp <- maps_marxan[[nisl]]

        # Génération des graphiques, sauvegarde des grilles de valeurs
        lapply(
          names(l),
          \(nsrl) {
            # nsrl <- "perc"
            # nsrl <- "cost"
            srl <- l[[nsrl]]
            sr_df <- as.data.frame(sr, xy = T)
            names(sr_df)[3] <- label_legend

            # Nuancier
            col_low  <- if (nsrl != "cost") "#150E37FF" else "#FEC085FF"
            col_high <- if (nsrl != "cost") "#FEC085FF" else "#150E37FF"

            # Label
            llegend <- if (nsrl != "cost") names(sr_df)[3] else "Coût"

            # Graphique
            p_cost <- ggplot() +
              geom_tile(data = sr_df, aes(x, y, fill = get(label_legend))) +
              scale_fill_gradient(low = col_low, high = col_high) +
              guides(fill = guide_colorbar(title = llegend)) +
              geom_sf(data = shp, fill = "white", col = NA) +
              geom_sf(data = shp, fill = "lightgreen", col = NA, alpha = 0.5) +
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_continuous(expand = c(0, 0)) +
              theme(
                axis.title           = element_blank(),
                panel.background     = element_blank(),
                panel.border         = element_blank(),
                panel.grid           = element_blank(),
                panel.spacing        = unit(0, "lines"),
                plot.background      = element_blank()
              )

            # Sauvegarde
            sauv <- if (nsrl != "cost") 100 else "cost"
            file_name <- paste(nsr, sauv, sep = "_")

            if(writePlot) {
              ggexport(
                p_cost,
                filename = here(path_sc, file_name %>% paste0(".png")),
                width = 5000, height = 5000, res = 500
              )
            }

            if(writeFile) {
              writeRaster(
                srl, here(path_sc, file_name %>% paste0(".tif")), overwrite = T
              )
            }

          }
        )

        return(as.data.frame(l$cost))

      }
    )
    my_costs %>% lapply(dim)
    # Compilation des coûts
    df_costs <- Reduce(rowSums, my_costs)

    # Aggrégation en un dataframe
    PlanUnFile <- as.data.frame(r_proj, cells = T, xy = T) %>%
      cbind(
        area   = values(r_size)[which(!is.nan(values(r_proj)))],
        id     = 1:nrow(.),
        cost   = df_costs,
        status = my_status
      )
    names(PlanUnFile) <- c(
      "cell", "xloc", "yloc", "value", "area", "id", "cost", "status"
    )

    if (writeFile){
      write.table(
        PlanUnFile,
        file = here(path_inout$input, "PlanUnFile.txt"),
        row.names = F
      )

      write.table(
        PlanUnFile[order(PlanUnFile$id), c('id', 'cost', 'status')],
        file = here(path_inout$input, "pu.dat"),
        sep = ",",
        row.names = F,
        quote = F
      )
    }

    return(PlanUnFile)
  } else {
    PlanUnFile <- as.data.frame(r_proj, cells = T, xy = T) %>%
      cbind(
        area   = values(r_size)[which(!is.nan(values(r_proj)))],
        id     = 1:nrow(.),
        cost   = 1,
        status = 1
      )
    names(PlanUnFile) <- c(
      "cell", "xloc", "yloc", "value", "area", "id", "cost", "status"
    )

    if (writeFile){
      write.table(
        PlanUnFile,
        file = here(path_inout$input, "PlanUnFile.txt"),
        row.names = F
      )

      write.table(
        PlanUnFile[order(PlanUnFile$id), c('id', 'cost', 'status')],
        file = here(path_inout$input, "pu.dat"),
        sep = ",",
        row.names = F,
        quote = F
      )
    }
  }

}
