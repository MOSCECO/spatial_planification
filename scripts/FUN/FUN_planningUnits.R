planningUnits <- function(
    spatRast_data,
    spatRast_cost,
    spatRast_status,
    path_inout,
    cost_threshold = "none",
    pa_status      = "none",
    writeFile = TRUE
){

  if (is.double(cost_threshold)) {
    if (cost_threshold <= 0 | cost_threshold > 1) {
      stop("Le seuil de pression doit être strictement supérieur à 0 et inférieur ou égale à 1")
    }
  }

  # Statut des cellules au sein du réseau d'AMP déjà existantes
  # cell_stat <- switch(
  #   pa_status,
  #   none       = 1,
  #   locked_in  = 2,
  #   locked_out = 3
  # )

  # Pré-traitement
  r <- spatRast_data[[1]]
  r_proj <- terra::project(r, "EPSG:4087")
  r_size <- terra::cellSize(r_proj, unit = "km")
  # d <- data.frame(spatRast_status, cells = T)

  # Association d'un score aux cellules selon leur appartenance à une AMP
  # my_status <- ifelse(
  #   as.data.frame(r_proj, cells = T, xy = T)$cell %in%
  #     as.numeric(row.names(d[d$MPAnationalStatus == 1, ])), cell_stat, 1
  # )
  my_status <- 1

  # Association d'un coût à la cellule selon une pression

  my_cost <- if (is.double(cost_threshold)) {
    spatRast_cost_thresh <- ifel(
      spatRast_cost < max(values(spatRast_cost), na.rm = T)*(1-cost_threshold), 0, 1
    )
    if (cost_threshold != "none") {
      makeMyDir(here("figures", "Anthropic_pressures"))
      makeMyDir(here("figures", "Anthropic_pressures", "Proportion"))
      path1 <- here("figures", "Anthropic_pressures", "Proportion", mer)
      makeMyDir(path1)

      shp <- med_zones_sf[[mer]] %>%
        st_transform("EPSG:4326")

      r1 <- spatRast_cost_thresh
      r1df <- as.data.frame(r1, xy = T)
      r1df[, 3] <- factor(r1df[, 3])
      names(r1df)[3] <- paste(
        "Pression", "\n", "(" %>% paste0(cost_threshold*100, "%)")
      )

      p11 <- ggplot() +
        geom_tile(data = r1df, aes(x, y, fill = get(names(r1df)[3]))) +
        scale_fill_manual(
          values = c("#150E37FF", "#FEC085FF")
        ) +
        guides(fill = guide_legend(title = names(r1df)[3])) +
        geom_sf(data = shp, fill = "lightgrey", col = NA) +
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

      file_name <- here(
        path1,
        paste(cost, "prop", sub("0\\.", "", cost_threshold), sep = "_") %>%
          paste0(".png")
      )

      if (!dir.exists(file_name)) {
        ggexport(
          p11, filename = file_name, width = 5000, height = 5000, res = 500
        )
      }
    }

    dcost <- data.frame(spatRast_cost_thresh, cells = T)
    ifelse(
      as.data.frame(r_proj, cells = T, xy = T)$cell %in%
        as.numeric(row.names(dcost[dcost[names(spatRast_cost)] == 1, ])),
      1000,
      1
    )
  } else {
    1
  }

  # Aggrégation en un dataframe
  PlanUnFile <- as.data.frame(r_proj, cells = T, xy = T) %>%
    cbind(
      area   = values(r_size)[which(!is.nan(values(r_proj)))],
      id     = 1:nrow(.),
      cost   = my_cost,
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
}
