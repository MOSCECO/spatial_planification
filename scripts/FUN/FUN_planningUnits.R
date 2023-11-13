planningUnits <- function(
    spatRast_data, 
    spatRast_cost, 
    spatRast_status, 
    path_inout, 
    cost_threshold = "none", 
    pa_status      = "none",
    write_file = TRUE
){
  
  if (is.double(cost_threshold)) {
    if (cost_threshold <= 0 | cost_threshold > 1) {
      stop("Le seuil de pression doit être strictement supérieur à 0 et inférieur ou égale à 1")
    }
  }
  
  # Statut des cellules au sein du réseau d'AMP déjà existantes
  cell_stat <- switch(
    pa_status, 
    none       = 1,
    locked_in  = 2, 
    locked_out = 3 
  )
  
  # Pré-traitement
  r <- spatRast_data[[1]]
  r_proj <- terra::project(r, "EPSG:4087")
  r_size <- terra::cellSize(r_proj, unit = "km")
  d <- data.frame(spatRast_status, cells = T)
  
  # Association d'un score aux cellules selon leur appartenance à une AMP
  my_status <- ifelse(
    as.data.frame(r_proj, cells = T, xy = T)$cell %in% 
      as.numeric(row.names(d[d$MPAnationalStatus == 1, ])), cell_stat, 1
  )
  
  # Association d'un coût à la cellule selon une pression
  
  my_cost <- if (is.double(cost_threshold)) {
    spatRast_cost_thresh <- ifel(
      spatRast_cost < max(values(spatRast_cost), na.rm = T)*(1-cost_threshold), 0, 1
    )
    x11(); plot(spatRast_cost_thresh)
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
  
  if (write_file){
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