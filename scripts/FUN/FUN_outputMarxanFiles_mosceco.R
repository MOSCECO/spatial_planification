outputMarxanFiles_mosceco <- function(spatial_raster, path_inout) {

  mesh <- as.data.frame(spatial_raster, cells = T, xy = T)

  ##Traitement des données Marxan
  input  <- path_inout$input
  output <- path_inout$output

  ##Tables issues de l'input de Marxan
  Spec   <- fread(here(input,"ConsFeaFile.txt"))
  PU     <- fread(here(input,"PlanUnFile.txt"))
  PUvsPR <- fread(here(input,"PUvsCFFile.txt"))

  # Importation et calcul de la somme des solutions
  temp_runs <- list.files(output, pattern = "temp_r.+", full.names = T)

  # Appliquer "fread" d'un seul coup bousille la RAM, tentative avec une boucle
  # "for" :
  temp_tb <- temp_runs[[1]] %>%
    fread(header = T, sep = ",") %>%
    select(number = solution)
  temp_tb$number <- rep(0, nrow(temp_tb))

  for (f in temp_runs) {
    o <- fread(f, header = T, sep = ",") %>% select(solution)
    temp_tb <- cbind(temp_tb, o)
    temp_tb$number <- rowSums(temp_tb) %>% as.integer()
  }

  tr <- cbind(
    temp_runs[[1]] %>% fread(header = T, sep = ",") %>% select(planning_unit),
    number = temp_tb$number
  )

  # Organisation de la table sommée dans l'ordre des plannin_units
  tr <- tr[order(tr), ]
  sols <- dir(output, pattern = "*_r")
  NRep <- nreps_total

  # Ajout de chacun des résultats des runs en une colonne dans la table finale
  for(i in 1:NRep){
    tr[
      ,
      paste0("Sol",i) := read.table(
        here(output, sols[i]), header = T, sep = ","
      )[,2]
    ]
  }

  # Remplacement des NA par 0 dans toutes les colonnes
  for(j in 1:ncol(tr)) {
    set(tr, which(is.na(tr[[j]])), j, 0)
  }

  # Sélection des unités de planification présentes dans la table d'équivalence
  # espèces/unité de planification
  # dim(tr)
  tr <- tr[planning_unit %in% PUvsPR$pu]
  # dim(tr)

  # Ajout des caractéristique des unités de planification
  tr <- tr %>%
    left_join(PU %>% select(cell, planning_unit = id))

  # Correspondance entre la grille initiale et la table d'irremplaçabilité
  spatial_raster_out <- mesh %>%
    left_join(dplyr::select(tr, cell, irremplacabilite = number)) %>%
    select(-cell) %>%
    rast()

  # sauvegarde ----
  file_name <- paste("output", "marxan", sep = "_")
  write.csv(
    mesh,
    here(output, paste0(file_name, ".csv")),
    row.names = F
  )
  file_name <- paste("output", "marxan", "spatrast", sep = "_")
  writeRaster(
    spatial_raster_out,
    here(output, paste0(file_name, ".tif")),
    overwrite = T
  )

  return(spatial_raster_out)
}
