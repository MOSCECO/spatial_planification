outputMarxanFiles_ensta <- function(spatial_raster, path_inout) {

  mesh <- as.data.frame(spatial_raster, cells = T, xy = T)
  ##Traitement des données Marxan
  input  <- path_inout$input
  output <- path_inout$output
  ##Tables issues de l'input de Marxan
  Spec   <- fread(here(input,"ConsFeaFile.txt"))
  PU     <- fread(here(input,"PlanUnFile.txt"))
  PUvsPR <- fread(here(input,"PUvsCFFile.txt"))
  #BlockDef<-fread(input,"BloDefFile.txt",))

  temp_sum    <- here(output, "temp_sum.txt") %>%
    read.delim(sep = ",") %>% as.data.table()
  temp_ssoln  <- here(output, "temp_ssoln.txt") %>%
    read.delim(sep = ",") %>% as.data.table()
  temp_best   <- here(output, "temp_best.txt") %>%
    read.delim(sep = ",") %>% as.data.table()
  temp_mbvest <- here(output, "temp_mvbest.txt") %>%
    read.delim(sep = ",") %>% as.data.table()
  # temp_sen    <- read.delim("temp_sen.dat", sep = ",")

  temp_ssoln <- temp_ssoln[order(temp_ssoln),]
  sols <-dir(output, pattern = "*_r")

  NRep <- 100

  for(i in 1:NRep){
    temp_ssoln[, paste0("Sol",i) := read.table(here(output, sols[i]),
                                               header = T, sep = ",")[,2]]
  }

  for(j in 1:ncol(temp_ssoln)){
    set(temp_ssoln, which(is.na(temp_ssoln[[j]])), j, 0)
  }

  temp_ssoln <- temp_ssoln[planning_unit %in% PUvsPR$pu]
  temp_ssoln2 <- temp_ssoln %>%
    left_join(PU %>% select(cell, planning_unit = id))
  spatial_raster_out <- mesh %>%
    left_join(
      dplyr::select(temp_ssoln2, cell, irremplacabilite = number)
    ) %>%
    select(-cell) %>%
    rast()
  # x11(); plot(spatial_raster_out)
  # saving ----
  file_name <- paste0("output", "marxan", sep = "_")
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
