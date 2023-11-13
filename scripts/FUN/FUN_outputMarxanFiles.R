outputMarxanFiles <- function(
  iso3_country, 
  mesh_resolution, 
  data_profile
  ) {
  
  reso <- paste0(mesh_resolution, "X", mesh_resolution)
  info <- info_islands[[grep(iso3_country, names(info_islands))]]
  iso3 <- info["iso3"] %>% as.character()
  iso2 <- info["iso2"] %>% as.character()
  isod <- info["double"] %>% as.character()
  
  switch_data_profile <- switch(
    EXPR = data_profile,
    full = "01_toutes_occurrences",
    part = "02_gastro_bival_cephalo",
    detr = "03_determinantes_znieff",
  )
  
  path_resolution <- here("figures", "marxan", isod, reso)
  dir.create(path_resolution, showWarnings = F)
  
  path_dataprofil <- here(path_resolution, switch_data_profile)
  dir.create(path_dataprofil, showWarnings = F)
  
  mesh <- mesh_islands[[paste0(iso3_country, reso)]] %>% 
    add_column(pu = 1:nrow(.))
  mapis <- maps_islands[[iso3_country]]
  
  figures_categories_names %>% lapply(
    function(x) {
      path_newfile <- here(path_dataprofil, x)
      overwriteFile(path_newfile)
    }
  )
  
  path_marxan <- here::here(
    "data", "analysis", "marxan", 
    isod, reso, switch_data_profile
  )
  
  input  <- here(path_marxan, "input")
  output <- here(path_marxan, "output")
  
  ##Traitement des données Marxan
  
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
  
  SPR <- dcast(data = PUvsPR, formula = pu ~ species)
  sr <- SPR %>% dplyr::select(!1)
  sr[sr > 1] <- 1
  sr <- rowSums(sr)
  SPR <- SPR %>% dplyr::select(pu = 1) %>% cbind(SPR = sr)
  
  points_occ <- datasets[[data_profile]] %>% 
    filter(in_mask & country == iso2)
  
  
  
  mesh <- mesh %>%
    left_join(SPR) %>%
    left_join(dplyr::select(temp_ssoln, 
                            pu = planning_unit, 
                            IRR = number)) %>% 
    add_column(
      OCC = lengths(
        st_intersects(
          mesh, 
          points_occ %>% st_transform(crs = st_crs(mesh))
        )
      )
    )
  
  if (iso3 %in% names(stations)) {
    points_stn <- stations_sf[[iso3]] %>% 
      st_transform(crs = st_crs(mesh)) %>% 
      st_intersection(mesh)
    
    mesh <- mesh %>% 
      add_column(
        ECH = lengths(
          st_intersects(
            mesh, 
            points_stn %>% st_transform(crs = st_crs(mesh))
          )
        )
      )
    
    mesh$ECH[mesh$ECH == 0] <- NA
  }
  
  mesh$OCC[mesh$OCC == 0] <- NA
  
  # saving ----
  file_name <- paste0("output_data_", iso3, reso, "-", data_profile)
  to_save <- mesh %>% 
    as.data.frame() %>% 
    select(-geometry)
  
  write.csv(to_save, 
            here(path_dataprofil, paste0(file_name, ".csv")), 
            row.names = F)
  st_write(mesh, 
           here(path_dataprofil, paste0(file_name, ".shp")), 
           append = F)
  
}