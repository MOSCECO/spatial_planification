MarxanFilesGenerator <- function(
  mesh_resolution, 
  iso3_country, 
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
  
  path_resolution <- here("data", "analysis", "marxan", isod, reso)
  dir.create(path_resolution, showWarnings = F)
  
  path_dataprofil <- here(path_resolution, switch_data_profile)
  dir.create(path_dataprofil, showWarnings = F)
  
  path_inout <- list(
    path_input  = here(path_dataprofil, "input"), 
    path_output = here(path_dataprofil, "output")
  )
  
  path_inout %>% lapply(
    overwriteFile
  )
  
  file.copy(here("input.dat"),      path_dataprofil)
  file.copy(here("Marxan_x64.exe"), path_dataprofil)
  
  ##############################################################################
  
  mesh <- mesh_islands[[paste0(iso3_country, reso)]]
  mapis <- maps_islands[[iso3_country]]
  datis <- datasets[[data_profile]] %>% 
    filter(
      country == iso2 &
        in_mask
    ) %>% 
    st_transform(crs = st_crs(mesh))
  
  ##############################################################################
  
  # ConsFeaFile ----
  source(here("scripts", "FUN_conservationFeature.R"))
  
  ConsFeaFile <- conservationFeature(
    df = datis, target = 1, spf = 1000, path_inout = path_inout
  )
  

  ##############################################################################
  
  # PuVSCFFile ----
  source(here("scripts", "FUN_unitsVScf.R"))
  
  puvspr <- unitsVScf(
    datum = datis, mesh = mesh, path_inout = path_inout
  )
  
  ##############################################################################
  
  # PlanUnFile ----
  source(here("scripts", "FUN_planningUnits.R"))
  
  PlanUnFile <- planningUnits(
    mesh = mesh, mapis = mapis, path_inout = path_inout
  )
  
}
