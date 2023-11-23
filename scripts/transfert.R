# functions
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/scripts/FUN /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts"
system(cmd)
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/scripts/FUN/* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/FUN/"
system(cmd)

# polygones îles ----
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/shp /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw"
system(cmd)

# occurrences d'espèces
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/tidy/occ/list_occ_rasterized.rds /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/occ/"
system(cmd)

# climatologies avec salinités hybrides ----
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/climatologies_spatRaster /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/"
system(cmd)

# Modélisations des distributions
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/analysis/compilation/* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/mae"
system(cmd)
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/scripts/popa_* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/"
system(cmd)

# Logiciel MARXAN
makeMyDir(here("scripts", "Marxan"))
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/data/Marxan* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/Marxan/"
system(cmd)
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/data/input.dat /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/"
system(cmd)

# Routine Marxan
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/scripts/*.R /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/"
system(cmd)
