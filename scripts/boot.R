# boot data_occ_preparation

# libraries ----
libs_to_call <- list(

  "data.table",
  "devtools",
  "ggnewscale",
  "ggplot2",
  "ggpubr",
  "ggthemes",
  "here",
  "patchwork",
  "purrr",
  "raster",
  "reshape2",
  "sf",
  "stars",
  "stringr",
  "terra",
  "tidyverse",
  "vegan",
  "worrms"

)

# library calls
lapply(

  libs_to_call,

  function(i) {

    bool <- is.element(i, .packages(all.available = TRUE))

    if (!bool) {
      install.packages(i, dependencies = T)
    }

    library(i, character.only = TRUE)

  }
)

# remote libraries (github)
# Sys.getenv("GITHUB_PAT")
# Sys.unsetenv("GITHUB_PAT")
# Sys.getenv("GITHUB_PAT")

# remote libraries ----
# github_accounts <- as.list(
#   rep("SantanderMetGroup", 6)
# )
#
# remote_libs_to_call <- list(
#   "loadeR.java",
#   "climate4R.UDG",
#   "loadeR",
#   "transformeR",
#   "visualizeR",
#   "downscaleR"
# )
#
# mapply(
#   function(pckg, usr) {
#
#     bool <- is.element(pckg, .packages(all.available = TRUE))
#
#     if (!bool) {
#       path_url <- paste0(usr, "/", pckg)
#       print(path_url)
#       devtools::install_github(path_url)
#     }
#
#     library(pckg, character.only = TRUE)
#
#   },
#   remote_libs_to_call,
#   github_accounts,
#   SIMPLIFY = FALSE
# )

# functions
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/scripts/FUN /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts"
# system(cmd)
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/scripts/FUN/* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/FUN/"
# system(cmd)
lapply(
  list.files(
    here("scripts", "FUN"),
    full.names = T
  ),
  source
)

# chemin vers le dossier des projets R
pp <- "/home/borea/Documents/mosceco/r_projects/MOSCECO_L3"

# shapefiles ----
sf::sf_use_s2(FALSE)
wgs <- "EPSG:4326"
utm20n <- "EPSG:32620"
islands <- c("GLP", "MTQ")
names(islands) <- islands
superFamilies <- c("Majoidea", "Muricoidea")
names(superFamilies) <- superFamilies
Taxa <- c("Majoidea", "Muricidae")
names(Taxa) <- Taxa
taxa <- c("majo", "muri")
names(taxa) <- taxa
colors_taxa <- c("#d04c4e", "#5765b4")
names(colors_taxa) <- superFamilies

# directories ----
makeMyDir(here("data", "raw", "occ"))

# polygones îles ----
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/shp /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw"
# system(cmd)
maps <- list.files(
  here("data", "raw", "shp", "polygones_iles"),
  pattern = "*.shp",
  full.names = T
) %>%
  lapply(st_read)
names(maps) <- islands

# stations & évènements de collectes
stations <- readRDS(
  here("data", "raw", "shp", "stations_me_sf.rds")
)
stations_nearest <- readRDS(
  here("data", "raw", "shp", "stations_ant_nearest.rds")
)

# masses d'eau de la DCE et artificielles
me <- readRDS(
  here("data", "raw", "shp", "ART_masses_d-eaux", "me.rds")
)

# occurrences d'espèces
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/tidy/occ/list_occ_rasterized.rds /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/occ/"
# system(cmd)
pa <- readRDS(here("data", "raw", "occ", "list_occ_rasterized.rds"))

# Espèces modélisées
species <- tibble(
  superFamily = c(rep("Majoidea", 8), rep("Muricoidea", 10)),
  species     = c(
    "Amphithrax hemphilli",
    "Macrocoeloma nodipes",
    "Mithraculus coryphe",
    "Mithraculus forceps",
    "Mithrax pleuracanthus",
    "Omalacantha bicornuta",
    "Stenorhynchus seticornis",
    "Teleophrys ruber",
    "Claremontiella nodulosa",
    "Coralliophila galea",
    "Coralliophila salebrosa",
    "Favartia alveata",
    "Favartia varimutabilis",
    "Phyllonotus pomum",
    "Siratus consuelae",
    "Stramonita rustica",
    "Trachypollia didyma",
    "Vasula deltoidea",
    NULL
  )
)

# climatologies avec salinités hybrides ----
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/climatologies_spatRaster /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/"
# system(cmd)
climatologies <- lapply(
  list.files(
    here("data", "raw", "climatologies_spatRaster"),
    full.names = T,
    pattern = "updated_so"
  ),
  rast
)
names(climatologies) <- islands

# aggrégation en mosaïque
climosaic <- mapply(
  \(nx, ny) {
    x <- climatologies$GLP[[nx]]
    y <- climatologies$MTQ[[ny]]
    terra::mosaic(x, y)
  },
  names(climatologies$GLP),
  names(climatologies$MTQ),
  SIMPLIFY = F,
  USE.NAMES = T
)
climosaic <- Reduce(c, climosaic)

# polygones et étendues îles/antilles
maps_marxan     <- maps
maps_marxan$ANT <- Reduce(rbind, maps)
maps_marxan     <- maps_marxan[sort(names(maps_marxan))]

ext_marxan <- list(
  ANT = ext(climosaic$depth),
  GLP = ext(climatologies$GLP$depth),
  MTQ = ext(climatologies$MTQ$depth)
)

# modélisations de distributions
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/analysis/compilation/presence_absence /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/mae"
# system(cmd)
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/analysis/compilation/adequation_environnementale /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/mae"
# system(cmd)
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/scripts/popa_* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/"
# system(cmd)
popa_path <- here("data", "raw", "mae")
source(here("scripts", "popa_import.R"))

# Logiciel MARXAN
# makeMyDir(here("scripts", "Marxan"))
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/data/Marxan* /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/Marxan/"
# system(cmd)
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/data/input.dat /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/data/raw/"
# system(cmd)

# Routine Marxan
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_miscellanous/TP_ENSTA_2023_Planification_Spatiale/scripts/*.R /home/borea/Documents/mosceco/r_projects/MOSCECO_L3/spatial_planification/scripts/"
# system(cmd)
