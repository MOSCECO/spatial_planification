outputMarxanGraphs <- function(
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
  switch_country <- switch(
    EXPR = iso3_country,
    GLP = "Guadeloupe",
    MAF = "Saint-Martin",
    MTQ = "Martinique",
  )
  
  path_to_figures_output <- here(
    "figures", "marxan", isod, reso, switch_data_profile
  )
  
  output_marxan <- list.files(
    path_to_figures_output, 
    pattern = ".shp", 
    full.names = T
  ) %>% 
    st_read()
  
  paths_to_figure_category <- figures_categories_names %>% lapply(
    function(x) {
      return(here(path_to_figures_output, x))
    }
  )
  nom <- names(output_marxan)
  categories <- nom[nchar(nom) == 3] %>% as.list()
  
  names(paths_to_figure_category) <- as.character(categories)
  
  categories %>% lapply(
    function(category){
      switch_figure_category <- switch(
        category, 
        SPR = "01_richesse_specifique", 
        IRR = "02_irremplacabilite",
        OCC = "03_nombre_occurrences",
        ECH = "04_echantillonnage"
      )
      switch_figure_title <- switch(
        category, 
        SPR = "de la richesse sp\u00e9cifique", 
        IRR = "de l\u0027irrempla\u00e7abilit\u00e9",
        OCC = "du nombre d\u0027occurrences",
        ECH = paste("du nombre d\u0027\u00e9v\u00e8nements de collecte",
                    "du Mus\u00e9um national d\u0027Histoire naturelle\n(MNHN)")
      )
      switch_figure_legend <- switch(
        category, 
        SPR = "Richesse\nsp\u00e9cifique", 
        IRR = "Irrempla\u00e7abilit\u00e9\npour 100 tirages",
        OCC = "Nombre d\u0027\noccurrences",
        ECH = "Nombre d\u0027\u00e9v\u00e8nements\nde collecte"
      )
      
      switch_dapr_title <- switch(
        data_profile, 
        full = "mollusques marins", 
        part = "Gastropoda, Bivalvia et Cephalopoda",
        detr = "esp\u00e8ces d\u00e9terminantes de mollusques marins",
      )
      
      # II. Graphiques ----
      #   1. Richesse Sp\u00e9cifique ----
      #     a) Indice ----
      sb  <- paste("Occurrences compil\u00e9es \u00e0 partir des bases GBIF,", 
                   "OBIS, OpenObs et INVMAR")
      sbe <- "Num\u00e9ro des mailles"
      cp  <- "Source maillage : https://inpn.mnhn.fr"
      
      if (category == "ECH") {
        sb <- paste("Occurrences compil\u00e9es \u00e0 partir du r\u00e9f\u00e9rentiel", 
                    "des campagnes du MNHN")
        cp <- paste0("Source maillage : https://inpn.mnhn.fr\n", 
                     "Source donn\u00e9es : https://expeditions.mnhn.fr/")
      }
      
      bool1 <- bool2 <- c(
        FALSE, 
        TRUE
      )
      
      mapis <- maps_islands[[iso3_country]]
      
      tabool <- expand.grid(bool1 = bool1, 
                            bool2 = bool2)
      tabool$tag <- letters[1:4]
      
      nstn_M <- max(output_marxan$ECH, na.rm = T)
      nstn_m <- min(output_marxan$ECH, na.rm = T)
      
      r <- mesh_resolution %>% as.numeric()
      
      graphs_znieff <- list()
      for (i in 1:nrow(tabool)){
        p <- ggplot() +
          geom_sf(
            data            = output_marxan, 
            aes_string(fill = category, alpha = 0.9), 
            color           = "black"
          ) +
          {
            if (category == "SPR") {
              scale_fill_continuous(
                switch_figure_legend, 
                type     = "viridis", 
                na.value = NA
              )
            }
          } +
          {
            if (category == "IRR") {
              scale_fill_gradient(
                switch_figure_legend, 
                low = "yellow", 
                high = "red", 
                na.value = NA)
            }
          } +
          {
            if (category == "OCC") {
              scale_fill_continuous(
                switch_figure_legend, 
                low = "lightblue", 
                high = "red",
                na.value = NA)
            }
          } +
          {
            if (category == "ECH") {
              scale_fill_gradient2(
                switch_figure_legend,
                low = "lightgreen", 
                mid = "blue", 
                high = "purple",
                na.value = NA,
                midpoint = (nstn_M + nstn_m) / 2, 
                breaks = seq(0, nstn_M, 10))
            }
          } +
          { 
            if (tabool$bool1[i]) {
              geom_sf_text(
                data             = output_marxan, 
                aes_string(label = category), 
                size             = r*(3/10))
            }
          } +
          { 
            if (tabool$bool2[i]) {
              geom_sf(
                data = mapis
              )
            }
          } +
          { 
            if (iso3 == "MAF") {
              ggspatial::annotation_north_arrow(
                height = unit(1, "cm"), 
                width = unit(1, "cm"),
                pad_x = unit(14, "cm"), 
                pad_y = unit(0.5, "cm") 
              )
            } else {
              ggspatial::annotation_north_arrow(
                height = unit(1, "cm"), 
                width = unit(1, "cm"),
                pad_x = unit(0.5, "cm"), 
                pad_y = unit(0.5, "cm") 
              )
            }
          } +
          guides(
            alpha = guide_legend(
              "Maille", 
              override.aes = list(
                fill = "white", 
                color = "black"
              )
            )
          ) +
          scale_alpha_continuous(
            labels = paste(mesh_resolution, "km")
          ) +
          labs(
            x = "Longitude", 
            y = "Latitude",
            title = paste(
              "Carte", 
              switch_figure_title, 
              "des", 
              switch_dapr_title, 
              "de", 
              switch_country
            ),
            caption = cp,
            subtitle = sb,
            tag = paste0(tabool$tag[i], ")")
          ) +
          theme_bw() +
          theme(
            panel.border     = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line        = element_line(colour = "black")
          )
        
        file_name <- here(
          paths_to_figure_category[[category]], 
          paste0(
            "carte-", 
            tabool$tag[i], 
            "_", 
            iso3, 
            reso,
            "-",
            data_profile, 
            ".png"
          ))
        
        ggexport(p, 
                 filename = file_name,
                 width = 6000, 
                 height = 4500, 
                 res = 720)
      }
      
      
      
      
      
      
      
      
      
      
      
      # e ----
      pe <- ggplot() +
        geom_sf(
          data = output_marxan, 
          aes_string(fill = category, alpha = 0.8)
        ) +
        {
          if (category == "SPR") {
            scale_fill_continuous(
              switch_figure_legend, 
              type     = "viridis", 
              na.value = NA
            )
          }
        } +
        {
          if (category == "IRR") {
            scale_fill_gradient(
              switch_figure_legend, 
              low = "yellow", 
              high = "red", 
              na.value = NA)
          }
        } +
        {
          if (category == "OCC") {
            scale_fill_continuous(
              switch_figure_legend,
              low = "lightblue", 
              high = "red",
              na.value = NA)
          }
        } +
        {
          if (category == "ECH") {
            scale_fill_gradient2(
              switch_figure_legend,
              low = "lightgreen", 
              mid = "blue", 
              high = "purple",
              na.value = NA,
              midpoint = (nstn_M + nstn_m) / 2, 
              breaks = seq(0, nstn_M, 10))
          }
        } +
        geom_sf_text(
          data = output_marxan, 
          aes(label = pu), 
          size = r*(3/10)
        ) +
        guides(
          alpha = guide_legend(
            "Maille", 
            override.aes = list(
              fill = "white", 
              color = "black")
          )
        ) +
        scale_alpha_continuous(
          labels = paste(mesh_resolution, "km")
        ) +
        labs(
          x = "Longitude", 
          y = "Latitude",
          title = paste(
            "Carte", 
            switch_figure_title, 
            "des", 
            switch_dapr_title, 
            "de", 
            switch_country
          ),
          caption = cp,
          subtitle = sbe, 
          tag = "e)"
        ) +
        theme_bw() + 
        theme(
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")
        )
      
      file_name <- here(
        paths_to_figure_category[[category]], 
        paste0(
          "carte-", 
          "e", 
          "_", 
          iso3, 
          reso,
          "-",
          data_profile, 
          ".png"
        ))
      
      print(pe)
      
      ggexport(pe, 
               filename = file_name,
               width = 6000, 
               height = 4500, 
               res = 720)
      
    }
  )
}