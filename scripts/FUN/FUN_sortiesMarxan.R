# FUN Sorties Marxan : sauvegarde de la table de maillage et des cartes de richesse spécifique
#  et irremplçabilité

sortiesMarxan <- function(pays, r, comp){
  
  reso <- paste0(r, "x", r)
  
  if (pays == "GP"){
    PAYS <- "guadeloupe"
    codePays <- "GLP"
    titrePays <- "Guadeloupe"
  }else if (pays == "MQ"){
    PAYS <- "martinique"
    codePays <- "MTQ"
    titrePays <- "Martinique"
  }else if (pays == "MF"){
    PAYS <- "saintmartin"
    codePays <- "MAF"
    titrePays <- "Saint-Martin"
  }
  
  if (comp == "complet"){
    titre <- "Mollusques marins\n"
  }else if (comp == "classes"){
    titre <- "Gasteropoda, Bivalvia\net Cephalopoda"
  }else if (comp == "determinantes"){
    titre <- "espèces déterminantes\nde Mollusques marins"
  }
  
  # dossier de sortie
  nomDoss <- paste(PAYS, reso, comp, sep = "_")
  chemin <- paste0("~/vacation_ZNIEFF/data/cartes/", nomDoss, "/")
  dir.create(chemin)
  cheminRS <- paste0(chemin, "RS/")
  cheminIR <- paste0(chemin, "IR/")
  dir.create(cheminRS)
  dir.create(cheminIR)
  
  carte <- st_read(dsn = "~/vacation_ZNIEFF/data", layer = paste0("polygones_", PAYS))
  
  # données
  maillageConcis <- st_read(dsn = paste0("~/vacation_ZNIEFF/data/zonage_marin/", PAYS), 
                            layer = paste0("mailles", codePays, reso, comp))
  
  
  ##Changement répertoire
  PATH <- paste0("~/Marxan_v406/gregoire_maniel/znieff_antilles/", PAYS, reso, comp)
  setwd(PATH)
  path <- PATH
  
  
  ################################################################################
  ##TaxRef
  # load("~/Documents/Boulot/Extrac_INPN/TAXREFv12_2018-10-24.RData")
  # taxref14 <- readRDS("~/TAXREF/taxref14.rds")
  
  ################################################################################
  
  input  <- paste0(PATH,"/input/")
  output <- paste0(PATH,"/output/")
  
  ##Traitement des données Marxan
  
  ##Tables issues de l'input de Marxan                                     
  Spec   <- fread(paste0(input,"ConsFeaFile.txt"))
  PU     <- fread(paste0(input,"PlanUnFile.txt")) 
  PUvsPR <- fread(paste0(input,"PUvsCFFile.txt"))
  #BlockDef<-fread(input,"BloDefFile.txt",))
  
  setwd(output)
  
  temp_sum    <- as.data.table(read.delim("temp_sum.txt", sep = ","))
  temp_ssoln  <- as.data.table(read.delim("temp_ssoln.txt", sep = ","))
  temp_best   <- as.data.table(read.delim("temp_best.txt", sep = ","))
  temp_mbvest <- as.data.table(read.delim("temp_mvbest.txt", sep = ","))
  # temp_sen    <- read.delim("temp_sen.dat", sep = ",")
  
  temp_ssoln <- temp_ssoln[order(temp_ssoln),]
  sols <-dir(output, pattern = "*_r")
  
  NRep <- 100
  
  for(i in 1:NRep){
    temp_ssoln[, paste0("Sol",i) := read.table(paste0(output, sols[i]), header = T, sep = ",")[,2]]
  }
  
  for(j in 1:ncol(temp_ssoln)){
    set(temp_ssoln, which(is.na(temp_ssoln[[j]])), j, 0)
  }
  
  temp_ssoln <- temp_ssoln[planning_unit %in% PUvsPR$pu]
  
  SR <- dcast(data = PUvsPR, formula = pu ~ species)
  sr <- SR %>% dplyr::select(!1)
  sr[sr > 1] <- 1
  sr <- rowSums(sr)
  SR <- SR %>% dplyr::select(maille = 1) %>% cbind(SR = sr)
  
  maillageConcis$geometry_sfc <- maillageConcis %>% geomToSFC()
  st_crs(maillageConcis$geometry_sfc) <- st_crs(maillageConcis)
  
  maillageConcis <- maillageConcis %>% left_join(SR) %>%
    left_join(dplyr::select(temp_ssoln, maille = planning_unit, IR = number))
  maillageConcisnona <- drop_na(maillageConcis)
  
  # I. Table ----
  write.csv(maillageConcis[,1:7], 
            paste0(chemin, "maillage_SR_IR_", PAYS, reso, comp, ".csv"), 
            row.names = F)
  
  if (pays == "MF"){
    diviseur <- 1.2
    eloi <- 8.5
  }else{
    diviseur <- 3/10
    eloi <- 0.5
  }
  
  # II. Graphiques ----
  #   1. Richesse Spécifique ----
  #     a) Indice ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$SR, alpha = 0.9), color = "white") +
    scale_fill_continuous("Richesse spécifique", type = "viridis", na.value = NA) +
    # geom_sf_text(data = maillageConcis, aes(label = SR), size = r*(diviseur)) +
    # geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de la richesse spécifique des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Occurrences compliées à partir des bases GBIF, OBIS, OpenObs et INVMAR") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminRS, "carte_RS_a_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     b) Indice + île ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$SR, alpha = 0.9), color = "white") +
    scale_fill_continuous("Richesse spécifique", type = "viridis", na.value = NA) +
    # geom_sf_text(data = maillageConcis, aes(label = SR), size = r*(diviseur)) +
    geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de la richesse spécifique des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Occurrences compliées à partir des bases GBIF, OBIS, OpenObs et INVMAR") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminRS, "carte_RS_b_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     c) Indice + valeur maille ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$SR, alpha = 0.9), color = "white") +
    scale_fill_continuous("Richesse spécifique", type = "viridis", na.value = NA) +
    geom_sf_text(data = maillageConcis, aes(label = SR), size = r*(diviseur)) +
    # geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de la richesse spécifique des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Occurrences compliées à partir des bases GBIF, OBIS, OpenObs et INVMAR") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminRS, "carte_RS_c_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     d) Indice + valeur maille + île ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$SR, alpha = 0.9), color = "white") +
    scale_fill_continuous("Richesse spécifique", type = "viridis", na.value = NA) +
    geom_sf_text(data = maillageConcis, aes(label = SR), size = r*(diviseur)) +
    geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de la richesse spécifique des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Occurrences compliées à partir des bases GBIF, OBIS, OpenObs et INVMAR") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminRS, "carte_RS_d_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     e) Indice + nom des mailles ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$SR, alpha = 0.9)) +
    scale_fill_continuous("Richesse spécifique", type = "viridis", na.value = NA) +
    geom_sf_text(data = maillageConcis, aes(label = pu), size = r*(diviseur)) +
    # ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
    # pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de la richesse spécifique des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Numéro des mailles") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ggsave(paste0(cheminRS, "carte_RS_nom_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #   2. Irremplaçabilité ----
  #     a) Indice ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$IR, alpha = 0.8)) +
    scale_fill_gradient("Irremplaçabilité\npour 100 tirages", low = "yellow", high = "red", na.value = NA) +
    # geom_sf_text(data = maillageConcis, aes(label = IR), size = r*(diviseur)) +
    # geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de l'irremplaçabilité des mailles des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Données générées par le logiciel Marxan à partir d'occurrences d'espèces") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminIR, "carte_IR_a_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     b) Indice + île ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$IR, alpha = 0.8)) +
    scale_fill_gradient("Irremplaçabilité\npour 100 tirages", low = "yellow", high = "red", na.value = NA) +
    # geom_sf_text(data = maillageConcis, aes(label = IR), size = r*(diviseur)) +
    geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de l'irremplaçabilité des mailles des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Données générées par le logiciel Marxan à partir d'occurrences d'espèces") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminIR, "carte_IR_b_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     c) Indice + valeur maille ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$IR, alpha = 0.8), color = "white") +
    scale_fill_gradient("Irremplaçabilité\npour 100 tirages", low = "yellow", high = "red", na.value = NA) +
    geom_sf_text(data = maillageConcis, aes(label = IR), size = r*(diviseur)) +
    # geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de l'irremplaçabilité des mailles des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Données générées par le logiciel Marxan à partir d'occurrences d'espèces") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminIR, "carte_IR_c_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     d) Indice + valeur maille + île ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$IR, alpha = 0.8), color = "white") +
    scale_fill_gradient("Irremplaçabilité\npour 100 tirages", low = "yellow", high = "red", na.value = NA) +
    geom_sf_text(data = maillageConcis, aes(label = IR), size = r*(diviseur)) +
    geom_sf(data = carte) +
    ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
                                      pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de l'irremplaçabilité des mailles des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Données générées par le logiciel Marxan à partir d'occurrences d'espèces") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminIR, "carte_IR_d_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
  
  #     e) Indice + nom des mailles ----
  ggplot() +
    geom_sf(data = maillageConcis$geometry_sfc, aes(fill = maillageConcis$IR, alpha = 0.8)) +
    scale_fill_gradient("Irremplaçabilité\npour 100 tirages", low = "yellow", high = "red", na.value = NA) +
    geom_sf_text(data = maillageConcis, aes(label = pu), size = r*(diviseur)) +
    # geom_sf(data = carte) +
    # ggspatial::annotation_north_arrow(height = unit(1, "cm"), width = unit(1, "cm"),
    # pad_x = unit(eloi, "cm"), pad_y = unit(0.5, "cm") ) +
    guides(alpha = guide_legend("Maille", override.aes=
                                  list(fill = "white", color = "black"))) +
    scale_alpha_continuous(labels = paste(r, "km")) +
    labs(x = "Longitude", y = "Latitude",
         title = paste("Carte de l'irremplaçabilité des mailles des", titre, "de", titrePays),
         caption = "Source maillage : https://inpn.mnhn.fr",
         subtitle = "Numéro des mailles") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave(paste0(cheminIR, "carte_IR_nom_", PAYS, reso, comp, ".png"),
         device = png, width = 2000, height = 2000, limitsize = F, res = 300)
}
