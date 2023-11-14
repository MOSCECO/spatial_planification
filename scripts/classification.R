# classification
aphia <- species$species %>% lapply(wm_name2id) %>% unlist(use.names = F)
ttree <- aphia %>% lapply(wm_classification)
ttree <- lapply(
  ttree,
  \(d) {
    d$rank <- factor(d$rank, levels = d$rank)
    vec <- d$scientificname
    names(vec) <- d$rank
    return(t(vec) %>% as_tibble())
  }
)
classi <- Reduce(intersect, lapply(ttree, names))
ttree <- lapply(ttree, select, all_of(classi))
cl <- Reduce(rbind, ttree)
write.csv(cl, here("data", "tidy", "classification.csv"), row.names = F)
