conservationFeature <- function(
    species,
    target,
    target_cell = NA,
    spf,
    path_inout,
    write_file = TRUE
){

  spf <- if(!is.double(spf)) {
    ifelse(
      species %in% spf,
      1000,
      100
    )
  } else {
    spf
  }

  target <- if(!is.double(target)) {
    x <- rep(1, length(species))
    y <- replace(x, species %in% target, target_cell)
    unlist(y, use.names = F)
  } else {
    target
  }

  ConsFeaFile <- cbind(
    id     = which(cl$Species %in% gsub(" ", "_", species)),
    name   = species,
    target = target,
    spf    = spf
  ) %>%
    as.data.frame()

  if (write_file){

    write.table(
      ConsFeaFile,
      file      = here(path_inout$input, "ConsFeaFile.txt"),
      row.names = F
    )

    write.table(
      ConsFeaFile[, names(ConsFeaFile) != 'name'],
      file = here(path_inout$input, "spec.dat"),
      sep = ",",
      row.names = F,
      quote = F)
  }

  return(ConsFeaFile)
}
