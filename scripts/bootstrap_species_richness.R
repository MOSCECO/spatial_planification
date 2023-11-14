# diminution des outliers de richesses spécifiques
sp <- popaPlot(
  projRasters        = mae,
  type               = "presence_absence",
  superfamily        = "all",
  ensemble_algorithm = "wmean",
  projection_time    = "current",
  do_plot            = FALSE,
  do_plot_combine    = FALSE
)
sp_pa <- sp$species

# Sélection des profondeurs 0-150m
dmask <- climosaic$depth
dmask <- ifel(dmask > -150, 1, NA)
sp_pa <- sp_pa * dmask
sp_cb <- sp$combine * dmask

# Carte initiale
x11(); plot(sp_cb)

# bootstrap
n_resamp <- 10
bootstrap <- lapply(
  X   = seq(1, n_resamp),
  FUN = \(x) {
    sample(
      x       = names(sp_pa),
      size    = length(names(sp_pa)),
      replace = TRUE
    )
  }
)
bootstrap <- bootstrap %>% lapply(\(nsp) unique(nsp))
bootstrap_raster <- bootstrap %>% lapply(\(nsp) app(subset(sp_pa, nsp), sum))
bootstrap_raster <- Mapply(
  \(r, id) {names(r) <- paste0("run", id) ; return(r)},
  bootstrap_raster,
  1:n_resamp
)
bsr <- Reduce(c, bootstrap_raster)
writeRaster(bsr, here("data", "tidy", "bsr.tif"), overwrite = T)
bsr_q02.5 <- app(bsr, \(x) quantile(x, probs = 0.025, na.rm = T))
bsr_q50.0 <- app(bsr, \(x) quantile(x, probs = 0.500, na.rm = T))
bsr_q97.5 <- app(bsr, \(x) quantile(x, probs = 0.975, na.rm = T))
BSR <- c(sp$combine, bsr_q02.5, bsr_q50.0, bsr_q97.5)
names(BSR) <- c("init", "q02.5", "q50.0", "q97.5")
writeRaster(BSR, here("data", "tidy", "BSR.tif"), overwrite = T)
x11(); plot(BSR$init)
x11(); plot(BSR$q02.5)
x11(); plot(BSR$q50.0)
x11(); plot(BSR$q97.5)
