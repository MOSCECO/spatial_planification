unitsVScf <- function(spatial_raster, path_inout, wf = TRUE) {
  
  cpx <- mapply(
    \(d, sn) {
      out <- cbind(
        species = which(cl$Species %in% sn), 
        as.data.frame(d, cells = T), 
        pu = 1:nrow(as.data.frame(d))
      ) %>% 
        filter(get(sn) == 1)
      names(out)[3] <- "amount"
      out %>% select(1, 2, 4, 3)
    }, 
    as.list(spatial_raster), 
    names(spatial_raster), 
    SIMPLIFY = F, 
    USE.NAMES = T
  )
  
  cpx.df <- do.call(rbind, cpx) %>% 
    group_by(pu) %>% 
    arrange(.by_group = T)
  
  if (wf){
    write.table(
      cpx.df, 
      file = here(path_inout$input, "PUvsCFFile.txt"), 
      row.names = F
    )
    
    write.table(
      cpx.df[order(cpx.df$pu), c("species", "pu", "amount")], 
      file = here(path_inout$input, "puvspr.dat"), 
      sep = ",", 
      row.names = F, 
      quote = F
    )
  }
  
}