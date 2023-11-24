fragSpatialRaster <- function(spatial_raster, dvd) {
  # sr <- spatial_raster <- bsr
  # dvd <- 100
  nr <- dim(sr)[1]
  nc <- dim(sr)[2]
  chunks_rows <- split(1:nr, ceiling(seq_along(1:nr)/dvd))
  chunks_cols <- split(1:nc, ceiling(seq_along(1:nc)/dvd))
  sr_chunks <- lapply(
    chunks_rows,
    # nr <- chunks_rows[[1]]
    # nc <- chunks_cols[[1]]
    \(nr) lapply(chunks_cols, \(nc) sr[nr, nc, , drop = FALSE])
  )
  sr_chunks <- Reduce(
    append, lapply(1:length(sr_chunks), \(x) pluck(sr_chunks, x))
  )
  names(sr_chunks) <- 1:length(sr_chunks)
  return(sr_chunks)
}
