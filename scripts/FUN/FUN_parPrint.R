parPrint <- function(...) {
  system(sprintf('echo "\n%s\n"', paste0(..., collapse = "")))
}
