readKolam <- function(file = NA, data = NA, read_csv = FALSE){
  if (read_csv) {
    data <- read.csv(file = file, stringsAsFactors = FALSE)
  }
  data <- data %>% dplyr::rename(png_hash = kolam_png_hash, notes = kolam_transcriber_comments)

  kolams <- lapply(1:nrow(data), function(x) {loadKolam(data[x, ])})
  return(kolams)
}
