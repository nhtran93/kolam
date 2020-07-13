sketchKolam <- function(kolamObject_list){
  output <- list()
  for (i in 1:length(kolamObject_list)) {
    kolam <- extractControlpoints(kolamObject_list[[i]])
    output[[paste("Kolam", i, sep = "")]] <- kolam
  }

  return(output)
}
