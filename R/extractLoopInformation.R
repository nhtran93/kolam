extractControlpoints <- function(kolamObject){
  out <- list()
  out[["controlpoints1"]] <- sketchLoop(gestures = kolamObject$sequences[[1]], xStart = kolamObject$initialization[1, "x_init"],
             yStart = kolamObject$initialization[1, "y_init"],
             headingStart = kolamObject$initialization[1, "heading_init"])$controlPoints
    if (length(kolamObject$sequences) > 1) {
      for (i in 2:length(kolamObject$sequences)) {
        out[[paste0("controlpoints", i)]] <- sketchLoop(gestures = kolamObject$sequences[[i]], xStart = kolamObject$initialization[i, "x_init"],
                 yStart = kolamObject$initialization[i, "y_init"],
                 headingStart = kolamObject$initialization[i, "heading_init"])$controlPoints
      }
    }
  # Extracting Path Information
  out1 <- list()
  out1[["path1"]] <- sketchLoop(gestures = kolamObject$sequences[[1]], xStart = kolamObject$initialization[1, "x_init"],
                                        yStart = kolamObject$initialization[1, "y_init"],
                                        headingStart = kolamObject$initialization[1, "heading_init"])$path
  if (length(kolamObject$sequences) > 1) {
    for (i in 2:length(kolamObject$sequences)) {
      out1[[paste0("path", i)]] <- sketchLoop(gestures = kolamObject$sequences[[i]], xStart = kolamObject$initialization[i, "x_init"],
                                                      yStart = kolamObject$initialization[i, "y_init"],
                                                      headingStart = kolamObject$initialization[i, "heading_init"])$path
    }
  }

  kolamObject[["control_points"]] <- out
  kolamObject[["path"]] <- out1

  return(kolamObject)
}


