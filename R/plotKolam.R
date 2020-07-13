plotKolam <- function(kolamObject, loop_col = TRUE, delay = NA, pulli = TRUE, arrow = TRUE, lwd = 2, cex = 1){
  windowMin <- -1
  windowMax <- kolamObject$pulli+2

  if (loop_col) {
    plotLoop(gestures = kolamObject$sequences[[1]], xStart = kolamObject$initialization[1, "x_init"],
             yStart = kolamObject$initialization[1, "y_init"],
             headingStart = kolamObject$initialization[1, "heading_init"], delay = delay, col = 1, windowMin = windowMin, windowMax = windowMax, pulli = pulli, arrow = arrow, lwd = lwd, cex = cex)
    if (length(kolamObject$sequences) > 1) {
      for (i in 2:length(kolamObject$sequences)) {
        plotLoop(gestures = kolamObject$sequences[[i]], xStart = kolamObject$initialization[i, "x_init"],
                 yStart = kolamObject$initialization[i, "y_init"],
                 headingStart = kolamObject$initialization[i, "heading_init"], delay = delay,
                 col = brewer.pal(n = 12, name = 'Paired')[ifelse(ifelse(i > 12, i%%12, i) == 0, 1, ifelse(i > 12, i%%12, i))], add = TRUE, arrow = arrow, lwd = lwd, cex = cex)
      }
    }
  }
  else{
    plotLoop(gestures = kolamObject$sequences[[1]], xStart = kolamObject$initialization[1, "x_init"],
             yStart = kolamObject$initialization[1, "y_init"],
             headingStart = kolamObject$initialization[1, "heading_init"], delay = delay, col = 1, windowMin = windowMin, windowMax = windowMax, pulli = pulli, arrow = arrow, lwd = lwd, cex = cex)
    if (length(kolamObject$sequences) > 1) {
      for (i in 2:length(kolamObject$sequences)) {
        plotLoop(gestures = kolamObject$sequences[[i]], xStart = kolamObject$initialization[i, "x_init"],
                 yStart = kolamObject$initialization[i, "y_init"],
                 headingStart = kolamObject$initialization[i, "heading_init"], delay = delay, col = 1, add = TRUE, arrow = arrow, lwd = lwd, cex = cex)
      }
    }
  }

}
