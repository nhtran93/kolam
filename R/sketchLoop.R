sketchLoop <- function(gestures, xStart = 0, yStart = 0.5, headingStart = 45) {
  lexicon <- c("o1", "o2", "o3", "o4",
               "d1", "d2r", "d2l", "d3r", "d3l", "d4r", "d4l",
               "od1", "od2", "od3", "od4",
               "do1", "do2", "do3", "do4",
               "t1", "t2", "t3", "t4",
               "t1r", "t1l", "t2r", "t2l", "t3r", "t3l", "t4r", "t4l",
               "c1", "c2", "c3", "h3l", "h3r", "op")
  if (any(!gestures %in% lexicon)) stop("some gestures aren't in the lexicon!")
  if (length(gestures) == 1) {
    if (gestures == "o1") out <- o1(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "o2") out <- o2(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "o3") out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "o4") out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)

    if (gestures == "t1") out <- t1(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t2") out <- t2(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t3") out <- t3(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t4") out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)

    if (gestures == "d1") out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "d2r") out <- d2Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "d2l") out <- d2Left(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "d3r") out <- d3Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "d3l") out <- d3Left(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "d4r") out <- d4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "d4l") out <- d4Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

    if (gestures == "t1r") out <- t1Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t1l") out <- t1Left(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t2r") out <- t2Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t2l") out <- t2Left(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t3r") out <- t3Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t3l") out <- t3Left(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t4r") out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "t4l") out <- t4Left(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "c1") out <- c1(xStart = xStart, yStart = yStart, headingStart = headingStart, method = "catmull")
    if (gestures == "c2") out <- c2(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "c3") out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "op") out <- op(xStart = xStart, yStart = yStart, headingStart = headingStart)

    if (gestures == "h3l") out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart)
    if (gestures == "h3r") out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart)
  } else {
    out <- sketchLoop(gestures[1], xStart = xStart, yStart = yStart, headingStart = headingStart)
    for (i in 2:length(gestures)) {
      one_more <- sketchLoop(gestures[i], xStart = out$xEnd, yStart = out$yEnd, headingStart = out$headingEnd)
      out$xEnd <- one_more$xEnd
      out$yEnd <- one_more$yEnd
      out$headingEnd <- one_more$headingEnd
      out$path <- rbind(out$path, one_more$path)
      out$controlPoints <- rbind(out$controlPoints, one_more$controlPoints)
    }
  }
  return(out)
}


plotLoop <- function(gestures, xStart = 0, yStart = 0.5, headingStart = 45, delay = NA, add = FALSE, col = "black", windowMin = NA, windowMax = NA, Netlogo = TRUE, pulli = TRUE, arrow = TRUE, lwd = 2, cex = 1){
  if (Netlogo) {
    if (headingStart == 90 | headingStart == 270 | headingStart == 135 | headingStart == 315 | headingStart == 90 | headingStart == 270 | headingStart == 0 | headingStart == 180) {
      angle <- c(0, 180, 90, 270, 135, 315)
      test <- which(angle == headingStart)
      if (test == 1) {
        headingStart <- 90
      }
      if (test == 2) {
        headingStart <- 270
      }
      if (test == 3) {
        headingStart <- 0
      }
      if (test == 4) {
        headingStart <- 180
      }
      if (test == 5) {
        headingStart <- 315
      }
      if (test == 6) {
        headingStart <- 135
      }
    }
  }

  out <- sketchLoop(gestures, xStart = xStart, yStart = yStart, headingStart = headingStart)
  if (is.na(windowMin) & is.na(windowMax)) {
    windowMin <- round(min(c(out$path[, 1], out$path[, 2])))-2
    windowMax <- round(max(c(out$path[, 1], out$path[, 2])))+2
  }

  if (add == FALSE) plot.new( )
  if (is.na(delay)){
    if (add == FALSE){
      plot.window(xlim = c(windowMin, windowMax), ylim = c(windowMin, windowMax))
      if (pulli) {
        points(expand.grid(c(windowMin:windowMax), c(windowMin:windowMax)), pch = 20, col = "gray", cex = cex)
      }
    }
      points(out$path[,1], out$path[,2], type = "l", lwd = lwd, col = col)
      if (arrow) {
        shape::Arrowhead(x0 = out$xEnd, y0 = out$yEnd, angle = out$headingEnd, arr.width = 0.1,
                         arr.length = 0.1, arr.type = "triangle")
      }
  }
  if (!is.na(delay)) {
      plot_points <- round(seq(1, nrow(out$path), by = 3))
      for (i in plot_points) {
        if (add == FALSE){
          plot.window(xlim = c(windowMin, windowMax), ylim = c(windowMin, windowMax))
          if (pulli) {
            points(expand.grid(c(windowMin:windowMax), c(windowMin:windowMax)), pch = 20, col = "gray")
          }
        }
        points(out$path[1:i, 1], out$path[1:i, 2], type = "l", lwd = lwd, col = col)
        Sys.sleep(delay)
      }
      if (arrow) {
        shape::Arrowhead(x0 = out$path[i, 1], y0 = out$path[i, 2], angle = out$headingEnd, arr.width = 0.1,
                       arr.length = 0.1, arr.type = "triangle", lcol = col)
      }
  }
  return(out)
}
