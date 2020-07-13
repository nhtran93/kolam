c1 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
    xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")
  if (method == "bezier") {
      if (yStart %% 1 == 0.5) {
        if (headingStart == 45) {
          yStart <- yStart + 1
        }

        if (headingStart == 135) {
          yStart <- yStart + 1
        }
      }

      if (yStart %% 1 == 0) {
        if (headingStart == 45) {
          xStart <- xStart + 1
        }
      }
  }
  if (method == "catmull") {
    if (yStart %% 1 == 0.5) {
      if (headingStart == 45) {
        xStart <- xStart - 0.25
        yStart <- yStart + 0.25
      }
      if (headingStart == 135) {
        xStart <- xStart - 0.25
        yStart <- yStart + 0.25
      }
      if (headingStart == 225) {
        xStart <- xStart - 0.25
        yStart <- yStart - 0.75
      }
      if (headingStart == 315) {
        xStart <- xStart - 0.25
        yStart <- yStart - 0.75
      }
    }
    else{
      if (headingStart == 45) {
        xStart <- xStart + 0.25
        yStart <- yStart - 0.25
      }
      if (headingStart == 135) {
        xStart <- xStart - 0.75
        yStart <- yStart - 0.25
      }
      if (headingStart == 225) {
        xStart <- xStart - 0.75
        yStart <- yStart - 0.25
      }
      if (headingStart == 315) {
        xStart <- xStart + 0.25
        yStart <- yStart - 0.25
      }
    }
  }

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0, 0, 0, 0), ncol = 2, byrow = TRUE)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    if (xStart %% 1 == 0 & yStart %% 1 == 0.5){
      controlPoints <- matrix(c(0, 0,
                                0.2761424, 0,
                                0.5, -0.2238576,
                                0.6, -0.5,
                                0.5, -0.7761424,
                                0.2761424, -1,
                                0, -1,
                                -0.2761424, -1,
                                -0.5,  -0.7761424,
                                -0.5, -0.5,
                                -0.5, -0.2,
                                -0.3, -0.05,
                                0, 0),
                              byrow = TRUE, ncol = 2)
      path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
      path <- path/1.3
      path[, 1] <- path[, 1] + xStart
      path[, 2] <- path[, 2] + yStart
    }
    else{
      controlPoints <- matrix(c(0, 0,
                                0, 0.2761424,
                                -0.2238576, 0.5,
                                -0.5, 0.6,
                                -0.7761424, 0.5,
                                -1, 0.2761424,
                                -1, 0,
                                -1, -0.2761424,
                                -0.7761424, -0.5,
                                -0.5, -0.5,
                                -0.2, -0.5,
                                -0.05, -0.3,
                                0, 0),
                              byrow = TRUE, ncol = 2)
      path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
      path <- path/1.3
      path[, 1] <- path[, 1] + xStart
      path[, 2] <- path[, 2] + yStart
    }
  }

  if (method == "catmull") {
    controlPoints <- matrix(c(0, 0,
                              0.2761424, 0,
                              0.5, -0.2238576,
                              0.6, -0.5,
                              0.5, -0.7761424,
                              0.2761424, -1,
                              0, -1,
                              -0.2761424, -1,
                              -0.5,  -0.7761424,
                              -0.5, -0.5,
                              -0.5, -0.2,
                              -0.3, -0.05,
                              0, 0),
                            byrow = TRUE, ncol = 2)

    x <- xsplineGrob(x = c(0.25, 0.25, 0.75, 0.75), y = c(0.25, 0.75, 0.75, 0.25), shape = c( 1,  1,  1,  1), open = FALSE)
    path <- xsplinePoints(x)
    path <- lapply(path, as.numeric)
    path <- lapply(path, function(x) (x-min(x))/(max(x) - min(x)))
    path <- data.frame(x = path$x, y = path$y, stringsAsFactors = FALSE)
    path[, 1] <- path[, 1]/2 + xStart
    path[, 2] <- path[, 2]/2 + yStart
  }


  output <- list(
    xEnd = xEnd,
    yEnd = yEnd,
    headingEnd = headingEnd,
    path = path,
    controlPoints = controlPoints
  )

  return(output)

}
