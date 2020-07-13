op <- function(xStart, yStart, headingStart = 45, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
    xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
    (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  xEnd <- xStart
  yEnd <- yStart

  headingEnd <- headingStart + 270 * (-1)^right_turn
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(  # left turn version is default
      c( 0,       0,
         0.405,   0.2835,
         0.405,   0.405,
         0.405,   0.6885,
         -0.7,  1,
         0,       1,
         0.7,  1,
         -0.405,   0.6885,
         -0.405,   0.405,
         -0.405,   0.2835,
         0,       0
      ), byrow = TRUE, ncol = 2)
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
  }

  if (method == "xspline") {
    controlPoints <- matrix(
      c(  0,     0,
          0.25,  0.5,
          0,     0.85,
          -0.25,  0.5,
          0,     0
      ), byrow = TRUE, ncol = 2)
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
    weights <- c(0, -1, -1, -1, 0)
    pathXY <- xspline(controlPoints[, 1], controlPoints[, 2], shape = weights, draw = FALSE)
    path <- cbind(pathXY$x, pathXY$y)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
  }

  output <- list(
    xEnd = xEnd,
    yEnd = yEnd,
    headingEnd = headingEnd,
    path = path
  )

  return(output)

}
