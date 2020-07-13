t1Left <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(1, 0.5), ncol = 2) # unique to t1Left
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart + 45 # unique to t1Left
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(  0,    0,
        0.6,    0,
          1,  0.5), byrow = TRUE, ncol = 2) # unique to t1Left
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t1Right <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(1, -0.5), ncol = 2) # unique to t1Right
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart - 45 # unique to t1Right
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(  0,     0,
        0.6,     0,
          1,  -0.5), byrow = TRUE, ncol = 2) # unique to t1Right
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t2Left <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0.5, 1), ncol = 2) # unique to t2Left
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart + 135 # unique to t2Left
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c( # unique to t2Left
      0,   0,
      1,   0,
      1,   0.5,
      0.5, 1
    ), byrow = TRUE, ncol = 2)
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t2Right <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0.5, -1), ncol = 2) # unique to t2Right
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart - 135 # unique to t2Right
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c( # unique to t2Right
      0,    0,
      1,    0,
      1,   -0.5,
      0.5, -1
    ), byrow = TRUE, ncol = 2)
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t3Left <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0, 0.5), ncol = 2) # unique to t3Left
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart + 225 # unique to t3Left
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c( # unique to t3Left
      0,    0,
      0.9,  0.04,
      0.9,  0.62,
      0.82, 0.83,
      0.31, 0.83,
      0,    0.5
    ), byrow = TRUE, ncol = 2)
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t3Right <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0, -0.5), ncol = 2) # unique to t3Right
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart - 225 # unique to t3Right
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c( # unique to t3Right
      0,     0,
      0.9,  -0.04,
      0.9,  -0.62,
      0.82, -0.83,
      0.31, -0.83,
      0,    -0.5
    ), byrow = TRUE, ncol = 2)
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t4Left <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0.5, 0), ncol = 2) # unique to t4Left
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart + 315 # unique to t4Left
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c( # unique to t4Left
      0,    0,
      0.9,  0.04,
      0.9,  0.62,
      0.82, 0.83,
      0.31, 0.83,
      0.19, 0.73,
      0.19, 0.34,
      0.5,  0),
    byrow = TRUE, ncol = 2)
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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

t4Right <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0.5, 0), ncol = 2) # unique to t4Right
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart - 315 # unique to t4Right
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c( # unique to t4Left
      0,     0,
      0.9,  -0.04,
      0.9,  -0.62,
      0.82, -0.83,
      0.31, -0.83,
      0.19, -0.73,
      0.19, -0.34,
      0.5,   0),
    byrow = TRUE, ncol = 2)
    controlPoints <- rotateDiagonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
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
