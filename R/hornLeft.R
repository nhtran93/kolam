h3llongShort <- function(xStart, yStart, headingStart = 45, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
    xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  if (xStart %% 1 == 0 & yStart %% 1 == 0.5) {
    validHeading <- headingStart %in% c(45 * c(1, 5))
    if(!validHeading) stop("start heading is invalid")
  }

  if (xStart %% 1 == 0.5 & yStart %% 1 == 0) {
    validHeading <- headingStart %in% c(45 * c(3, 7))
    if(!validHeading) stop("start heading is invalid")
  }

  gestureVector <- matrix(c(0, 0, -0.5, 0.5), ncol = 2, byrow = TRUE)
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[2, 1] + xStart
  yEnd <- gestureVector[2, 2] + yStart

  headingEnd <- headingStart + 180
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {

    controlPointsLong <- matrix(
      c(0,    0,
        0.74, 0.7,
        0,    1.5), byrow = TRUE, ncol = 2)
    controlPointsLong <- rotateOrthogonalPoints(headingStart, controlPointsLong)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPointsLong )

    controlPointsShort <- matrix(
      c( 0,    1.5,
         0.06, 0.83,
        -0.5,  0.5), byrow = TRUE, ncol = 2)
    controlPointsShort <- rotateOrthogonalPoints(headingStart, controlPointsShort)
    path <- rbind(path, bezier::bezier(t = seq(0, 1, length = 100), p = controlPointsShort))

    controlPoints <- rbind(controlPointsLong, controlPointsShort)

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

h3lshortLong <- function(xStart, yStart, headingStart = 45, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
    xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360

  if (xStart %% 1 == 0 & yStart %% 1 == 0.5) {
    validHeading <- headingStart %in% c(45 * c(3, 7))
    if(!validHeading) stop("start heading is invalid")
  }

  if (xStart %% 1 == 0.5 & yStart %% 1 == 0) {
    validHeading <- headingStart %in% c(45 * c(1, 5))
    if(!validHeading) stop("start heading is invalid")
  }

  gestureVector <- matrix(c(0, 0, 0.5, -0.5), ncol = 2, byrow = TRUE)
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[2, 1] + xStart
  yEnd <- gestureVector[2, 2] + yStart

  headingEnd <- headingStart + 180
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {

    controlPointsShort <- matrix(
      c(0,   0,
        0.56, 0.33,
        0.5, 1), byrow = TRUE, ncol = 2)
    controlPointsShort <- rotateOrthogonalPoints(headingStart, controlPointsShort)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPointsShort)

    controlPointsLong <- matrix(
      c(0.5,   1,
        1.24,  0.2,
        0.5, -0.5), byrow = TRUE, ncol = 2)
    controlPointsLong <- rotateOrthogonalPoints(headingStart, controlPointsLong)
    path <- rbind(path, bezier::bezier(t = seq(0, 1, length = 100), p = controlPointsLong))

    controlPoints <- rbind(controlPointsShort, controlPointsLong)

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

h3l <- function(xStart, yStart, headingStart = 45, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
    xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  validHeading <- headingStart %in% c(45 * c(1, 3, 5,  7))
  if(!validHeading) stop("start heading is invalid")

  if (xStart %% 1 == 0.5 & headingStart %in% c(45, 225)) {
    output <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart, method = method)
  }
  if (yStart %% 1 == 0.5 & headingStart %in% c(45, 225)) {
    output <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart, method = method)
  }
  if (xStart %% 1 == 0.5 & headingStart %in% c(135, 315)) {
    output <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart, method = method)
  }
  if (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) {
    output <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart, method = method)
  }

  return(output)
}
