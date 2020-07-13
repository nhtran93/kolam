d1 <- function(xStart, yStart, headingStart, method = "bezier") {

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(1, 0), ncol = 2)
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(c(0, 0, 1, 0), byrow = TRUE, ncol = 2)
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

d2Left <- function(xStart, yStart, headingStart, method = "bezier") {

  right_turn <- FALSE

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0, 0, 1, 1), byrow = TRUE, ncol = 2) # left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 90 * (-1) ^ right_turn
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0, 0,
        1, 0,
        1, 1), byrow = TRUE, ncol = 2)
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
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

d2Right <- function(xStart, yStart, headingStart, method = "bezier") {

  right_turn <- TRUE

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
                 xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0, 0, 1, 1), byrow = TRUE, ncol = 2) # left turn version
  if (right_turn) gestureVector[, 2] <- gestureVector[, 2] * (-1) # flip on x axis
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 90 * (-1) ^ right_turn
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0, 0,
        1, 0,
        1, 1), byrow = TRUE, ncol = 2)
    if (right_turn) controlPoints[, 2] <- controlPoints[, 2] * (-1) # flip x and y
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

d3Right <- function(xStart, yStart, headingStart, method = "bezier") {

  right_turn <- TRUE

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
    xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  # gestureVector always starts at 0, 0, and indicates one point more, the final location after the gesture
  gestureVector <- matrix(c(0, 0, 0, 1), byrow =TRUE, ncol = 2) # define left turn version first
  if (right_turn) gestureVector[, 2] <- gestureVector[, 2] * (-1) # flip on x axis to get the right turn version
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 180 * (-1) ^ right_turn # unique to each transformation, here d3
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0, 0,
        1, 0,
        1, 1,
        0, 1), byrow =TRUE, ncol = 2) # define left turn version first
    if (right_turn) controlPoints[, 2] <- controlPoints[, 2] * (-1) # flip on x axis to get the right turn version
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

d3Left <- function(xStart, yStart, headingStart, method = "bezier") {

  right_turn <- FALSE

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
    xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  # gestureVector always starts at 0, 0, and indicates one point more, the final location after the gesture
  gestureVector <- matrix(c(0, 0, 0, 1), byrow =TRUE, ncol = 2) # define left turn version first
  if (right_turn) gestureVector[, 2] <- gestureVector[, 2] * (-1) # flip on x axis to get the right turn version
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 180 * (-1) ^ right_turn # unique to each transformation, here d3
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0, 0,
        1, 0,
        1, 1,
        0, 1), byrow =TRUE, ncol = 2) # define left turn version first
    if (right_turn) controlPoints[, 2] <- controlPoints[, 2] * (-1) # flip on x axis to get the right turn version
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

d4Right <- function(xStart, yStart, headingStart, method = "bezier") {

  right_turn <- TRUE

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
    xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  # gestureVector always starts at 0, 0, and indicates one point more, the final location after the gesture
  gestureVector <- matrix(c(0, 0, 0, 0), byrow =TRUE, ncol = 2) # define left turn version first
  if (right_turn) gestureVector[, 2] <- gestureVector[, 2] * (-1) # flip on x axis to get the right turn version
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 270 * (-1) ^ right_turn # unique to each transformation, here d3
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0,    0,
        1.25, 0,
        1.25, 1.25,
        0,    1.25,
        0,    0), byrow =TRUE, ncol = 2) # define left turn version first
    if (right_turn) controlPoints[, 2] <- controlPoints[, 2] * (-1) # flip on x axis to get the right turn version
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

d4Left <- function(xStart, yStart, headingStart, method = "bezier") {

  right_turn <- FALSE

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0.5 |
    xStart %% 1 == 0.5 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% (90 * c(0, 1, 2, 3))
  if(!validHeading) stop("start heading is invalid")

  # gestureVector always starts at 0, 0, and indicates one point more, the final location after the gesture
  gestureVector <- matrix(c(0, 0, 0, 0), byrow =TRUE, ncol = 2) # define left turn version first
  if (right_turn) gestureVector[, 2] <- gestureVector[, 2] * (-1) # flip on x axis to get the right turn version
  gestureVector <- rotateDiagonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 270 * (-1) ^ right_turn # unique to each transformation, here d3
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0,    0,
        1.25, 0,
        1.25, 1.25,
        0,    1.25,
        0,    0), byrow =TRUE, ncol = 2)
    if (right_turn) controlPoints[, 2] <- controlPoints[, 2] * (-1) # flip on x axis to get the right turn version
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
