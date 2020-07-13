t1 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
                (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  gestureVector <- matrix(c(0, 0, 0.5, 1), byrow = TRUE, ncol = 2) # unique to t1, left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 45 * (-1)^right_turn # unique to t1
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
       c(  0,    0,
         0.5,  0.4,
         0.5,    1), byrow = TRUE, ncol = 2) # unique to t1, left turn version
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
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

t2 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
                (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  gestureVector <- matrix(c(0, 0, -0.5, 1), byrow = TRUE, ncol = 2) # unique to t2, left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 135 * (-1)^right_turn # unique to t2
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
    c(0,   0,
      0.5, 0.5,
      0.5, 1,
     -0.5, 1), byrow = TRUE, ncol = 2) # unique to t2, left turn version
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
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

t3 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
                (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  gestureVector <- matrix(c(0, 0, -0.5, 0), byrow = TRUE, ncol = 2) # unique to t3, left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 225 * (-1)^right_turn # unique to t3
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
  controlPoints <- matrix(
    c( 0,    0,
       0.33, 0.31,
       0.33, 0.82,
       0.12, 0.9,
      -0.46, 0.9,
      -0.5,  0), byrow = TRUE, ncol = 2) # unique to t3, left turn version
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
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

t4 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
                (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  gestureVector <- matrix(
    c(0, 0, 0.5, 0), byrow = TRUE, ncol = 2) # unique to t4, left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 315 * (-1)^right_turn # unique to t4
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0,    0,
        0.31, 0.34,
        0.31, 0.73,
        0.19, 0.83,
       -0.32, 0.83,
       -0.4,  0.62,
       -0.4,  0.04,
        0.5,  0), byrow = TRUE, ncol = 2) # unique to t4, left turn version
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
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
