o1 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  gestureVector <- matrix(c(0.5, 0.5), ncol = 2)
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0,   0,
        0.5, 0.5), byrow = TRUE, ncol = 2)
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
  }

  if (method == "catmull") {
    controlPoints <- matrix(
      c(0, 0,
        1, 1), byrow = TRUE, ncol = 2)
    x <- xsplineGrob(x = controlPoints[, 1], y = controlPoints[, 2], shape = 0)
    path <- xsplinePoints(x)
    path <- lapply(path, as.numeric)
    path <- lapply(path, function(x) (x-min(x))/(max(x) - min(x)))
    path <- data.frame(x = path$x, y = path$y, stringsAsFactors = FALSE)
    path[, 1] <- path[, 1]/3 + xStart
    path[, 2] <- path[, 2]/3 + yStart
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

o2 <- function(xStart, yStart, headingStart = 45, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
                (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  gestureVector <- matrix(c(0, 0, 0, 1), byrow = TRUE, ncol = 2) # left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 90 * (-1)^right_turn
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(
      c(0,   0,
        0.5, 0.5,
        0,   1), byrow = TRUE, ncol = 2)
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
  }

  if (method == "catmull") {
    controlPoints <- matrix(
      c(0,   0,
        0.3, 0.5,
        0,   1), byrow = TRUE, ncol = 2)
    x <- xsplineGrob(x = controlPoints[, 1], y = controlPoints[, 2], shape = -1)
    path <- xsplinePoints(x)
    path <- lapply(path, as.numeric)
    path <- lapply(path, function(x) (x-min(x))/(max(x) - min(x)))
    path <- data.frame(x = path$x, y = path$y, stringsAsFactors = FALSE)

    if(right_turn) path <- path[, c(2, 1)] # flip x and y
    path <- rotateOrthogonalPoints(headingStart, path)

    # Rescale curve
    if (xStart %% 1 == 0.5) {
      path[, 2] <- path[, 2]/3
    }
    else{
      path[, 1] <- path[, 1]/3
    }

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


o3 <- function(xStart, yStart, headingStart = 45, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
                 xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  right_turn <- (yStart %% 1 == 0.5 & headingStart %in% c(135, 315)) |
                (xStart %% 1 == 0.5 & headingStart %in% c(45, 225))

  gestureVector <- matrix(c(0, 0, -0.5, 0.5), ncol = 2, byrow = TRUE) # left turn version
  if(right_turn) gestureVector <- gestureVector[, c(2, 1)] # flip x and y
  gestureVector <- rotateOrthogonalPoints(headingStart, gestureVector)
  xEnd <- gestureVector[nrow(gestureVector), 1] + xStart
  yEnd <- gestureVector[nrow(gestureVector), 2] + yStart

  headingEnd <- headingStart + 180 * (-1)^right_turn
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints <- matrix(  # left turn version is default
      c( 0,    0,
         0.6,  0.5,
         0,    1.1,
        -0.5,  0.5
      ), byrow = TRUE, ncol = 2)
    if(right_turn) controlPoints <- controlPoints[, c(2, 1)] # flip x and y
    controlPoints <- rotateOrthogonalPoints(headingStart, controlPoints)
    path <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints)
    path[, 1] <- path[, 1] + xStart
    path[, 2] <- path[, 2] + yStart
  }

  if (method == "catmull") {
    controlPoints <- matrix(
      c(0.25, 0.25,
        0.25, 0.75,
        0.75, 0.75,
        0.75, 0.25), byrow = TRUE, ncol = 2)
    x <- xsplineGrob(x = controlPoints[, 1], y = controlPoints[, 2],
      shape = c( 1, 1, 0, 1), open = FALSE)
    # we should be transforming the control points, not the curve!
    path <- xsplinePoints(x)
    path <- lapply(path, as.numeric)
    path <- lapply(path, function(x) (x-min(x))/(max(x) - min(x)))
    path <- data.frame(x = path$x, y = path$y, stringsAsFactors = FALSE)
    path <- data.matrix(path, rownames.force = NA)
    rotate <- matrix(c(0, -1, 1, 0), byrow = TRUE, ncol = 2)
    path <- path %*% rotate
    rotate <- (sqrt(2)/2)*matrix(c(1, -1, 1, 1), byrow = TRUE, ncol = 2)
    path <- path %*% rotate
    path[, 2] <- path[, 2] - min(path[, 2])

    path <- cbind(rev(path[1:which(path[, 1] == 0), 1]), rev(path[1:which(path[, 1] == 0), 2]))
    mirror <- path[1:nrow(path), 1]/(-1)
    path <- rbind(path, cbind(rev(mirror), rev(path[, 2])))

    path <- path[1:(3/4*nrow(path)), ]

    if(right_turn) path <- path[, c(2, 1)] # flip x and y
    path <- rotateOrthogonalPoints(headingStart, path)

    path[, 1] <- path[, 1]/1.2 + xStart
    path[, 2] <- path[, 2]/1.2 + yStart

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

o4 <- function(xStart, yStart, headingStart = 45, method = "bezier"){

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
         0.2835,  0.81,
         0,       0.81,
        -0.2835,  0.81,
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

  if (method == "catmull") {
    controlPoints <- matrix(
      c(0.25, 0.25,
        0.25, 0.75,
        0.75, 0.75,
        0.25, 0.75), byrow = TRUE, ncol = 2)
    x <- xsplineGrob(x = controlPoints[, 1], y = controlPoints[, 2], shape = c(1, 1, 0, 1), open = FALSE)
    path <- xsplinePoints(x)
    path <- lapply(path, as.numeric)
    path <- lapply(path, function(x) (x-min(x))/(max(x) - min(x)))
    path <- data.frame(x = path$x, y = path$y, stringsAsFactors = FALSE)
    path <- data.matrix(path, rownames.force = NA)
    rotate <- matrix(c(0, -1, 1, 0), byrow = TRUE, ncol = 2)
    path <- path %*% rotate
    rotate <- (sqrt(2)/2)*matrix(c(1, -1, 1, 1), byrow = TRUE, ncol = 2)
    path <- path %*% rotate
    path[, 2] <- path[, 2] - min(path[, 2])

    path <- cbind(rev(path[1:which(path[, 1] == 0), 1]), rev(path[1:which(path[, 1] == 0), 2]))
    mirror <- path[1:nrow(path), 1]/(-1)
    path <- rbind(path, cbind(rev(mirror), rev(path[, 2])))

    path <- path/1.4

    if(right_turn) path <- path[, c(2, 1)] # flip x and y
    path <- rotateOrthogonalPoints(headingStart, path)

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
