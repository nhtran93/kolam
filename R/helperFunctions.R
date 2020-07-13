rotateOrthogonalPoints <- function(headingStart, pathPoints) {

  headingStart <- headingStart %% 360

  if (headingStart == 45){
    rotate0 <- matrix(c(1, 0, 0, 1), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate0 %*% t(pathPoints))
  }

  if (headingStart == 135){
    rotate90 <- matrix(c(0, -1, 1, 0), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate90 %*% t(pathPoints))
  }

  if (headingStart == 225){
    rotate180 <- matrix(c(-1, 0, 0, -1), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate180 %*% t(pathPoints))
  }

  if (headingStart == 315){
    rotate270 <- matrix(c(0, 1, -1, 0), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate270 %*% t(pathPoints))
  }

  return(pathPoints)
}

rotateHornPoints <- function(headingStart, pathPoints) {

  headingStart <- headingStart %% 360

  if (headingStart == 45){
    rotate0 <- matrix(c(1, 0, 0, 1), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate0 %*% t(pathPoints))
  }

  if (headingStart == 315){
    rotate90 <- matrix(c(0, -1, 1, 0), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate90 %*% t(pathPoints))
  }

  if (headingStart == 225){
    rotate180 <- matrix(c(-1, 0, 0, -1), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate180 %*% t(pathPoints))
  }

  if (headingStart == 135){
    rotate270 <- matrix(c(0, 1, -1, 0), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate270 %*% t(pathPoints))
  }

  return(pathPoints)
}

rotateDiagonalPoints <- function(headingStart, pathPoints) {

  headingStart <- headingStart %% 360

  if (headingStart == 0){
    rotate0 <- matrix(c(1, 0, 0, 1), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate0 %*% t(pathPoints))
  }

  if (headingStart == 90){
    rotate90 <- matrix(c(0, -1, 1, 0), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate90 %*% t(pathPoints))
  }

  if (headingStart == 180){
    rotate180 <- matrix(c(-1, 0, 0, -1), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate180 %*% t(pathPoints))
  }

  if (headingStart == 270){
    rotate270 <- matrix(c(0, 1, -1, 0), byrow = TRUE, ncol = 2)
    pathPoints <- t(rotate270 %*% t(pathPoints))
  }

  return(pathPoints)
}
