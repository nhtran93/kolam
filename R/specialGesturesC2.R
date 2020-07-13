c2 <- function(xStart, yStart, headingStart, method = "bezier"){

  validStarts <- xStart %% 1 == 0.5 & yStart %% 1 == 0 |
    xStart %% 1 == 0 & yStart %% 1 == 0.5
  if(!validStarts) stop("start coordinates are invalid")

  headingStart <- headingStart %% 360
  validHeading <- headingStart %in% c(45 * c(1, 3, 5, 7))
  if(!validHeading) stop("start heading is invalid")

  if (yStart %% 1 == 0) {
    if (headingStart == 45) {
      xStart <- xStart + 0.5
    }
    if (headingStart == 135) {
      xStart <- xStart - 0.5
    }
    if (headingStart == 225) {
      xStart <- xStart - 0.5
    }
    if (headingStart == 315) {
      xStart <- xStart + 0.5
    }
  }

  if (yStart %% 1 == 0.5) {
    if (headingStart == 45) {
      yStart <-yStart + 0.5
    }
    if (headingStart == 135) {
      yStart <-yStart + 0.5
    }
    if (headingStart == 225) {
      yStart <-yStart - 0.5
    }
    if (headingStart == 315) {
      yStart <-yStart - 0.5
    }
  }


  gestureVector <- matrix(c(0, 0, 0, 0), ncol = 2, byrow = TRUE)
  xEnd <- gestureVector[1] + xStart
  yEnd <- gestureVector[2] + yStart
  headingEnd <- headingStart
  headingEnd <- headingEnd %% 360

  # calculate Bezier path
  if (method == "bezier") {
    controlPoints1 <- matrix(c(0, 0,
                               0.15, 0.15),
                             byrow = TRUE, ncol = 2)
    controlPoints2 <- matrix(c(0, 0,
                               -0.15, -0.15),
                             byrow = TRUE, ncol = 2)
    controlPoints3 <- matrix(c(0, 0,
                               0, -0.2),
                             byrow = TRUE, ncol = 2)
    controlPoints4 <- matrix(c(0, 0,
                               0, 0.2),
                             byrow = TRUE, ncol = 2)
    controlPoints5 <- matrix(c(0, 0,
                               -0.2, 0),
                             byrow = TRUE, ncol = 2)
    controlPoints6 <- matrix(c(0, 0,
                               0.2, 0),
                             byrow = TRUE, ncol = 2)
    controlPoints7 <- matrix(c(0, 0,
                               -0.15, 0.15),
                             byrow = TRUE, ncol = 2)
    controlPoints8 <- matrix(c(0, 0,
                               0.15, -0.15),
                             byrow = TRUE, ncol = 2)
    path1 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints1)
    path1[, 1] <- path1[, 1] + xStart
    path1[, 2] <- path1[, 2] + yStart

    path2 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints2)
    path2[, 1] <- path2[, 1] + xStart
    path2[, 2] <- path2[, 2] + yStart

    path3 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints3)
    path3[, 1] <- path3[, 1] + xStart
    path3[, 2] <- path3[, 2] + yStart

    path4 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints4)
    path4[, 1] <- path4[, 1] + xStart
    path4[, 2] <- path4[, 2] + yStart

    path5 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints5)
    path5[, 1] <- path5[, 1] + xStart
    path5[, 2] <- path5[, 2] + yStart

    path6 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints6)
    path6[, 1] <- path6[, 1] + xStart
    path6[, 2] <- path6[, 2] + yStart

    path7 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints7)
    path7[, 1] <- path7[, 1] + xStart
    path7[, 2] <- path7[, 2] + yStart

    path8 <- bezier::bezier(t = seq(0, 1, length = 100), p = controlPoints8)
    path8[, 1] <- path8[, 1] + xStart
    path8[, 2] <- path8[, 2] + yStart

    path = data.frame(list(path1 = path1, path2 = path2,
                           path3 = path3, path4 = path4,
                           path5 = path5, path6 = path6,
                           path7 = path7, path8 = path8))

    path <- as.matrix(cbind(c(path[, 1], path[, 3],
                              path[, 5], path[, 7],
                              path[, 9], path[, 11],
                              path[, 13], path[, 15]),
                            c(path[, 2], path[, 4],
                              path[, 6], path[, 8],
                              path[, 10], path[, 12],
                              path[, 14], path[, 16])))
  }

  controlPoints <-  rbind(controlPoints1, controlPoints2, controlPoints3, controlPoints4, controlPoints5, controlPoints6, controlPoints7, controlPoints8)

  output <- list(
    xEnd = xEnd,
    yEnd = yEnd,
    headingEnd = headingEnd,
    path = path,
    controlPoints = controlPoints
  )

  return(output)

}
