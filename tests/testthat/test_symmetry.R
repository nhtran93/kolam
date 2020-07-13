
# c gestures?

# default orientation is 45 degress for orthogonal control points
# default orientatinon is 0 degres for diagonal control points
# default is left turn for orthogonal gestures

# check symmetry for all
# simplify c2 and c3 to not use bezier

test_that("h3's shape is identical regardless of the handedness", {

  # ### shortlong vs longshort left

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45
  out1 <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- (-0.5)
  headingStart <- 45
  out2 <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(rev(out1$controlPoints[, 1]) - 1, out2$controlPoints[, 1] - 0.5)
  expect_equal(rev(out1$controlPoints[, 2]), out2$controlPoints[, 2] - 0.5)

  ## now using h3l

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45
  out1 <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- (-0.5)
  headingStart <- 45
  out2 <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(rev(out1$controlPoints[, 1]) - 1, out2$controlPoints[, 1] - 0.5)
  expect_equal(rev(out1$controlPoints[, 2]), out2$controlPoints[, 2] - 0.5)

  ### shortlong vs longshort right

  xStart <- 1
  yStart <- 0.5
  headingStart <- 45
  out1 <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 1
  headingStart <- 45
  out2 <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(rev(out1$controlPoints[, 1]) + 1, out2$controlPoints[, 1] + 0.5)
  expect_equal(rev(out1$controlPoints[, 2]), out2$controlPoints[, 2] + 0.5)

  # now using h3r

  xStart <- 1
  yStart <- 0.5
  headingStart <- 45
  out1 <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 1
  headingStart <- 45
  out2 <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(rev(out1$controlPoints[, 1]) + 1, out2$controlPoints[, 1] + 0.5)
  expect_equal(rev(out1$controlPoints[, 2]), out2$controlPoints[, 2] + 0.5)

  ### shortlong left vs right transposed across line of symmetry

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45
  out1 <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0
  headingStart <- 135
  out2 <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1] * (-1))
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2])

  # now h3l vs h3r

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45
  out1 <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0
  headingStart <- 135
  out2 <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1] * (-1))
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2])

  ### longshort left vs right transposed across line of symmetry

  xStart <- 1
  yStart <- 0.5
  headingStart <- 45
  out1 <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- 0.5
  headingStart <- 315
  out2 <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2] * (-1))

  # now h3l vs h3r

  xStart <- 1
  yStart <- 0.5
  headingStart <- 45
  out1 <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- 0.5
  headingStart <- 315
  out2 <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2] * (-1))

})



test_that("t1's shape is identical regardless of handedness", {

  # t1Left (d -> o) vs t1 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t1Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1.5
  yStart <- 1
  headingStart <- 225
  out2 <- t1(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]) + 1)
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]) + 0.5)

  # t1Right (d -> o) vs t1 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t1Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1.5
  yStart <- 0
  headingStart <- 135
  out2 <- t1(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]) + 1)
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]) - 0.5)

  # t1Right vs t1Left translated across axis of symmetry

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t1Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out2 <- t1Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2] * (-1))

})



test_that("t2's shape is identical regardless of handedness", {

  # t2Left (d -> o) vs t2 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t2Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- 1.5
  headingStart <- 315
  out2 <- t2(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]) + 0.5)
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]) + 1)

  # t2Right (d -> o) vs t2 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t2Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- (-0.5)
  headingStart <- 45
  out2 <- t2(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]) + 0.5)
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]) - 1)

  # t2Right vs t2Left translated across axis of symmetry

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t2Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out2 <- t2Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2] * (-1))

})



test_that("t3's shape is identical regardless of handedness", {

  # t3Left (d -> o) vs t3 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t3Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 1
  headingStart <- 45
  out2 <- t3(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]))
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]) + 0.5)

  # t3Right (d -> o) vs t3 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t3Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0
  headingStart <- 315
  out2 <- t3(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]))
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]) - 0.5)

  # t3Right vs t3Left translated across axis of symmetry

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t3Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out2 <- t3Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2] * (-1))

})



test_that("t4's shape is identical regardless of handedness", {

  # t4Left (d -> o) vs t4 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t4Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- 0.5
  headingStart <- 135
  out2 <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]) + 0.5)
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]))

  # t4Right (d -> o) vs t4 (o -> d)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 1
  yStart <- 0.5
  headingStart <- 225
  out2 <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], rev(out2$controlPoints[, 1]) + 0.5)
  expect_equal(out1$controlPoints[, 2], rev(out2$controlPoints[, 2]))

  # t4Right vs t4Left translated across axis of symmetry

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- t4Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out2 <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_equal(out1$controlPoints[, 2], out2$controlPoints[, 2] * (-1))

})



test_that("o2's shape is identical regardless of handedness", {

  xStart <- 0
  yStart <- 0.5
  headingStart <- 45
  out1 <- o2(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0
  yStart <- 1.5
  headingStart <- 315
  out2 <- o2(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_identical(out1$controlPoints[, 1], out2$controlPoints[, 1])
  expect_identical(out1$controlPoints[, 2], (-1) * out2$controlPoints[, 2])

})



test_that("o3's shape is identical regardless of handedness", {

  xStart <- 0
  yStart <- -0.5
  headingStart <- 45
  out1 <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- (-0.5)
  yStart <- 0
  headingStart <- 45
  out2 <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_identical(out1$controlPoints, out2$controlPoints[, 2:1])

})



test_that("o4's shape is identical regardless of handedness", {

  xStart <- 0
  yStart <- 0.5
  headingStart <- 45
  out1 <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0
  yStart <- 0.5
  headingStart <- 135
  out2 <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_identical(out1$controlPoints, out2$controlPoints[nrow(out2$controlPoints):1, ])

})



test_that("d2's shape is identical regardless of handedness", {

  xStart <- (-0.5)
  yStart <- (-0.5)
  headingStart <- 0
  out1 <- d2Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- (0.5)
  yStart <- (0.5)
  headingStart <- 270
  out2 <- d2Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out2$controlPoints[,1] * (-1), out1$controlPoints[,2])
  expect_equal(out2$controlPoints[,2] * (-1), out1$controlPoints[,1])

})



test_that("d3's shape is identical regardless of handedness", {

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- d3Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 1.5
  headingStart <- 0
  out2 <- d3Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out2$controlPoints[, 1], out1$controlPoints[, 1])
  expect_equal(out2$controlPoints[, 2] * (-1), out1$controlPoints[, 2])

})



test_that("d4's shape is identical regardless of handedness", {

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 0
  out1 <- d4Left(xStart = xStart, yStart = yStart, headingStart = headingStart)

  xStart <- 0.5
  yStart <- 0.5
  headingStart <- 90
  out2 <- d4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)

  expect_equal(out2$controlPoints[, 1], rev(out1$controlPoints[, 1]))
  expect_equal(out2$controlPoints[, 2], rev(out1$controlPoints[, 2]))

})

