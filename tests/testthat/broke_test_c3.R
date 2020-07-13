
test_that("c3 works on y-line", {

  xStart <- 0
  yStart <- 0

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 45)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 45)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 135)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 135)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 225)
  expect_true(out$xEnd == xStart  + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 225)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 315)
  expect_true(out$xEnd == xStart  + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 315)

})

test_that("c3 works on x-line", {

  xStart <- 0
  yStart <- 0

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 45)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 45)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 135)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 135)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 225)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0)
  expect_true(out$headingEnd == 225)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 315)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0)
  expect_true(out$headingEnd == 315)

})


test_that("c3 shifting works", {

  xStart <- 10
  yStart <- 10

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 45)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 45)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 135)
  expect_true(out$xEnd == xStart - 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 135)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 225)
  expect_true(out$xEnd == xStart - 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 225)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 315)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 315)

  xStart <- 9
  yStart <- 10

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 45)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 45)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 135)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 135)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 225)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0)
  expect_true(out$headingEnd == 225)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 315)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0)
  expect_true(out$headingEnd == 315)

  xStart <- (-10)
  yStart <- (-10)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 45)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 45)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 135)
  expect_true(out$xEnd == xStart - 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 135)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 225)
  expect_true(out$xEnd == xStart - 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 225)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 315)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 315)

  xStart <- (-9)
  yStart <- (-10)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 45)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 45)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 135)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == 135)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 225)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0)
  expect_true(out$headingEnd == 225)

  out <- c3(xStart = xStart, yStart = yStart, headingStart = 315)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0)
  expect_true(out$headingEnd == 315)

})


test_that("c3 will report an error on invalid heading", {

  xStart <- 0
  yStart <- 0

  headingStart <- 0
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- -5
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("c3 will report an error on invalid starting location", {

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45

  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- c3(xStart = xStart, yStart = yStart, headingStart = headingStart))

})
