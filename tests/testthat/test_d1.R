
test_that("d1 works at origin", {

  xStart <- 0.5
  yStart <- 0.5

  headingStart <- 0
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 1)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 90
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 1)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 180
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 1)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 270
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 1)
  expect_true(out$headingEnd == headingStart)

})


test_that("d1 shifting works", {

  xStart <- 10.5
  yStart <- 2.5

  headingStart <- 0
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 1)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 90
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 1)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 180
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 1)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 270
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 1)
  expect_true(out$headingEnd == headingStart)

  xStart <- (-10.5)
  yStart <- (-3.5)
  
  headingStart <- 0
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 1)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 90
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 1)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 180
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 1)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == headingStart)

  headingStart <- 270
  out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 1)
  expect_true(out$headingEnd == headingStart)

})


test_that("d1 will report an error on invalid heading", {
  
  xStart <- 0.5
  yStart <- 0.5
  
  headingStart <- 10
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- (-5)
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("o1 will report an error on invalid starting location", {

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45

  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- d1(xStart = xStart, yStart = yStart, headingStart = headingStart))
  
})