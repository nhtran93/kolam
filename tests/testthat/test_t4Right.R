
test_that("t4Right works", {

  xStart <- 0.5
  yStart <- 0.5

  headingStart <- 0
  out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == (headingStart - 315) %% 360)

  headingStart <- 90
  out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == (headingStart - 315) %% 360)

  headingStart <- 180
  out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == (headingStart - 315) %% 360)

  headingStart <- 270
  out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == (headingStart - 315) %% 360)
  
})


test_that("t4Right will report an error on invalid heading", {
  
  xStart <- 0.5
  yStart <- 0.5
  
  headingStart <- 10
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- (-5)
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("t4Right will report an error on invalid starting location", {

  xStart <- 0.5
  yStart <- 0
  headingStart <- 45

  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- t4Right(xStart = xStart, yStart = yStart, headingStart = headingStart))
  
})