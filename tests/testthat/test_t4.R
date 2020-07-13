
test_that("t4 left-turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 45
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == ((headingStart + 315) %% 360))

  headingStart <- 225
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == ((headingStart + 315) %% 360))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 135
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 315) %% 360))

  headingStart <- 315
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 315) %% 360))

})


test_that("t4 right-turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 135
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == ((headingStart - 315) %% 360))

  headingStart <- 315
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0)
  expect_true(out$headingEnd == ((headingStart - 315) %% 360))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 45
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 315) %% 360))

  headingStart <- 225
  out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 315) %% 360))

})



test_that("t4 will report an error on invalid heading", {
  
  xStart <- 0
  yStart <- 0.5
  
  headingStart <- 0
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- -5
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("t4 will report an error on invalid starting location", {

  xStart <- 0
  yStart <- 0
  headingStart <- 45

  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- t4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  
})