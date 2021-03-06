

test_that("o4 left turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 45
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  headingStart <- 225
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 135
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  headingStart <- 315
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

})




test_that("o4 left turn can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 45
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  headingStart <- 225
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 135
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  headingStart <- 315
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 45
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  headingStart <- 225
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 135
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))

  headingStart <- 315
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart + 270) %% 360))


})




test_that("o4 right turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 135
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  headingStart <- 315
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 45
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  headingStart <- 225
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

})




test_that("o4 right turns can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 135
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  headingStart <- 315
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 45
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  headingStart <- 225
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 135
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  headingStart <- 315
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 45
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

  headingStart <- 225
  out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart)
  expect_true(out$yEnd == yStart)
  expect_true(out$headingEnd == ((headingStart - 270) %% 360))

})



test_that("o4 will report an error on invalid heading", {
  
  xStart <- 0
  yStart <- 0.5
  
  headingStart <- 0
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- -5
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("o4 will report an error on invalid starting location", {

  xStart <- 0
  yStart <- 0
  headingStart <- 45

  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- o4(xStart = xStart, yStart = yStart, headingStart = headingStart))
  
})