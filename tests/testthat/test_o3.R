

test_that("o3 left turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 45
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 135
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

})



test_that("o3 left turns can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 45
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 135
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 45
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 135
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))


})




test_that("o3 right turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 135
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  headingStart <- 315
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 45
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  headingStart <- 225
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

})




test_that("o3 right turns can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 135
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  headingStart <- 315
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 45
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  headingStart <- 225
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 135
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  headingStart <- 315
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 45
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))

  headingStart <- 225
  out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart - 180) %% 360))


})



test_that("o3 will report an error on invalid heading", {
  
  xStart <- 0
  yStart <- 0.5
  
  headingStart <- 0
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- -5
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("o3 will report an error on invalid starting location", {

  xStart <- 0
  yStart <- 0
  headingStart <- 45

  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- o3(xStart = xStart, yStart = yStart, headingStart = headingStart))
  
})



