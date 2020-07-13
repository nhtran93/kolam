
test_that("h3lShortLong left turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 135
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <-  h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 225
  expect_error(out <-  h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 45
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  expect_error(out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 315
  expect_error(out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))



})


test_that("h3lLongShort left turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 45
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  expect_error(out <-  h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn Left ong-short version for 135° when starting from y axis")
  headingStart <- 315
  expect_error(out <-  h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn Left long-short version for 315° when starting from y axis")

  xStart <- 0.5
  yStart <- 0

  headingStart <- 135
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 225
  expect_error(out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))




})






test_that("h3lshortLong left turns can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 135
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 45
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 135
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 45
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3lshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))



})


test_that("h3llongShort left turns can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 45
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 135
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 45
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 135
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3llongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

})


test_that("h3l will report an error on invalid heading", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 0
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- -5
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("h3l will report an error on invalid starting location", {

  xStart <- 0
  yStart <- 0
  headingStart <- 45

  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- h3l(xStart = xStart, yStart = yStart, headingStart = headingStart))

})





