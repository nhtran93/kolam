
# tests for h3rshortLong
# tests for h3rlongShort
# tests for h3r

test_that("h3rshortLong right turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 45
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  expect_error(out <-  h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 315
  expect_error(out <-  h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))

  xStart <- 0.5
  yStart <- 0

  headingStart <- 135
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 225
  expect_error(out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("h3rlongShort right turns work", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 315
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <-  h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn right ong-short version for 135° when starting from y axis")
  headingStart <- 225
  expect_error(out <-  h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn right long-short version for 315° when starting from y axis")

  xStart <- 0.5
  yStart <- 0

  headingStart <- 45
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  expect_error(out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 135
  expect_error(out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))




})






test_that("h3rshortLong right turns can change position", {

  xStart <- 10
  yStart <- 10.5

  headingStart <- 45
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  expect_error(out <-  h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 315
  expect_error(out <-  h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))

  xStart <- 10.5
  yStart <- 10

  headingStart <- 135
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 225
  expect_error(out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))


  xStart <- (-10)
  yStart <- (-10.5)

  headingStart <- 45
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  expect_error(out <-  h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 315
  expect_error(out <-  h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 135
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 225
  expect_error(out <- h3rshortLong(xStart = xStart, yStart = yStart, headingStart = headingStart))




})


test_that("h3rlongShort right turns can change position", {

  xStart <- 10
  yStart <- 10.5


  headingStart <- 315
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <-  h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn right ong-short version for 135° when starting from y axis")
  headingStart <- 225
  expect_error(out <-  h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn right long-short version for 315° when starting from y axis")

  xStart <- 10.5
  yStart <- 10

  headingStart <- 45
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  expect_error(out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 135
  expect_error(out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))

  xStart <- (-10)
  yStart <- (-10.5)


  headingStart <- 315
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 135
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 45
  expect_error(out <-  h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn right ong-short version for 135° when starting from y axis")
  headingStart <- 225
  expect_error(out <-  h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  shows_message("There is no Horn right long-short version for 315° when starting from y axis")

  xStart <- (-10.5)
  yStart <- (-10)

  headingStart <- 45
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart + 0.5)
  expect_true(out$yEnd == yStart - 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 225
  out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart)
  expect_true(out$xEnd == xStart - 0.5)
  expect_true(out$yEnd == yStart + 0.5)
  expect_true(out$headingEnd == ((headingStart + 180) %% 360))

  headingStart <- 315
  expect_error(out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))
  headingStart <- 135
  expect_error(out <- h3rlongShort(xStart = xStart, yStart = yStart, headingStart = headingStart))



})








test_that("h3r will report an error on invalid heading", {

  xStart <- 0
  yStart <- 0.5

  headingStart <- 0
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 100
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- -5
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))

  headingStart <- 1.324
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))

})


test_that("h3r will report an error on invalid starting location", {

  xStart <- 0
  yStart <- 0
  headingStart <- 45

  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))
  expect_error(out <- h3r(xStart = xStart, yStart = yStart, headingStart = headingStart))

})





