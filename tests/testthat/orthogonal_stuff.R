


testthat("sketchLoop combines the paths of several gestures properly", {})

testthat("sketchLoop updates the xEnd, yEnd, and headingEnd properly", {})

testthat("sketchLoop handles random kolams without error", {})

testthat("sketchLoop catches flawed loop input", {})




pattern <- sample(c("o1"), size = 10, replace = TRUE)
plotLoop(pattern)

pattern <- sample(c("o2"), size = 10, replace = TRUE)
plotLoop(pattern, xStart = 0, yStart = 0.5, Netlogo = FALSE, windowMin = -1, windowMax = 10)

pattern <- sample(c("o3"), size = 10, replace = TRUE)
plotLoop(pattern, delay = 0.0001)

pattern <- sample(c("o1", "o2", "o3", "o4"), size = 10, replace = TRUE)
plotLoop(pattern, delay = 0.0001)




pattern <- sample(c("d2r"), size = 1, replace = TRUE)
plotLoop(pattern, xStart = 0.5, yStart = 0.5, headingStart = 0)

pattern <- sample(c("t1"), size = 1, replace = TRUE)
plotLoop(pattern, xStart = 0.5, yStart = 0, headingStart = 45)

pattern <- rep(c("d2r", "d2l"), times = 5)
plotLoop(pattern, xStart = 0.5, yStart = 0.5, headingStart = 0)

plotLoop("t1r", xStart = 0.5, yStart = 0.5, headingStart = 0)





# kolam 1

plotLoop(c("o4", "o3", "o1", "o1", "o4", "o3", "o1",
  "o1", "o4", "o3", "o1", "o1", "o4", "o3", "o1", "o1"), delay = 0.0001, headingStart = 45)


# kolam 2


plotLoop(c("o4", "o2", "o1", "o4", "o3", "o1", "o1",
  "o1", "o4", "o3", "o1", "o1", "o4", "o3", "o2", "o1"), delay = 0.0001, headingStart = 135)


# kolam 3

plotLoop(c("o4", "o1", "o4", "o1", "o4", "o1", "o4", "o1"), delay = 0.0001)



# kolam 4

plotLoop(c(
  "o1", "o3", "o4", "o1", "o1", "o3", "o4", "o1", "o1", "o2",
  "o1", "o3", "o4", "o1", "o1", "o3", "o4", "o1", "o1", "o2",
  "o1", "o3", "o4", "o1", "o1", "o3", "o4", "o1", "o1", "o2",
  "o1", "o3", "o4", "o1", "o1", "o3", "o4", "o1", "o1", "o2"
  ), delay = .005
)
plotLoop(c("o1", "o2", "o1", "o2", "o1", "o2", "o1", "o2"), xStart = -2, add = TRUE, delay = .005, col = "red")


# kolam 5

plotLoop(c(
  "o4", "o1", "o1", "o1", "o1", "o1", "o1", "o1",
  "o4", "o1", "o1", "o1", "o1", "o1", "o1", "o1",
  "o4", "o1", "o1", "o1", "o1", "o1", "o1", "o1",
  "o4", "o1", "o1", "o1", "o1", "o1", "o1", "o1"
), delay = 0.002)

plotLoop(c(
  "o3", "o3", "o3", "o3", "o3", "o1", "o4", "o1",
  "o3", "o3", "o3", "o3", "o3", "o1", "o4", "o1",
  "o3", "o3", "o3", "o3", "o3", "o1", "o4", "o1",
  "o3", "o3", "o3", "o3", "o3", "o1", "o4", "o1"
), add = TRUE, xStart = 3, yStart = -2.5, delay = 0.002, col = "blue"
)

plotLoop(c(
  "o1", "o4", "o3", "o1",
  "o1", "o4", "o3", "o1",
  "o1", "o4", "o3", "o1",
  "o1", "o4", "o3", "o1"
), xStart = 0, yStart = -2.5, add = TRUE, delay = 0.002, col = "dodgerblue")




# temple lamp

pattern <- c(
  "o4",
  "o1", "o1", "o4",
  "o3",
  "o1", "o2", "o1",
  "t2", "d4r", "t1l",
  "o1", "o4", "o1",
  "t1", "d4r", "t2l",
  "o1", "o2", "o1",
  "o3",
  "o4", "o1", "o1"
)

pattern <- c(
  "o4",
  "o1", "o1", "o4",
  "o3",
  "o1", "o2", "o1",
  "t2", "d4r", "t1l",
  "o1", "o4", "o1",
  "t1", "d4r", "t2l",
  "o1", "o2", "o1",
  "o3",
  "o4", "o1", "o1"
)

plotLoop(pattern, delay = 0)

pattern <- c(
  "o4",
  "o1", "o1", "o4",
  "h3l",
  "o1", "o2", "o1",
   "t2", "d4r", "t1l",
    "o1", "o4", "o1",
    "t1", "d4r", "t2l",
     "o1", "o2", "o1",
      "h3r",
      "o4", "o1", "o1"
)

plotLoop(pattern, delay = 0)





