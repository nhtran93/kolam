
# library(grid)
# library(shape)
# library(bezier)


######################################################################## Orthogonal ####################################################################################################################################







# C1
plot.new( )
plot.window( xlim=c(-5,5), ylim=c(-5,5) )
axis(side = 1, at = seq(-5, 5, 1))
axis(side = 2, at = seq(-5, 5, 1))
for (i in -5:5) {
  for (j in -5:5) {
  points(i, j, pch = 19)

  }
}

delta <- (4/3)*tan(pi/8)
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.2761424, -0.2238576, 0.5,-0.5, 0.5, -0.7761424, 0.5,
                                                             -1, 0.2761424, -1, 0,-1, -0.2761424, -0.7761424, -0.5,
                                                             -0.5, -0.5, -0.2238576, -0.5, 0, 0), byrow = TRUE, ncol = 2)), bty = "n", add = TRUE)
c1 <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.2761424, -0.2238576, 0.5,-0.5, 0.5, -0.7761424, 0.5,
                                                             -1, 0.2761424, -1, 0,-1, -0.2761424, -0.7761424, -0.5,
                                                             -0.5, -0.5, -0.2238576, -0.5, 0, 0), byrow = TRUE, ncol = 2))

# O1
plot.new( )
plot.window( xlim=c(-5,5), ylim=c(-5,5) )
axis(side = 1, at = seq(-5, 5, 1))
axis(side = 2, at = seq(-5, 5, 1))
for (i in -5:5) {
  for (j in -5:5) {
    points(i, j, pch = 19)

  }
}
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0,  0, 0.5, 0.5), byrow =TRUE, ncol = 2)), bty = "n")
shape::Arrowhead(x0= 0.5, y0 = 0.5, angle = 45, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

o1 <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0,  0, -0.5, 0.5), byrow =TRUE, ncol = 2))

# O2
plot.new( )
plot.window( xlim=c(-5,5), ylim=c(-5,5) )
axis(side = 1, at = seq(-5, 5, 1))
axis(side = 2, at = seq(-5, 5, 1))
for (i in -5:5) {
  for (j in -5:5) {
    points(i, j, pch = 19, cex = 0.5)

  }
}

# o2r
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, -0.5, 0.5, 0, 1), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0, y0 = 1, angle = 45, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

# o2l
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0.5, -0.5, 0, 1), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0, y0 = 1, angle = 45, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")



# O3
plot.new( )
plot.window( xlim=c(-5,5), ylim=c(-5,5) )
axis(side = 1, at = seq(-5, 5, 1))
axis(side = 2, at = seq(-5, 5, 1))
for (i in -5:5) {
  for (j in -5:5) {
    points(i, j, pch = 19)

  }
}
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, -0.55, -0.55, -1.15, 0, -0.55, 0.55), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= -0.35, y0 = 0.35, angle = 45, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

o3 <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, -0.35, 0.35, 0, 0.7, 0.35, 0.35), byrow =TRUE, ncol = 2))

# O4
plot.new( )
plot.window( xlim=c(-5,5), ylim=c(-5,5) )
axis(side = 1, at = seq(-5, 5, 1))
axis(side = 2, at = seq(-5, 5, 1))
for (i in -5:5) {
  for (j in -5:5) {
    points(i, j, pch = 19)

  }
}
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0,
                                                     -0.405, -0.2835,
                                                     -0.405, -0.405,
                                                     -0.405, -0.6885,
                                                     -0.2835, -0.81,
                                                     0, -0.81,
                                                     0.1215, -0.81,
                                                     0.405, -0.6885,
                                                      0.405, -0.405,
                                                      0.405, -0.2835,
                                                     0, 0), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0, y0 = 0, angle = 45, arr.width = 0.2, arr.length = 0.2, arr.type = "triangle")

o4 <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0.27, 0.189, 0.27, 0.270,0.27, 0.459,
                                                             0.189, 0.54,  0, 0.54,  -0.081, 0.54,-0.27, 0.459,
                                                             - 0.27, 0.27, - 0.270, 0.189, 0, 0), byrow =TRUE, ncol = 2))


######## Orthogonal Function



######################################################################## Diagonal ####################################################################################################################################
# D1
plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0,  0, 0, 0.5), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0., y0 = 0.5, angle = 90, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d1 <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0,  0, 0, 0.5), byrow =TRUE, ncol = 2))

# D2 R

plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, 0.5, 0.5), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0.5, y0 = 0.5, angle = 360, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d2r <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, 0.5, 0.5), byrow =TRUE, ncol = 2))

# D2 L
plot(1, type="n", xlab="", ylab="", xlim=c(-1, .1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, -0.5, 0.5), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= -0.5, y0 = 0.5, angle = 180, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d2l <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, -0.5, 0.5), byrow =TRUE, ncol = 2))

rotate_90 <- matrix(c(0, -1, 1, 0), byrow = TRUE, ncol = 2)
rotate_180 <- matrix(c(-1, 0, 0, -1), byrow = TRUE, ncol = 2)
rotate_270 <- matrix(c(0, 1, -1, 0), byrow = TRUE, ncol = 2)
test1 <- t(rotate_90 %*% t(test))
test2 <- t(rotate_180 %*% t(test))
test3 <- t(rotate_270 %*% t(test))

lines(bezier::bezier(t = seq(0, 1, length=100), p = test1))
lines(bezier::bezier(t = seq(0, 1, length=100), p = test2))
lines(bezier::bezier(t = seq(0, 1, length=100), p = test3))

# D3 R
plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, 0.5, 0.5, 0.5, 0), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0.5, y0 = 0, angle = 270, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d3r <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, 0.5, 0.5, 0.5, 0), byrow =TRUE, ncol = 2))

# D3 L
plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, -0.5, 0.5, -0.5, 0), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= -0.15, y0 = 0, angle = 270, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d3l <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.5, -0.5, 0.5, -0.5, 0), byrow =TRUE, ncol = 2))

# D4 R
plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0.75, 0, 0.75, 0.75, 0, 0.75, 0, 0), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0, y0 = 0, angle = 180, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d4r <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0.75, 0, 0.75, 0.75, 0, 0.75, 0, 0), byrow =TRUE, ncol = 2))

# D4 L

plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1), bty = "n")
grid(col = "black")
lines(bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.75, -0.75, 0.75, -0.75, 0, 0, 0), byrow =TRUE, ncol = 2)))
shape::Arrowhead(x0= 0, y0 = 0, angle = 360, arr.width = 0.1, arr.length = 0.1, arr.type = "triangle")

d4l <- bezier::bezier(t = seq(0, 1, length=100), p = matrix(c(0, 0, 0, 0.75, -0.75, 0.75, -0.75, 0, 0, 0), byrow =TRUE, ncol = 2))

######## Diagonal Function



################################################################## Transitional: Orthogonal -> Diagonal ########################################################################################################################################################



################################################################## Transitional: Diagonal -> Orthogonal ########################################################################################################################################################


################################################################## Stylistic Variations ########################################################################################################################################################





################################################################ Plotting Kolams ################################################################################################################################################################################################





plot(1, type="n", xlab="", ylab="", xlim=c(-3, 3), ylim=c(-3, 3), bty = "n")
grid(col = "black")
test <- orthogonal("o3", plot = FALSE, orientation = 270)
lines(test$path)
test <- orthogonal("o1", plot = FALSE,  xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree-45, shifting = FALSE)
lines(test$path)
test <- orthogonal("o1", plot = FALSE,  xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree-45, shifting = FALSE)
lines(test$path)
test <- orthogonal("o4", plot = FALSE, xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation =  90)
lines(test$path)

test <- orthogonal("o1", plot = FALSE)
lines(test$path)
test <- orthogonal("o1", plot = FALSE,  xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree+45)
lines(test$path)
test <- orthogonal("o4", plot = FALSE, xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree+45)
lines(test$path)

test <- orthogonal("o3", plot = FALSE, xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree-45+90+90)
lines(test$path)
test <- orthogonal("o4", plot = FALSE, xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree+45)
lines(test$path)
test <- orthogonal("o4", plot = FALSE,  xStart = test$path[length(test$path[, 1]), 1], yStart = test$path[length(test$path[, 2]), 2], orientation = test$degree+45)
lines(test$path)











                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                