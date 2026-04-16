#same data set up, but add a protected variable 
#highly (though not perfectly) correlated with x2
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1

# NEW: binary variable p correlated with x2
p = ifelse(x[,2] + rnorm(20, 0, 0.1) > 0, 1, 0)
#compute correlation
cor(p,x)

plot(x, col = y + 3, pch = 19)

library(e1071)
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 1, scale = FALSE)
print(svmfit)
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = -2, to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)

ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

set.seed(13)
x1 = matrix(rnorm(20), 10, 2)
y1 = rep(c(-1, 1), c(5, 5))
x1[y1 == 1,] = x1[y1 == 1,] + 1

# p for test data
p1 = ifelse(x1[,2] + rnorm(10, 0, 0.1) > 0, 1, 0)

plot(x1, col = y1 + 3, pch = 19)

xnew <- rbind(x, x1)
ynew <- append(y, y1)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(xnew)

ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(xnew, col = ynew + 3, pch = 19)

points(x[svmfit$index,], pch = 5, cex = 2)

points(xnew[21:30,], pch = 4, cex = 2)

#overlay shapes around X based on p1
test_idx <- 21:30

# circles for p = 1
points(xnew[test_idx[p1 == 1], ], pch = 1, cex = 2.5, lwd = 2)

# squares for p = 0
points(xnew[test_idx[p1 == 0], ], pch = 0, cex = 2.5, lwd = 2)

#same thing for radial basis 
svmfit = svm(y ~ ., data = dat, kernel = "radial", cost = 100, scale = FALSE)
print(svmfit)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = -2, to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)

ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(xnew)

ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(xnew, col = ynew + 3, pch = 19)

points(x[svmfit$index,], pch = 5, cex = 2)

# test points (X)
points(xnew[21:30,], pch = 4, cex = 2)

# NEW: overlay shapes again
points(xnew[test_idx[p1 == 1], ], pch = 1, cex = 2.5, lwd = 2)
points(xnew[test_idx[p1 == 0], ], pch = 0, cex = 2.5, lwd = 2)